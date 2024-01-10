use std::{cmp::Ordering, ops::SubAssign};


#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum CompletionRequirement {
    FillOnly,
    PartialOk,
}

// Represents a number of (fractions of) shares, where a share is something
// that resolves to one dollar
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Volume(u32);

impl SubAssign for Volume {
    fn sub_assign(&mut self, rhs: Self) {
        self.0 -= rhs.0
    }
}

const ONE_SHARE: u32 = 10_000;

// Represents a "probability price": the share of the price of the instrument
// paid by each party (yes and no)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Probability(u32);
const MAX_YES: u32 = 10_000_000;

// Represents an amount of money
#[derive(Clone, Copy, Debug)]
struct Money(u64);
const ONE_DOLLAR: u64 = (MAX_YES as u64) * (ONE_SHARE as u64);

impl Probability {
    fn from_yes(yes_proportion: u32) -> Self {
        assert!(yes_proportion <= MAX_YES);
        Probability(yes_proportion)
    }

    fn from_no(no_proportion: u32) -> Self {
        assert!(no_proportion <= MAX_YES);
        Probability(MAX_YES - no_proportion)
    }

    pub fn from_yes_float(yes_proportion: f64) -> Self {
        Self::from_yes((yes_proportion*(MAX_YES as f64)).round() as u32)
    }

    pub fn from_no_float(no_proportion: f64) -> Self {
        Self::from_yes_float(1.0-no_proportion)
    }

    pub fn get_total_price_yes(&self, volume: Volume) -> Money {
        Money((self.0 as u64) * (volume.0 as u64))
    }

    pub fn average(p1: Self, p2: Self) -> Self {
        Probability((p1.0 + p2.0)/2)
    }
}

impl Money {
    fn from_raw(r: u64) -> Self {
        Money(r)
    }

    fn from_dollars(dollars: u64) -> Self {
        Money(dollars*ONE_DOLLAR)
    }

    fn from_cents(cents: u64) -> Self {
        Money(cents*ONE_DOLLAR/100)
    }

    fn from_basis_points(bps: u64) -> Self {
        Money(bps*ONE_DOLLAR/1000)
    }
}


#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct CustomerID(usize);

#[derive(Debug, Clone, PartialEq)]
struct OutstandingYes {
    orderid: u64,
    creq: CompletionRequirement,
    customer: CustomerID,
    volume: Volume,
    prob: Probability,
}

#[derive(Debug, Clone, PartialEq)]
struct OutstandingNo {
    orderid: u64,
    creq: CompletionRequirement,
    customer: CustomerID,
    volume: Volume,
    prob: Probability,
}

#[derive(Debug)]
struct MatchingEngine {
    yeses: Vec<OutstandingYes>,
    nos: Vec<OutstandingNo>,
}

#[derive(Debug, PartialEq, Eq)]
struct Transaction {
    yes_party: CustomerID,
    no_party: CustomerID,
    volume: Volume,
    prob: Probability,
}

#[derive(Debug)]
enum TransactionOutcome {
    FailedTransaction(OutstandingYes, OutstandingNo),
    YesRemaining(Transaction, OutstandingYes),
    NoRemaining(Transaction, OutstandingNo),
    NoneRemaining(Transaction),
}

fn make_transaction(mut yes: OutstandingYes, mut no: OutstandingNo) -> TransactionOutcome {
    use TransactionOutcome::*;
    if yes.prob < no.prob {
        FailedTransaction(yes, no)
    } else {
        let prob = Probability::average(yes.prob, no.prob);
        match yes.volume.cmp(&no.volume) {
            Ordering::Greater => {
                yes.volume -= no.volume;
                YesRemaining(Transaction {
                    yes_party: yes.customer,
                    no_party: no.customer,
                    volume: no.volume,
                    prob,
                }, yes)
            }
            Ordering::Less => {
                no.volume -= yes.volume;
                NoRemaining(Transaction {
                    yes_party: yes.customer,
                    no_party: no.customer,
                    volume: yes.volume,
                    prob,
                }, no)
            }
            Ordering::Equal => {
                NoneRemaining(Transaction {
                    yes_party: yes.customer,
                    no_party: no.customer,
                    volume: no.volume,
                    prob
                })
            }
        }
    }
}

// TODO: Replace arrays with priority queues?
// TODO: order cancellation
impl MatchingEngine {
    fn new() -> Self {
        MatchingEngine {
            yeses: vec![],
            nos: vec![],
        }
    }
    
    fn handle_partialable_yes(&mut self, order: OutstandingYes) -> Vec<Transaction> {
        let mut transactions: Vec<Transaction> = vec![];

        let mut order = Some(order);
        
        loop {
            match (order.take(), self.nos.pop()) {
                (None, _) => {
                    panic!();
                }
                
                (Some(or), None) => {
                    // No more nos available. Put the rest of this yes order into the yeses pool, and exit
                    self.insert_yes(or);
                    break;
                }
                
                (Some(or), Some(sell)) => {
                    use TransactionOutcome::*;
                    match make_transaction(or, sell) {
                        FailedTransaction(b, s) => {
                            self.insert_yes(b);
                            self.nos.push(s);
                            break;
                        }
                        YesRemaining(t, b) => {
                            order = Some(b);
                            transactions.push(t);
                        }
                        NoRemaining(t, s) => {
                            transactions.push(t);
                            self.nos.push(s);
                            break;
                        }
                        NoneRemaining(t) => {
                            transactions.push(t);
                        }
                    }
                }
            }
        }

        transactions
    }

    // TODO: If an incoming order has been filled at all, the order currently on the end of the list
    // can't possibly be better than it, because if it were, it would have matched with the orders
    // that the incoming order matched with. So we can just stick the oncoming order on the end of
    // the list
    fn insert_yes(&mut self, order: OutstandingYes) {
        let insertion_index = self.yeses.iter().rev().enumerate()
                .filter(|(_, other)| other.prob < order.prob)
                .map(|(i, _)| i+1)
                .next()
            .unwrap_or(0);
        self.yeses.insert(insertion_index, order);
    }


    fn handle_partialable_no(&mut self, order: OutstandingNo) -> Vec<Transaction> {
        let mut transactions: Vec<Transaction> = vec![];
        
        let mut order = Some(order);
        
        loop {
            match (order.take(), self.yeses.pop()) {
                (None, _) => {
                    panic!();
                }
                
                (Some(or), None) => {
                    self.insert_no(or);
                    break;
                }
                
                (Some(or), Some(buy)) => {
                    use TransactionOutcome::*;
                    match make_transaction(buy, or) {
                        FailedTransaction(b, s) => {
                            self.insert_no(s);
                            self.yeses.push(b);
                            break;
                        }
                        YesRemaining(t, b) => {
                            transactions.push(t);
                            self.yeses.push(b);
                            break;
                        }
                        NoRemaining(t, s) => {
                            order = Some(s);
                            transactions.push(t);
                        }
                        NoneRemaining(t) => {
                            transactions.push(t);
                        }
                    }
                }
            }
        }
        
        transactions
    }

    fn insert_no(&mut self, order: OutstandingNo) {
        let insertion_index = self.nos.iter().rev().enumerate()
                .filter(|(_, other)| other.prob > order.prob)
                .map(|(i, _)| i+1)
                .next()
            .unwrap_or(0);
        self.nos.insert(insertion_index, order);
    }

    pub fn cancel_yes_order(&mut self, orderid: u64) -> Option<OutstandingYes> {
        self.yeses.iter()
            .position(|o| o.orderid == orderid)
            .map(|i| self.yeses.remove(i))
    }

    pub fn cancel_no_order(&mut self, orderid: u64) -> Option<OutstandingNo> {
        self.nos.iter()
            .position(|o| o.orderid == orderid)
            .map(|i| self.nos.remove(i))
    }
    
    pub fn get_order_book(&self) -> (Vec<OutstandingYes>, Vec<OutstandingNo>) {
        (self.yeses.clone(), self.nos.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn v(value: u32) -> Volume {
        Volume(value)
    }
    fn p(f: f64) -> Probability {
        Probability::from_yes_float(f)
    }
    
    #[test]
    fn test1() {

        
        let mut m = MatchingEngine::new();
        let partial = CompletionRequirement::PartialOk;
        let c1 = CustomerID(1);
        let c2 = CustomerID(2);
        let c3 = CustomerID(3);
        let c4 = CustomerID(4);
        let c5 = CustomerID(5);
        let c6 = CustomerID(6);
        let c7 = CustomerID(7);
        let c8 = CustomerID(8);

        let mut o0 = OutstandingYes {
            orderid: 0,
            creq: partial,
            customer: c1,
            volume: v(10),
            prob: p(0.6),
        };

        assert_eq!(m.handle_partialable_yes(o0.clone()), vec![]);

        assert_eq!(m.get_order_book(), (vec![o0.clone()], vec![]));

        let mut o1 = OutstandingYes {
            orderid: 1,
            creq: partial,
            customer: c2,
            volume: v(20),
            prob: p(0.3),
        };

        assert_eq!(m.handle_partialable_yes(o1.clone()), vec![]);

        assert_eq!(m.get_order_book(), (vec![o1.clone(), o0.clone()], vec![]));
        let o2 = OutstandingNo {
            orderid: 2,
            creq: partial,
            customer: c5,
            volume: v(30),
            prob: p(0.7),
        };
        
        assert_eq!(m.handle_partialable_no(o2.clone()), vec![]);

        assert_eq!(m.get_order_book(), (vec![o1.clone(), o0.clone()], vec![o2.clone()]));

        let o3 = OutstandingNo {
            orderid: 3,
            creq: partial,
            customer: c3,
            volume: v(3),
            prob: p(0.4),
        };

        assert_eq!(m.handle_partialable_no(o3.clone()), vec![Transaction {
            yes_party: c1,
            no_party: c3,
            volume: v(3),
            prob: p(0.5),
        }]);

        o0.volume -= v(3);

        assert_eq!(m.get_order_book(), (vec![o1.clone(), o0.clone()], vec![o2.clone()]));

        let o4 = OutstandingNo {
            orderid: 4,
            creq: partial,
            customer: c4,
            volume: v(20),
            prob: p(0.2)
        };
 
        assert_eq!(m.handle_partialable_no(o4.clone()), vec![Transaction {
            yes_party: c1,
            no_party: c4,
            volume: v(7),
            prob: p(0.4),
        }, Transaction {
            yes_party: c2,
            no_party: c4,
            volume: v(13),
            prob: p(0.25)
        }]);

        o1.volume -= v(13);

        assert_eq!(m.get_order_book(), (vec![o1.clone()], vec![o2.clone()]));

        let mut o5 = OutstandingNo {
            orderid: 5,
            creq: partial,
            customer: c7,
            volume: v(20),
            prob: p(0.2)
        };
        
        assert_eq!(m.handle_partialable_no(o5.clone()), vec![Transaction {
            yes_party: c2,
            no_party: c7,
            volume: v(7),
            prob: p(0.25)
        }]);

        o5.volume -= v(7);

        assert_eq!(m.get_order_book(), (vec![], vec![o2.clone(), o5.clone()]));

        let mut o6 = OutstandingYes {
            orderid: 6,
            creq: partial,
            customer: c6,
            volume: v(100),
            prob: p(1.0),
        };

        assert_eq!(m.handle_partialable_yes(o6.clone()), vec![Transaction {
            yes_party: c6,
            no_party: c7,
            volume: v(13),
            prob: p(0.6),
        }, Transaction {
            yes_party: c6,
            no_party: c5,
            volume: v(30),
            prob: p(0.85),
        }, ]);

        o6.volume -= v(43);

        assert_eq!(m.get_order_book(), (vec![o6.clone()], vec![]));
    }

    #[test]
    fn test_cancellation() {
        let partial = CompletionRequirement::PartialOk;
        
        let mut m = MatchingEngine::new();

        let c1 = CustomerID(1);
        let c2 = CustomerID(2);
        let c3 = CustomerID(3);

        let o1 = OutstandingYes {
            orderid: 1,
            creq: partial,
            customer: c1,
            volume: v(20),
            prob: p(0.3),
        };

        let o2 = OutstandingNo {
            orderid: 2,
            creq: partial,
            customer: c2,
            volume: v(20),
            prob: p(0.7),
        };

        let o3 = OutstandingNo {
            orderid: 3,
            creq: partial,
            customer: c3,
            volume: v(20),
            prob: p(0.6),
        };

        m.handle_partialable_yes(o1.clone());
        m.handle_partialable_no(o2.clone());
        m.handle_partialable_no(o3.clone());

        assert_eq!(m.cancel_no_order(2), Some(o2.clone()));

        assert_eq!(m.get_order_book(), (vec![o1.clone()], vec![o3.clone()]));

        assert_eq!(m.cancel_no_order(2), None);

        assert_eq!(m.get_order_book(), (vec![o1.clone()], vec![o3.clone()]));
        
        assert_eq!(m.cancel_yes_order(1), Some(o1.clone()));

        assert_eq!(m.get_order_book(), (vec![], vec![o3.clone()]));
    }
}
