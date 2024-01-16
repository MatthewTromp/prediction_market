use std::ops::SubAssign;


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
// paid by each party to a disjoint set contract
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Contribution(u32);
const TOTAL_CONTRIB: u32 = 10_000_000;

// Represents an amount of money
#[derive(Clone, Copy, Debug)]
struct Money(u64);
const ONE_DOLLAR: u64 = (TOTAL_CONTRIB as u64) * (ONE_SHARE as u64);

impl Contribution {
    fn from_raw(yes_proportion: u32) -> Self {
        assert!(yes_proportion <= TOTAL_CONTRIB);
        Contribution(yes_proportion)
    }

    pub fn from_float(yes_proportion: f64) -> Self {
        Self::from_raw((yes_proportion*(TOTAL_CONTRIB as f64)).round() as u32)
    }

    pub fn get_total_price(&self, volume: Volume) -> Money {
        Money((self.0 as u64) * (volume.0 as u64))
    }

    pub fn share_contribs<const N: usize>(contribs: [Contribution; N]) -> Option<[Contribution; N]> {
        // Normalize the contributions
        let sum: u32 = contribs.iter().map(|c| c.0).sum();
        if sum >= TOTAL_CONTRIB {
            let mut a = contribs.map(|c| ((c.0 as u64*TOTAL_CONTRIB as u64)/sum as u64) as u32);
            let missing = TOTAL_CONTRIB - a.iter().sum::<u32>();
            for i in 0..missing {
                a[i as usize] += 1;
            }
            Some(a.map(|v| Contribution(v)))
        } else {
            None
        }
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

// An "order" represents an incoming order
// An "outstanding" is an order in the order book
// Details are common between oders and outstandings

#[derive(Debug, Clone, PartialEq)]
struct Order {
    details: Details,
}

#[derive(Debug, Clone, PartialEq)]
struct Details {
    creq: CompletionRequirement,
    customer: CustomerID,
    volume: Volume,
    prob: Contribution,    
}

#[derive(Debug, Clone, PartialEq)]
struct Identifier {
    id: u64,
    prob: Contribution,
}

#[derive(Debug, Clone, PartialEq)]
struct OutstandingOrder {
    orderid: u64,
    details: Details,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct PH1 {
    customer: CustomerID,
    contribution: Contribution,
}

#[derive(Debug, PartialEq, Eq)]
struct Transaction<const N: usize> {
    contributions: [PH1; N],
    volume: Volume,
}

#[derive(Debug)]
enum TransactionOutcome<const N: usize> {
    Failure,
    Success(Transaction<N>, Vec<usize>),
}

fn make_transaction<const N: usize> (orders: [& mut Details; N]) -> TransactionOutcome<N> {
    use TransactionOutcome::*;
    match Contribution::share_contribs::<N>(orders.iter().map(|o| o.prob).collect::<Vec<Contribution>>().try_into().unwrap()) {
        None => {
            Failure
        }
        Some(contribs) => {
            let volume = orders.iter()
                .map(|o| o.volume)
                .min()
                .unwrap();
            assert!(volume.0 > 0);
            let mut to_remove = vec![];
            for i in 0..N {
                orders[i].volume -= volume;
                if orders[i].volume == Volume(0) {
                    to_remove.push(i);
                }
            }
            Success(Transaction {
                contributions: orders.into_iter().zip(contribs.iter()).map(|(o, &contribution)| PH1 {
                    customer: o.customer,
                    contribution,
                }).collect::<Vec<PH1>>().try_into().unwrap(),
                volume
            }, to_remove)
        }
    }
}


#[derive(Debug)]
struct MatchingEngine<const N: usize> {
    book: [Vec<OutstandingOrder>; N],
    next_id: u64,
}

// So, how do we get this to work properly?
// We need to be able to efficiently
// - Add an order to the order book
// - Get the current best order on the book
// - Remove the best order from the book
// - Delete an order by id
// - Get the entire order book

// What if the ID of an order contains its price and timestamp? Then we just
// have our entries sorted by price and timestamp. As long as we never have
// orders which are the same in both of those attributes of those two. In
// fact, the timestamp could just be an incrementing counter that gets
// assigned when the entry is added to the book.

// Alternatively, NOTHING
// Alternatively cry. This is how it's gonna work

// Having duplicates for yes/no everything sucks
// A binary option is just a special case of the more general disjoint set
// option.
// Is there some way of doing this?

// TODO: Replace arrays with priority queues?
impl<const N: usize> MatchingEngine<N> {
    fn new() -> Self {
        MatchingEngine {
            book: [(); N].map(|()| Vec::new()),
            next_id: 1,
        }
    }

    fn handle_partialable_order<const I: usize>(&mut self, mut order: Order) -> (Option<Identifier>, Vec<Transaction<N>>)
    {
        assert!(I < N);

        let mut identifier = None;

        let mut transactions = vec![];

        let mut done = false;

        while !done {
            let (before, after) = self.book.split_at_mut(I);
            let k = before.iter_mut()
                .map(|v: &mut Vec<OutstandingOrder>| v.last_mut().map(|o| &mut o.details))
                .chain(Some(Some(&mut order.details)))
                .chain(after.iter_mut().skip(1)
                       .map(|v: &mut Vec<OutstandingOrder>| v.last_mut().map(|o| &mut o.details)))
                .collect::<Option<Vec<&mut Details>>>();
            match k {
                None => {
                    identifier = Some(self.insert_order::<I>(order.details));
                    break;
                }
                Some(orders) => {
                    match make_transaction(orders.try_into().unwrap()) {
                        TransactionOutcome::Failure => {
                            identifier = Some(self.insert_order::<I>(order.details));
                            break;
                        }
                        TransactionOutcome::Success(a, b) => {
                            transactions.push(a);
                            for i in b {
                                if i == I {
                                    done = true;
                                } else {
                                    let removed = self.book[i].pop();
                                    assert_eq!(removed.map(|o| o.details.volume), Some(Volume(0)));
                                }
                            }
                        }
                    }
                }
            }
        }

        (identifier, transactions)
    }
    
    // TODO: If an incoming order has been filled at all, the order currently on the end of the list
    // can't possibly be better than it, because if it were, it would have matched with the orders
    // that the incoming order matched with. So we can just stick the oncoming order on the end of
    // the list
    fn insert_order<const I: usize>(&mut self, order: Details) -> Identifier {
        assert!(I < N);
        
        let id = self.next_id;
        self.next_id += 1;
        
        let identifier = Identifier {
            id,
            prob: order.prob,
        };

        let insertion_index = self.book[I].iter()
            .rposition(|other| other.details.prob < order.prob)
            .map(|i| i + 1)
            .unwrap_or(0);
        
        let outstanding_order = OutstandingOrder {
            orderid: id,
            details: order,
        };
        
        self.book[I].insert(insertion_index, outstanding_order);

        identifier
    }

    pub fn cancel_order<const I: usize>(&mut self, orderid: Identifier) -> Option<OutstandingOrder> {
        assert!(I < N);
        let orderid = orderid.id;
        self.book[I].iter()
            .position(|o| o.orderid == orderid)
            .map(|i| self.book[I].remove(i))
    }
        
    pub fn get_order_book(&self) -> [Vec<OutstandingOrder>; N] {
        self.book.clone()
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    fn v(value: u32) -> Volume {
        Volume(value)
    }
    fn p(f: f64) -> Contribution {
        Contribution::from_float(f)
    }

    fn check_order_response<const N: usize>(resp: Vec<Transaction<N>>, expct: Vec<([(CustomerID, Contribution); N], Volume)>) {
        assert_eq!(resp.len(), expct.len());
        for (Transaction { contributions, volume }, (expct_contribs, expct_volume)) in  resp.iter().zip(expct.iter()) {
            assert_eq!(volume, expct_volume);
            for i in 0..N {
                assert_eq!(contributions[i].customer, expct_contribs[i].0);
                let e = expct_contribs[i].1.0;
                let a = contributions[i].contribution.0;
                assert!(a == e || a == (e-1) || a == (e+1), "{a}, {e}");
            }
        }
    }
    
    #[test]
    fn test1() {
        let mut m: MatchingEngine<2> = MatchingEngine::new();
        let partial = CompletionRequirement::PartialOk;
        let c1 = CustomerID(1);
        let c2 = CustomerID(2);
        let c3 = CustomerID(3);
        let c4 = CustomerID(4);
        let c5 = CustomerID(5);
        let c6 = CustomerID(6);
        let c7 = CustomerID(7);
        let c8 = CustomerID(8);

        let mut o0 = Order {
            details: Details {
                creq: partial,
                customer: c1,
                volume: v(10),
                prob: p(0.6),
            }
        };

        assert_eq!(m.handle_partialable_order::<1>(o0.clone()).1, vec![]);

        assert_eq!(m.get_order_book().map(|v| v.into_iter().map(|o| o.details).collect::<Vec<Details>>()), [vec![], vec![o0.clone().details]]);

        let mut o1 = Order {
            details: Details {
                creq: partial,
                customer: c2,
                volume: v(20),
                prob: p(0.3),
            }
        };

        assert_eq!(m.handle_partialable_order::<1>(o1.clone()).1, vec![]);

        assert_eq!(m.get_order_book().map(|v| v.into_iter().map(|o| o.details).collect::<Vec<Details>>()), [vec![], vec![o1.clone().details, o0.clone().details]]);
        let o2 = Order {
            details: Details {
                creq: partial,
                customer: c5,
                volume: v(30),
                prob: p(0.3),
            }
        };
        
        assert_eq!(m.handle_partialable_order::<0>(o2.clone()).1, vec![]);

        assert_eq!(m.get_order_book().map(|v| v.into_iter().map(|o| o.details).collect::<Vec<Details>>()), [vec![o2.clone().details], vec![o1.clone().details, o0.clone().details]]);

        let o3 = Order {
            details: Details {
                creq: partial,
                customer: c3,
                volume: v(3),
                prob: p(0.6),
            }
        };

        check_order_response::<2>(m.handle_partialable_order::<0>(o3.clone()).1, vec![([(c3, p(0.5)), (c1, p(0.5))], v(3))]);

        o0.details.volume -= v(3);

        assert_eq!(m.get_order_book().map(|v| v.into_iter().map(|o| o.details).collect::<Vec<Details>>()), [vec![o2.clone().details], vec![o1.clone().details, o0.clone().details]]);

        let o4 = Order {
            details: Details {
                creq: partial,
                customer: c4,
                volume: v(20),
                prob: p(0.8)
            }
        };
 
        check_order_response::<2>(m.handle_partialable_order::<0>(o4.clone()).1, vec![([(c4, p(0.8/(0.8+0.6))), (c1, p(0.6/(0.8+0.6)))], v(7)),
                                                                                      ([(c4, p(0.8/(0.8+0.3))), (c2, p(0.3/(0.8+0.3)))], v(13))]);

        o1.details.volume -= v(13);

        assert_eq!(m.get_order_book().map(|v| v.into_iter().map(|o| o.details).collect::<Vec<Details>>()), [vec![o2.clone().details], vec![o1.clone().details]]);

        let mut o5 = Order {
            details: Details {
                creq: partial,
                customer: c7,
                volume: v(20),
                prob: p(0.8)
            }
        };
        
        check_order_response::<2>(m.handle_partialable_order::<0>(o5.clone()).1, vec![([(c7, p(0.8/(0.8 + 0.3))), (c2, p(0.3/(0.8+0.3)))], v(7))]);

        o5.details.volume -= v(7);

        assert_eq!(m.get_order_book().map(|v| v.into_iter().map(|o| o.details).collect::<Vec<Details>>()), [vec![o2.clone().details, o5.clone().details], vec![]]);

        let mut o6 = Order {
            details: Details {
                creq: partial,
                customer: c6,
                volume: v(100),
                prob: p(1.0),
            }
        };

        check_order_response::<2>(m.handle_partialable_order::<1>(o6.clone()).1, vec![([(c7, p(0.8/(1.8))), (c6, p(1.0/(1.8)))], v(13)),
                                                                                      ([(c5, p(0.3/1.3)), (c6, p(1.0/1.3))], v(30))]);

        o6.details.volume -= v(43);

        assert_eq!(m.get_order_book().map(|v| v.into_iter().map(|o| o.details).collect::<Vec<Details>>()), [vec![], vec![o6.clone().details]]);
    }

    #[test]
    fn test_cancellation() {
        let partial = CompletionRequirement::PartialOk;
        
        let mut m = MatchingEngine::new();

        let c1 = CustomerID(1);
        let c2 = CustomerID(2);
        let c3 = CustomerID(3);

        let o1 = Order {
            details: Details {
                creq: partial,
                customer: c1,
                volume: v(20),
                prob: p(0.3),
            }
        };

        let o2 = Order {
            details: Details {
                creq: partial,
                customer: c2,
                volume: v(20),
                prob: p(0.3),
            }
        };

        let o3 = Order {
            details: Details {
                creq: partial,
                customer: c3,
                volume: v(20),
                prob: p(0.4),
            }
        };

        let id1 = m.handle_partialable_order::<1>(o1.clone()).0.unwrap();
        let id2 = m.handle_partialable_order::<0>(o2.clone()).0.unwrap();
        let id3 = m.handle_partialable_order::<0>(o3.clone()).0.unwrap();

        assert_eq!(m.cancel_order::<0>(id2.clone()).map(|o| o.details), Some(o2.clone().details));

        assert_eq!(m.get_order_book().map(|v| v.into_iter().map(|o| o.details).collect::<Vec<Details>>()), [vec![o3.clone().details], vec![o1.clone().details]]);

        assert_eq!(m.cancel_order::<0>(id2).map(|o| o.details), None);

        assert_eq!(m.get_order_book().map(|v| v.into_iter().map(|o| o.details).collect::<Vec<Details>>()), [vec![o3.clone().details], vec![o1.clone().details]]);
        
        assert_eq!(m.cancel_order::<1>(id1).map(|o| o.details), Some(o1.clone().details));

        assert_eq!(m.get_order_book().map(|v| v.into_iter().map(|o| o.details).collect::<Vec<Details>>()), [vec![o3.details], vec![]]);
    }
}
