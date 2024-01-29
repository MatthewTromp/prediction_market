
use std::cmp::Ordering;
use std::collections::BTreeMap;
// An "order" represents an incoming order
// An "outstanding" is an order in the order book
// Details are common between oders and outstandings

use super::{Contribution, Volume, CompletionRequirement, CustomerID};
#[derive(Debug, Clone, Copy, PartialEq)]
struct Order {
    details: Details,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
struct Details {
    creq: CompletionRequirement,
    customer: CustomerID,
    volume: Volume,
    contribution: Contribution,    
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
struct Identifier {
    id: u64,
    prob: Contribution,
}

impl Ord for Identifier {
    fn cmp(&self, other: &Self) -> Ordering {
        // Sort by price, then by orderid
        match self.prob.partial_cmp(&other.prob) {
            None => panic!("NANS IN THE SYSTEM AAAAAAA"),
            Some(Ordering::Equal) => self.id.cmp(&other.id).reverse(),
            Some(c) => c,
        }
    }
}

impl PartialOrd for Identifier {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(&other))
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
struct OutstandingOrder {
    orderid: u64,
    details: Details,
}

impl OutstandingOrder {
    fn identifier(&self) -> Identifier {
        Identifier {
            id: self.orderid,
            prob: self.details.contribution,
        }
    }
}

impl Ord for OutstandingOrder {
    fn cmp(&self, other: &Self) -> Ordering {
        self.identifier().cmp(&other.identifier())
    }
}

impl PartialOrd for OutstandingOrder {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(&other))
    }
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
    match Contribution::share_contribs::<N>(orders.each_ref().map(|o| (**o).contribution)) {
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
            let mut c_i = contribs.into_iter();
            Success(Transaction {
                contributions: orders.map(|o| PH1 {
                    customer: o.customer,
                    contribution: c_i.next().unwrap(),
                    }),
                // contributions: orders.into_iter().zip(contribs.iter()).map(|(o, &contribution)| PH1 {
                //     customer: o.customer,
                //     contribution,
                // }).collect::<Vec<PH1>>().try_into().unwrap(),
                volume
            }, to_remove)
        }
    }
}

#[derive(Debug)]
struct MatchingEngine<const N: usize> {
    book: [BTreeMap<Identifier, OutstandingOrder>; N],
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

// Features
// - Partial, bookable orders DONE
// - Partial, unbookable orders DONE (needs testing)
// - Full, unbookable orders (FOK). Requires a different order handling function
// - Full, bookable orders (AON). Requires rethinking the entire system, but
// automatically supports FOK
// - Disjunction orders (I will contribute 0.3 to a contract where I have
// outcomes 2 and 3 and other parties have 1, 4 and 5)
// - Bookable disjunction orders (WOOF)
// - Bidder logic system, like OneChronos (very long-term goal)
// - Automatic cancellation after a timeout?

// Thoghts about matching
// You can do FIFO or pro-rata, it looks like
// - FIFO: First order has to get completely filled before a later order at
//   the same price
// - Pro-rata: if there are multiple orders at the same price, the volume is
//   distributed among them in proportion to their volumes
//  With pro-rata, there's incentive to make a lot of volume available,
// possibly even more than you actually want filled, so that you capture
// more of the volume of orders that come in
//  Issue: if we have very fine-grained prices, both of these fall apart, I
// think. Because to capture more of the volume, you can just make a new
// order that's more favorable than the next best by an arbitrarily small
// amount

// Pricing issues
//  If you do midpoint pricing, doesn't that give kinda bad incentives to
// people wanting to make a purchase? Like, if you know the state of the
// order book, instead of making one order with your actual limit price, you
// would make a bunch of smaller orders to eat up each counterparty offering
// at their limit price
//  Actually what you'll probably get in practice is a very large number of
// orders for small volumes with increasingly high contributions
//  So maybe the way to do pricing is to have the person making the incoming
// order pay the limit of their counterparties?
// Options:
//  - Midpoint
//    Pros:
//    - Fair: doesn't matter what order orders come in. The price is the same
//    - No incentive to try and be the last to put an order in
//    Cons:
//    - Makers and takers both have incentives to lie about their limits, takers
//      more so.
//    - To get the best price, takers will probably put in a bunch of small
//      orders with higher and higher contributions to eat up each order in the
//      book one at a time at a price that maximizes their profit
//  - Favor takers
//    Pros:
//    - Takers have no incentive to lie or spam
//    Cons:
//    - Makers have an incentive to lie (but that's mitigated by them losing out
//      on trades)
//    - Makers are disadvantaged, so the market will have lower liquidity

// Supporting AON
// - Transaction system? Do some operations, and undo them if need be?
//    That massively complicates a lot of things but also makes a lot of
//    things very simple?? Implementation seems kinda nightmarish, and also
//    slow, but it would make AON simple.
//  When filling an order, instead of modifying orders, we take orders off
// and put them in some data structure. Then at the end, when there's no more
// resolution to be done, we actually perform the operations
//  Choosing orders:
// - It's possible for skipping an order to actually result in a better
//   outcome. e.g. we have limit orders (contrib, volume, AON)
//    [(0.1, 10, n), (0.2, 20, y), (0.25, 10, n)]
//   And an order comes in for 20 volume. The best price would be to take the
//   AON order in the middle, but if you first take the top order, you won't
//   have enough volume for it and you'll get 0.175 average contribution.
//   I think there's an O(n^2) DP fix to this, but, like, gross. But this
//   seems like it might be an inherent issue
// - What if we somehow batch orders slightly? So if two orders come in
//   at the same time that fill an AON we can use them together to fill it?
// - Bird says AON isn't important but FOK is

// Two stage transactions
//  Stage 1: Take counterorders until we've fully satisified the order
//  Stage 2: Actually execute the transactions

//  Also, the way we're doing all of this is kinda silly
//  Like, we can just have everyone paying into a big pot. We don't need a
// separate "transaction" concept for every new participant in the
// transaction.

impl<const N: usize> MatchingEngine<N> {
    fn new() -> Self {
        MatchingEngine {
            book: [(); N].map(|()| BTreeMap::new()),
            next_id: 1,
        }
    }

    pub fn handle_unbookable_partial<const I: usize>(&mut self, order: Order) -> Vec<Transaction<N>> {
        let (id, transes) = self.handle_partialable_order::<I>(order);
        if let Some(id) = id {
            assert!(self.cancel_order::<I>(id).is_some());
        }
        transes
    }

    pub fn handle_fok(&mut self, side: usize, order: Order) -> Option<Vec<Transaction<N>>> {
        assert!(side < N);
        let mut iters = self.book.each_ref().map(|b| b.iter());
        let i = iters.each_mut().map(|i| i.next());
        todo!();
    }

    pub fn handle_partialable_order<const I: usize>(&mut self, order: Order) -> (Option<Identifier>, Vec<Transaction<N>>)
    {
        assert!(I < N);

        let mut identifier = Some(self.insert_order::<I>(order.details));

        let mut transactions = vec![];

        loop {
            let entries = self.book.each_mut().map(|k|
                   k.last_entry());
            if entries.iter().any(|v| v.is_none()) { break };
            let orders = entries.map(|e| e.unwrap().into_mut());
            let orders = orders.map(|o| &mut o.details);
            match make_transaction(orders) {
                TransactionOutcome::Failure => {
                    break;
                }
                TransactionOutcome::Success(a, b) => {
                    transactions.push(a);
                    for i in b {
                        let removed = self.book[i].pop_last();
                        if identifier.as_ref().is_some_and(|i| removed.as_ref().is_some_and(|(i2, _)| i2.id == i.id)) {
                            identifier = None;
                        }
                        assert_eq!(removed.map(|(_, o)| o.details.volume), Some(Volume(0)));
                    }
                }
            }
        }

        (identifier, transactions)
    }
    
    fn insert_order<const I: usize>(&mut self, order: Details) -> Identifier {
        assert!(I < N);
        
        let id = self.next_id;
        self.next_id += 1;
        
        let identifier = Identifier {
            id,
            prob: order.contribution,
        };

        let outstanding_order = OutstandingOrder {
            orderid: id,
            details: order,
        };
        
        self.book[I].insert(identifier, outstanding_order);

        identifier
    }

    pub fn cancel_order<const I: usize>(&mut self, orderid: Identifier) -> Option<OutstandingOrder> {
        assert!(I < N);
        self.book[I].remove(&orderid)
    }
        
    pub fn get_order_book(&self) -> [Vec<OutstandingOrder>; N] {
        self.book.each_ref().map(|b| b.iter().map(|(_, o)| o.clone()).collect())
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    fn v(value: u32) -> Volume {
        Volume(value)
    }
    fn c(f: f64) -> Contribution {
        Contribution::from_float(f)
    }

    fn check_order_response<const N: usize>(resp: Vec<Transaction<N>>, expct: Vec<([(CustomerID, Contribution); N], Volume)>) {
        assert_eq!(resp.len(), expct.len());
        for (Transaction { contributions, volume }, (expct_contribs, expct_volume)) in resp.iter().zip(expct.iter()) {
            assert_eq!(volume, expct_volume);
            for i in 0..N {
                assert_eq!(contributions[i].customer, expct_contribs[i].0);
                let e = expct_contribs[i].1.0;
                let a = contributions[i].contribution.0;
                assert!(a == e || a == (e-1) || a == (e+1), "{a}, {e}");
            }
        }
    }

    fn p(customer: CustomerID, volume: Volume, contribution: Contribution) -> Order {
        Order {
            details: Details {
                creq: CompletionRequirement::PartialOk,
                customer,
                volume,
                contribution,
            }
        }
    }

    fn f(customer: CustomerID, volume: Volume, contribution: Contribution) -> Order {
        Order {
            details: Details {
                creq: CompletionRequirement::FillOnly,
                customer,
                volume,
                contribution,
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
                contribution: c(0.6),
            }
        };

        assert_eq!(m.handle_partialable_order::<1>(o0.clone()).1, vec![]);

        assert_eq!(m.get_order_book().map(|v| v.into_iter().map(|o| o.details).collect::<Vec<Details>>()), [vec![], vec![o0.clone().details]]);

        let mut o1 = Order {
            details: Details {
                creq: partial,
                customer: c2,
                volume: v(20),
                contribution: c(0.3),
            }
        };

        assert_eq!(m.handle_partialable_order::<1>(o1.clone()).1, vec![]);

        assert_eq!(m.get_order_book().map(|v| v.into_iter().map(|o| o.details).collect::<Vec<Details>>()), [vec![], vec![o1.clone().details, o0.clone().details]]);
        let o2 = Order {
            details: Details {
                creq: partial,
                customer: c5,
                volume: v(30),
                contribution: c(0.3),
            }
        };
        
        assert_eq!(m.handle_partialable_order::<0>(o2.clone()).1, vec![]);

        assert_eq!(m.get_order_book().map(|v| v.into_iter().map(|o| o.details).collect::<Vec<Details>>()), [vec![o2.clone().details], vec![o1.clone().details, o0.clone().details]]);

        let o3 = Order {
            details: Details {
                creq: partial,
                customer: c3,
                volume: v(3),
                contribution: c(0.6),
            }
        };

        check_order_response::<2>(m.handle_partialable_order::<0>(o3.clone()).1, vec![([(c3, c(0.5)), (c1, c(0.5))], v(3))]);

        o0.details.volume -= v(3);

        assert_eq!(m.get_order_book().map(|v| v.into_iter().map(|o| o.details).collect::<Vec<Details>>()), [vec![o2.clone().details], vec![o1.clone().details, o0.clone().details]]);

        let o4 = Order {
            details: Details {
                creq: partial,
                customer: c4,
                volume: v(20),
                contribution: c(0.8)
            }
        };
 
        check_order_response::<2>(m.handle_partialable_order::<0>(o4.clone()).1, vec![([(c4, c(0.8/(0.8+0.6))), (c1, c(0.6/(0.8+0.6)))], v(7)),
                                                                                      ([(c4, c(0.8/(0.8+0.3))), (c2, c(0.3/(0.8+0.3)))], v(13))]);

        o1.details.volume -= v(13);

        assert_eq!(m.get_order_book().map(|v| v.into_iter().map(|o| o.details).collect::<Vec<Details>>()), [vec![o2.clone().details], vec![o1.clone().details]]);

        let mut o5 = Order {
            details: Details {
                creq: partial,
                customer: c7,
                volume: v(20),
                contribution: c(0.8)
            }
        };
        
        check_order_response::<2>(m.handle_partialable_order::<0>(o5.clone()).1, vec![([(c7, c(0.8/(0.8 + 0.3))), (c2, c(0.3/(0.8+0.3)))], v(7))]);
        

        o5.details.volume -= v(7);

        assert_eq!(m.get_order_book().map(|v| v.into_iter().map(|o| o.details).collect::<Vec<Details>>()), [vec![o2.clone().details, o5.clone().details], vec![]]);

        let mut o6 = Order {
            details: Details {
                creq: partial,
                customer: c6,
                volume: v(100),
                contribution: c(1.0),
            }
        };

        check_order_response::<2>(m.handle_partialable_order::<1>(o6.clone()).1, vec![([(c7, c(0.8/(1.8))), (c6, c(1.0/(1.8)))], v(13)),
                                                                                      ([(c5, c(0.3/1.3)), (c6, c(1.0/1.3))], v(30))]);

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
                contribution: c(0.3),
            }
        };

        let o2 = Order {
            details: Details {
                creq: partial,
                customer: c2,
                volume: v(20),
                contribution: c(0.3),
            }
        };

        let o3 = Order {
            details: Details {
                creq: partial,
                customer: c3,
                volume: v(20),
                contribution: c(0.4),
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

    fn i(id: usize) -> CustomerID {
        CustomerID(id)
    }

    #[test]
    fn test_fok() {
        let mut m = MatchingEngine::<2>::new();

        let o1 = p(i(0), v(10), c(0.4));

        check_order_response::<2>(m.handle_partialable_order::<0>(o1).1, vec![]);

        let o2 = f(i(1), v(20), c(0.6));

        assert_eq!(m.handle_fok(1, o2), None);

        let o3 = p(i(1), v(20), c(0.6));

        check_order_response::<2>(m.handle_partialable_order::<1>(o3).1, vec![([(i(0), c(0.4)), (i(1), c(0.6))], v(10))]);
    }
}


#[cfg(test)]
mod ordertest {
    use super::*;
    #[test]
    fn test1() {
        let c1 = CustomerID(0);
        let c2 = CustomerID(1);
        let c3 = CustomerID(2);
        let o1 = OutstandingOrder {
            orderid: 1,
            details: Details {
                creq: CompletionRequirement::PartialOk,
                customer: c1,
                volume: Volume(10),
                contribution: Contribution::from_float(0.4),
            }
        };
        let o2 = OutstandingOrder {
            orderid: 2,
            details: Details {
                creq: CompletionRequirement::PartialOk,
                customer: c1,
                volume: Volume(10),
                contribution: Contribution::from_float(0.4),
            }
        };
        let o3 = OutstandingOrder {
            orderid: 1,
            details: Details {
                creq: CompletionRequirement::PartialOk,
                customer: c1,
                volume: Volume(10),
                contribution: Contribution::from_float(0.7),
            }
        };
        let o4 = OutstandingOrder {
            orderid: 1,
            details: Details {
                creq: CompletionRequirement::FillOnly,
                customer: c2,
                volume: Volume(20),
                contribution: Contribution::from_float(0.4),
            }
        };

        assert_eq!(o1.cmp(&o2), Ordering::Greater);
        assert_eq!(o1.cmp(&o3), Ordering::Less);
        assert_eq!(o1.cmp(&o4), Ordering::Equal);
        assert!(o1 != o4);
    }
}
