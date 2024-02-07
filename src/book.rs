
use std::collections::hash_map::Entry;
use std::fmt::{self, Debug};
use std::error::Error;
use std::{cmp::Ordering, collections::BinaryHeap};
use std::collections::{BTreeMap, HashMap};
// An "order" represents an incoming order
// An "outstanding" is an order in the order book
// Details are common between oders and outstandings

use crate::units::{Money, Side, Contribution, Volume, CompletionRequirement, CustomerID};


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Order {
    pub details: Details,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Details {
    pub creq: CompletionRequirement,
    pub customer: CustomerID,
    pub volume: Volume,
    pub contribution: Contribution,    
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Identifier {
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
pub struct PH1 {
    pub customer: CustomerID,
    pub contribution: Contribution,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Transaction<const N: usize> {
    pub contributions: [PH1; N],
    pub volume: Volume,
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
            assert!(volume > Volume::ZERO);
            let mut to_remove = vec![];
            for i in 0..N {
                orders[i].volume -= volume;
                if orders[i].volume == Volume::ZERO {
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
// more of the volume of orders that come in. But that's still some non-
// incentive compatible nonsense!
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

//  What about doing pro-rata plus favor takers? That might offset some of the
// reduced liquidity from favoring takers

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

// Can we handle combinatorial orders in the book?
//  i.e. I want to buy sides 1 + 2 + 5 of this 7-side instrument
//  

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

    pub fn handle_fok(&mut self, sides: &[Side], _order: Order) -> Option<Vec<Transaction<N>>> {
        assert!(sides.iter().all(|s| s.0 < N));
        let mut iters = self.book.each_ref().map(|b| b.iter());
        let _i = iters.each_mut().map(|i| i.next());
        
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
                        assert_eq!(removed.map(|(_, o)| o.details.volume), Some(Volume::ZERO));
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


#[derive(Debug, PartialEq, Eq, Hash)]
pub struct CounterpartyPayment {
    customer: CustomerID,
    volume: Volume,
    contribution: Contribution,
    side: Side
}

#[derive(Debug, PartialEq, Eq)]
pub struct TransactionDyn {
    pub taker_volume: Volume,
    pub taker_pays: Money,
    pub counterparties: Vec<CounterpartyPayment>
}

#[derive(Debug, Clone)]
enum TransactionCreationError {
    BadVolume,
    BadMoney,
}

impl fmt::Display for TransactionCreationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TransactionCreationError::BadVolume => write!(f, "Volumes don't add up"),
            TransactionCreationError::BadMoney => write!(f, "Money doesn't add up"),
        }
    }
}

impl Error for TransactionCreationError {}

impl TransactionDyn {
    fn checked_make(taker_volume: Volume, taker_pays: Money, counterparties: Vec<CounterpartyPayment>) -> Result<Self, TransactionCreationError> {
        // Some sanity checks
        let total_money_in = taker_pays + counterparties.iter().map(|o| o.volume*o.contribution).sum();
        if total_money_in != taker_volume * Contribution::ONE {
            return Err(TransactionCreationError::BadMoney);
        }
        let mut sidevols = HashMap::new();
        for CounterpartyPayment { customer: _, volume: v, contribution: _, side: s } in counterparties.iter() {
            match sidevols.entry(s) {
                Entry::Occupied(o) => {*(o.into_mut()) += *v;},
                Entry::Vacant(va) => {va.insert(*v);},
            }
        }
        if !sidevols.into_iter().all(|(_, v)| v == taker_volume) {
            return Err(TransactionCreationError::BadVolume);
        }
        Ok(Self {
            taker_volume,
            taker_pays,
            counterparties,
        })
    }
}


#[derive(Debug)]
pub struct MatchingEngineDyn {
    book: Vec<BTreeMap<Identifier, OutstandingOrder>>,
    next_id: u64,
}

// TODO: Can do more efficient market operations using a similar strategy to
// the fast side cancellation data structure in positions.rs. Keep a
// priority queue and a volume offset.
// Performance analysis:
//  n is number of sides
//  m is the number of orders we "work through"
//  k is the max number of orders in a side
// Current system:
//  Need to do O(m) iterations where we
//   Check all n sides for the minimum volume (O(n))
//   Subtract that volume from all n top orders (O(n))
//  Also we zero up to O(m) orders
//   and removing an order from the book is O(logk)
//  So our overall running time is O(m*(n + logk))
// Priority queue:
//  Need to do O(m) iterations where we
//   Take the top element from the queue (O(logn))
//   Set our running volume count (O(1))
//   Remove that element from the book (O(logk)) (Possible to reduce this by waiting until the end and, like, truncating the tree?)
//   Get the next element from the book (O(logk)) (Again, maybe can reduce with a pointer to the rightmost block or something?)
//   Add the running volume to this element and put it in the queue (O(logn))
//  So our overall running time is O(m*(logn + logk))

// Wait is there something even smarter?
//  What if we get all the orders up to our full volume and construct
// a contrib/volume curve? Like with auctions
// That would require O(m*logm)? We have to get all m orders (up to
// our volume limit), calculate their contribution derivatives, put
// them in a big list, sort the list (again could techinically be
// O(m*logn) with merging but whatever), and proceed until we exceed
// the contribution need
//  But that's no better in terms of limiting runtime than the piority
// queue approach, right? So there's no real benefit. Would it have
// better performance in practice, because sorting a list is cheaper
// than a priority queue? Maybe...
//  But, with the priority queue, we exit early. With the list
// approach, we take all the orders, even if we're going to fall down
// at the first attempted match


impl MatchingEngineDyn {
    fn new(num_sides: usize) -> Self {
        Self {
            book: vec![BTreeMap::new(); num_sides],
            next_id: 1,
        }
    }

    pub fn num_sides(&self) -> usize {
        self.book.len()
    }

    pub fn handle_unbookable_partial(&mut self, sides: &[Side], order: Order) -> TransactionDyn {
        #[derive(PartialEq, Eq)]
        struct HeapEntries {
            order: OutstandingOrder,
            side: Side,
            cumulative_volume: Volume,
        }
        
        impl Ord for HeapEntries {
            fn cmp(&self, other: &Self) -> Ordering {
                self.cumulative_volume.cmp(&other.cumulative_volume).reverse()
            }
        }

        impl PartialOrd for HeapEntries {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                Some(self.cmp(other))
            }
        }

        assert!(sides.iter().all(|s| s.0 < self.num_sides()));
        assert!(sides.len() > 0);
        // Priority queue approach
        // Get orders on the top of the books
        let mut other_sides = self.book.iter_mut()
            .enumerate()
            .filter(|(o, _)| !sides.contains(&Side(*o))) // TODO: performance
            .map(|(s, b)| (Side(s), b))
            .collect::<HashMap<_,_>>();

        if other_sides.iter().any(|(_, o)| o.is_empty()) {
            return TransactionDyn { taker_volume: Volume::ZERO, taker_pays: Money::ZERO, counterparties: vec![] };
        }
        
        let init = other_sides.iter_mut()
            .map(|(s, i)| {
                let x = i.pop_last().unwrap().1;
                HeapEntries {
                    order: x,
                    side: *s,
                    cumulative_volume:  x.details.volume,
                }
            })
            .collect::<Vec<_>>();
        
        let mut total_contrib = init.iter()
            .map(|HeapEntries { order, side: _, cumulative_volume: _}| order.details.contribution)
            .sum();

        let mut other_resolutions: Vec<CounterpartyPayment> = vec![];
        
        let mut heap = BinaryHeap::from(init);
        let mut total_volume = Volume::ZERO;
        let volume_limit = order.details.volume;
        let mut total_payed = Money::ZERO;

        while Contribution::is_enough(total_contrib, order.details.contribution) && total_volume < volume_limit {
            let next = heap.pop().unwrap();
            if next.cumulative_volume > volume_limit {
                // We're done, and have some extra for this order
                let volume_used = volume_limit - total_volume;
                // Correct the total volume we've gone through
                total_volume = volume_limit;
                let HeapEntries { order: mut order_to_return, side, cumulative_volume: _ } = next;
                order_to_return.details.volume -= volume_used;
                // Pay
                total_payed += volume_used * Contribution::remainder(total_contrib);
                let d = order_to_return.details;
                // Record this party's payment
                other_resolutions.push(CounterpartyPayment {
                    customer: d.customer,
                    volume: d.volume,
                    contribution: d.contribution,
                    side,
                });
                // Put this back in the book
                other_sides.get_mut(&next.side).unwrap().insert(order_to_return.identifier(), order_to_return);
            } else {
                // We've exhausted this book order
                // Correct the volume value
                total_volume = next.cumulative_volume;
                // Pay
                total_payed += next.order.details.volume * Contribution::remainder(total_contrib);
                // This order is complete: remember to do something about that
                let d = next.order.details;
                other_resolutions.push(CounterpartyPayment {
                    customer: d.customer,
                    volume: d.volume,
                    contribution: d.contribution,
                    side: next.side,
                });
                // Get our new item for this side
                let new_order = match other_sides.get_mut(&next.side).unwrap().pop_last() {
                    None => break, // Out of orders!
                    Some((_, o)) => o,
                };
                let new_item = HeapEntries {
                    order: new_order,
                    side: next.side,
                    cumulative_volume: new_order.details.volume + total_volume,
                };
                // Update the total contribution
                total_contrib -= d.contribution;
                total_contrib += new_order.details.contribution;
                
                heap.push(new_item);
            }
        }
        
        // Return the orders we took from the book and record the portion that got transacted
        for HeapEntries { mut order, side, cumulative_volume } in heap {
            let volume_remaining = cumulative_volume - total_volume;
            let volume_used = order.details.volume - volume_remaining;
            assert!(volume_remaining >= Volume::ZERO);
            assert!(volume_used >= Volume::ZERO);
            if volume_used > Volume::ZERO {
                let d = order.details;
                other_resolutions.push(CounterpartyPayment {
                    customer: d.customer,
                    volume: volume_used,
                    contribution: d.contribution,
                    side,
                });
            }
            if volume_remaining > Volume::ZERO {
                other_sides.get_mut(&side).unwrap().insert(order.identifier(), order);
                order.details.volume = volume_remaining;
            }
        }

        TransactionDyn::checked_make(total_volume, total_payed, other_resolutions).unwrap()
    }
   
    // Runtime: O(m)
    fn can_complete(&self, sides: &[Side], order: Order) -> Option<bool> {
        assert!(sides.iter().all(|s| s.0 < self.num_sides()));
        assert!(sides.len() > 0);
        let other_sides = self.book.iter()
            .enumerate()
            .filter(|(o, _)| !sides.contains(&Side(*o))) // TODO: performance
            .map(|(_, a)|  a.iter().rev())
            .collect::<Vec<_>>();
        // Count along the other sides and see what max contribution we need to give each side
        let other_contribs_sum = other_sides.into_iter()
            .map(|i| {
                let mut remaining_volume = order.details.volume;
                i.skip_while(|(_, v)| {
                    // Skip until we've reached an order that either perfectly gets used up or has some remaining
                    remaining_volume -= v.details.volume;
                    remaining_volume > Volume::ZERO
                }).next()
                    .map(|(_, v)| v.details.contribution)
            }).collect::<Option<Vec<_>>>()?
            .into_iter()
            .sum::<Contribution>();
        Some(other_contribs_sum + order.details.contribution >= Contribution::ONE)
    }
    
    pub fn handle_fok(&mut self, sides: &[Side], order: Order) -> Option<TransactionDyn> {
        if self.can_complete(sides, order)? {
            Some(self.handle_unbookable_partial(sides, order))            
        } else {
            None
        }
    }

    pub fn handle_partialable_order(&mut self, side: Side, order: Order) -> (Option<Identifier>, TransactionDyn)
    {
        assert!(side.0 < self.num_sides());
        
        let out = self.handle_unbookable_partial(&vec![side], order);

        if out.taker_volume == order.details.volume {
            (None, out)
        } else {
            let id = self.insert_order(side, order.details);
            (Some(id), out)
        }
         
    }
    
    fn insert_order(&mut self, Side(side): Side, order: Details) -> Identifier {
        assert!(side < self.num_sides());
        
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
        
        self.book[side].insert(identifier, outstanding_order);

        identifier
    }

    pub fn cancel_order(&mut self, Side(side): Side, orderid: Identifier) -> Option<OutstandingOrder> {
        assert!(side < self.num_sides());
        self.book[side].remove(&orderid)
    }
        
    pub fn get_order_book(&self) -> Vec<Vec<OutstandingOrder>> {
        self.book.iter().map(|b| b.iter().map(|(_, o)| o.clone()).collect()).collect()
    }
}


#[cfg(test)]
mod matching_engine_tests {
    use std::collections::HashSet;

    use super::*;

    fn v(value: u32) -> Volume {
        Volume::from_shares(value)
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
                // let e = expct_contribs[i].1.0;
                // let a = contributions[i].contribution.0;
                // assert!(a == e || a == (e-1) || a == (e+1), "{a}, {e}");
            }
        }
    }

    fn check_order_response_dyn(resp: TransactionDyn, expct: (Volume, Money, Vec<(CustomerID, Volume, Contribution, Side)>)) {
        assert_eq!(resp.taker_volume, expct.0);
        assert_eq!(resp.taker_pays, expct.1);
        assert_eq!(resp.counterparties.len(), expct.2.len());
        // Turn one into a set
        let mut got = resp.counterparties.into_iter().collect::<HashSet<_>>();
        for (customer, volume, contribution, side) in expct.2 {
            assert!(got.remove(&CounterpartyPayment {
                customer,
                volume,
                contribution,
                side,
            }))
        }
        assert_eq!(got.len(), 0);
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
    fn test_bin_option() {
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

    fn n(money: i64) -> Money {
        Money::from_dollars(money)
    }

    #[test]
    fn test_fok() {
        let mut m = MatchingEngineDyn::new(2);

        let o1 = p(i(0), v(10), c(0.45));

        check_order_response_dyn(m.handle_partialable_order(Side(0), o1).1, (v(0), n(0), vec![]));

        let o2 = f(i(1), v(20), c(0.6));

        assert_eq!(m.handle_fok(&vec![Side(1)], o2), None);

        let o3 = p(i(1), v(20), c(0.6));

        check_order_response_dyn(m.handle_partialable_order(Side(1), o3).1, (v(10), v(10)*c(0.55), vec![(i(0), v(10), c(0.45), Side(0))]));
    }
}


#[cfg(test)]
mod ordertest {
    fn v(volume: u32) -> Volume {
        Volume::from_shares(volume)
    }
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
                volume: v(10),
                contribution: Contribution::from_float(0.4),
            }
        };
        let o2 = OutstandingOrder {
            orderid: 2,
            details: Details {
                creq: CompletionRequirement::PartialOk,
                customer: c1,
                volume: v(10),
                contribution: Contribution::from_float(0.4),
            }
        };
        let o3 = OutstandingOrder {
            orderid: 1,
            details: Details {
                creq: CompletionRequirement::PartialOk,
                customer: c1,
                volume: v(10),
                contribution: Contribution::from_float(0.7),
            }
        };
        let o4 = OutstandingOrder {
            orderid: 1,
            details: Details {
                creq: CompletionRequirement::FillOnly,
                customer: c2,
                volume: v(20),
                contribution: Contribution::from_float(0.4),
            }
        };

        assert_eq!(o1.cmp(&o2), Ordering::Greater);
        assert_eq!(o1.cmp(&o3), Ordering::Less);
        assert_eq!(o1.cmp(&o4), Ordering::Equal);
        assert!(o1 != o4);
    }
}
