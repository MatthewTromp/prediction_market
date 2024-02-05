// OneChronos style auction system
//  We collect a bunch of orders, then run an auction between them periodically
//  For a simple two-side transaction with only partialable orders and no
// constraints, our system is simple: find an equilibrium price (the price
// where there is as much volume on one side as the other) and perform the
// transaction
//  What about, say, a three-sided transaction? Then we're looking for a point
// in 2-d space where there is as much volume on any side as on any of the
// other two.
//  What we get are these downward-sloping curves of volume as a function of
// contribution. Call these f1, f2, f3. Then we need to find contributions
// c1, c2, c3 s.t. c1 + c2 + c3 = 1 and f1(c1) = f2(c2) = f3(c3)
//  How do you do that? Well, for any choice of c3, there's a split of the
// remaining contribution between c1 and c2 s.t. f1(c1) = f2(c2). So we
// can get a function c3 -> (v1|v2, c1), where v1|v2 is v1 and v2 (this is
// the point at which they're equal). And then we also have a function
// c3 -> v3. So we want to find c3 such that v3 == v1|v2
//  I think, by walking through f1 and f2 together, we can construct this
// curve.

//  Think of these as contribution as a function of volume curves instead.
// Then we're looking for v s.t. f1'(v) + f2'(v) = c1 + c2 = 1.
//  Or, okay, so we make f12'(v) = f1'(v) + f2'(v). And then we just find
// where f12'(v) = 1.
//  For 3-way contracts, we take f123'(v) = f1'(v) + f2'(v) + f3'(v) and
// find where f123'(v) = 1. Hmmmmm, this feels almost too easy....

//  Okay, so how do we solve this problem? This feels like a leetcode problem
// or something. Probably been done a billion times

//  Is it possible for this to miss possible transactions?
//  Wait okay, what happens if we have some weird situation, like we have a
// 3-side contract with these orders (all volumes are 1)
// A: 0.3, 0.4
// B: 0.4, 0.5
// C: 0.1, 0.3
//  Then we can make (0.3, 0.4, 0.3) and (0.4, 0.5, 0.1). Or we can make
// (0.4, 0.5, 0.3). That's less volume!. But, maybe that doesn't matter
//  The first option doesn't actually improve anyone's situation. They're
// all indifferent. Whereas the second one leads to a net gain. Also the
// first wouldn't be incentive compatible. By decreasing your contribution
// below your true value, you could get more utility, which shouldn't be
// possible
//  I think this has to do with the "trade at a single price" thing from
// OneChronos' website. If you allow trades at different prices you get
// stuff like this

//  Okay so this is fine. This works, I think. And I'm pretty sure it runs
// in linear time, which is pretty cool. Or, hm, no, maybe not. Hmmmmmm
//  Maybe I should actually write it

//  It's, like O(nlog(n)*k) where n is the number of orders and k is the
// number of sides to a bet???


//  Auction ideas
//  Low-volume instruments suffer from the same problems when they're
// auctions as everything does when it's a book. 
//  But the auction can't perform worse than a book would for low-
// volume securities. That would be embarassing.
//  In particular, that means no dummy midpoint pricing
//  Maybe we can maintain an order book, and only trigger auctions when
// there's enough volume available?

use crate::{Contribution, CustomerID, Volume};

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
struct Order(pub Contribution, pub Volume, pub CustomerID);

// Stepwise graph of contribution as a function of volume
struct CVGraph(pub Vec<(Volume, Contribution)>);

impl CVGraph {
    pub fn from_orders(mut orders: Vec<Order>) -> Self {
        // How do this?
        // 1. Sort in decreasing order of contribution
        orders.sort_by(|o1, o2| o2.0.cmp(&o1.0));
        // Accumulate the volumes
        let surface = orders.into_iter()
            .fold(vec![(Volume(0), Contribution::ONE)],
                |mut surface, Order(c, v, _)| {
                    let (v_sum, l) = surface.last_mut().unwrap();
                    if (*l).eq(&c) {
                        *v_sum += v;
                    } else {
                        let new_v = *v_sum + v;
                        surface.push((new_v, c));
                    }
                    surface
                });
        
        Self(surface)
    }

    fn sum(graphs: &[CVGraph]) -> CVGraph {
        // Calculate derivatives
        // TODO: The derivatives are already sorted, so we should be able to do this with a
        // series of merges, which should take more like O(n*logk) where n is the total number
        // of elements and k is the number of vectors
        let (mut all_derivs, init_contrib) = graphs.into_iter()
            .map(Self::derivative)
            .fold((vec![], Contribution::ZERO), |(mut all_derivs, total_c), (mut derivs, init_c)| {
                all_derivs.append(&mut derivs);
                (all_derivs, total_c + init_c)
            });

        all_derivs.sort_by_key(|(v, _)| *v);
        
        // Combine the derivatives and return the summed version
        let surface = all_derivs.into_iter().fold(vec![(Volume(0), init_contrib)], |mut s, (v, c)| {
            let (lv, running_c) = s.last_mut().unwrap();
            if (*lv).eq(&v) {
                *running_c += c;
            } else {
                let new_running_c = *running_c + c;
                s.push((v, new_running_c));
            }
            s
        });

        CVGraph(surface)
    }

    fn derivative(g: &CVGraph) -> (Vec<(Volume, Contribution)>, Contribution) {
        let mut iter = g.0.iter();
        let (vi, ci) = iter.next().unwrap();
        assert_eq!(*vi, Volume(0));
        (iter.fold((vec![], ci), |(mut out, last_c), (v, c)| {
            out.push((*v, *c - *last_c));
            (out, c)
        }).0, *ci)
    }
}

struct Completion(Contribution, Vec<(CustomerID, Volume)>);

fn solve(mut orders: Vec<Vec<Order>>) -> Vec<Completion> {
    // Sort orders in descending order of contribution (in order of desireability)
    orders.iter_mut().for_each(|v| v.sort_by(|o1, o2| o2.0.cmp(&o1.0)));

    // Step 1: Construct our contribution/volume graphs
    let surfaces = orders.clone().into_iter().map(CVGraph::from_orders).collect::<Vec<CVGraph>>();
    
    // Step 2: Combine the graphs into a sum graph
    let total = CVGraph::sum(&surfaces);
    
    // Step 3: Find the largest volume with a contribution of at least 1.0
    let (target_v, _) = *total.0.iter().rev().skip_while(|(_, c)| *c < Contribution::ONE).next().expect("No volume big enough despite massive initial element");

    // Step 4: Find the orders to include
    orders.into_iter().map(|os| {
        let mut remaining_v = target_v;
        let mut out = vec![];
        let mut contrib = Contribution::ZERO;
        for o in os {
            contrib = o.0;
            if o.1 < remaining_v {
                out.push((o.2, o.1));
                remaining_v -= o.1;
            } else {
                out.push((o.2, remaining_v));
                break;
            }
        }
        Completion(contrib, out)
    }).collect()
}

//  Another idea: only have binary options, and somehow maybe have some kind
// of external constraint system that enforces disjoint sets or that equalizes
// between disjoint sets or something? That could also just be left to
// arbitrage but it's worth avoiding that if possible

//  Also, can we decrease the tick size with an auction system? I think so,
// although you still have to be careful. But, like, traders could just add
// jitter to their prices if this became a serious issue, right? Maybe the
// market should add that jitter itself???

#[cfg(test)]
mod cvgraphtest {
    use super::*;

    fn c(contrib: f64) -> Contribution {
        Contribution::from_float(contrib)
    }
    fn v(volume: u32) -> Volume {
        Volume(volume)
    }
    fn o(contrib: f64, volume: u32) -> Order{
        Order(Contribution::from_float(contrib), Volume(volume), CustomerID(0))
    }
    #[test]
    fn test_from_orders() {
        let orders = vec![o(0.1, 30), o(0.4, 20), o(0.2, 10), o(0.1, 20)];
        let cvg = CVGraph::from_orders(orders);
        assert_eq!(cvg.0, vec![(v(0), c(1.0)), (v(20), c(0.4)), (v(30), c(0.2)), (v(80), c(0.1))])
    }

    #[test]
    fn test_sum() {
        let cvg1 = CVGraph(vec![(v(0), c(1.0)), (v(10), c(0.8)), (v(20), c(0.6))]);
        let cvg2 = CVGraph(vec![(v(0), c(1.0)), (v(5), c(0.7)), (v(15), c(0.5)), (v(20), c(0.3))]);
        let sm = CVGraph::sum(&vec![cvg1, cvg2]);
        assert_eq!(sm.0, vec![(v(0), c(2.0)), (v(5), c(1.7)), (v(10), c(1.5)), (v(15), c(1.3)), (v(20), c(0.9))]);
    }
}

#[cfg(test)]
mod testsolve {
    use super::*;

    fn c(contrib: f64) -> Contribution {
        Contribution::from_float(contrib)
    }
    fn v(volume: u32) -> Volume {
        Volume(volume)
    }
    fn i(id: usize) -> CustomerID {
        CustomerID(id)
    }
    fn o(contrib: f64, volume: u32, id: usize) -> Order{
        Order(Contribution::from_float(contrib), Volume(volume), i(id))
    }
    #[test]
    fn testsolve() {
        let side1 = vec![o(0.3, 20, 1), o(0.5, 30, 2), o(0.4, 5, 3), o(0.4, 10, 4)];
        let side2 = vec![o(0.8, 30, 10), o(0.7, 20, 11), o(0.5, 10, 12)];

        let cs = solve(vec![side1, side2]);
        let mut cs = cs.into_iter();
        let Completion(c1, os1) = cs.next().unwrap();
        assert_eq!(c1, c(0.3));
        assert_eq!(os1, vec![(i(2), v(30)), (i(3), v(5)), (i(4), v(10)), (i(1), v(5))]);

        let Completion(c2, os2) = cs.next().unwrap();
        assert_eq!(c2, c(0.7));
        assert_eq!(os2, vec![(i(10), v(30)), (i(11), v(20))]);

        assert_eq!(os1.iter().map(|(_, v)| v.0).sum::<u32>(), os2.iter().map(|(_, v)| v.0).sum::<u32>());

        assert!(cs.next().is_none());
    }
}
