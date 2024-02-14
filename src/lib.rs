#![feature(array_methods)]
mod book;
mod auction;
mod positions;
mod logging;
mod message;
mod units;

use std::collections::HashMap;

use book::{MatchingEngine, Order};
use positions::PosManager;
use units::{InstrumentID, Side};

// TODO
//  - Logging matching engine messages
//  - Combinatorial book orders
//    - Especially "no" positions (all but one, i.e. Bob will not be
//      president, but any of the others could be)
//  - Pro-rata matching?
//  - Automatic cancellation

pub struct Server {
    engines: HashMap<InstrumentID, MatchingEngine>,
    money_stuff: PosManager,
}

impl Server {
    pub fn new() -> Self {
        Self {
            engines: HashMap::new(),
            money_stuff: PosManager::new(),
        }
    }

    pub fn make_book_order(&mut self, instrument: InstrumentID, side: Side, order: Order) {
        let a = self.engines.get_mut(&instrument).unwrap().handle_partialable_order(side, order);
    }
}
