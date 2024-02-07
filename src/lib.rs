#![feature(array_methods)]
mod book;
mod auction;
mod positions;
mod logging;
mod message;
mod units;

use std::collections::HashMap;

use book::{MatchingEngineDyn, Order};
use positions::PosManager;
use units::{InstrumentID, Side};


pub struct Server {
    engines: HashMap<InstrumentID, MatchingEngineDyn>,
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
