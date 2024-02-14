use std::io::Write;

use ciborium::into_writer;

use crate::{book::MatchingEngine, positions::PosManager, units::{Contribution, CustomerID, InstrumentID, Money, Position, Volume}};
use crate::message::PosManagerMessage;

type Error = Box<dyn std::error::Error + 'static>;

pub struct LoggedPosManager<L: Write> {
    pos_manager: PosManager,
    log_sink: L,
}

impl<L: Write> LoggedPosManager<L> {
    pub fn new(sink: L) -> Self {
        LoggedPosManager {
            pos_manager: PosManager::new(),
            log_sink: sink,
        }
    }

    fn log_message(&mut self, message: &PosManagerMessage) -> Result<(), Error> {
        into_writer(message, &mut self.log_sink)?;
        Ok(())
    }

    pub fn add_customer(&mut self, account: CustomerID, initial_balance: Money) ->  Result<(), Error> {
        self.pos_manager.add_customer(account, initial_balance);
        self.log_message(&PosManagerMessage::AddCustomer(account, initial_balance))?;
        Ok(())
    }

    pub fn modify_balance(&mut self, account: CustomerID, amount: Money) -> Result<Result<Money, Error>, Error> {
        let out = self.pos_manager.modify_balance(&account, amount);
        self.log_message(&PosManagerMessage::ModifyBalance(account, amount))?;
        Ok(out)
    }

    pub fn create_instrument(&mut self, instrument: InstrumentID) -> Result<Result<(), Error>, Error> {
        let out = self.pos_manager.create_instrument(instrument);
        self.log_message(&PosManagerMessage::CreateInstrument(instrument))?;
        Ok(out)
    }

    pub fn credit_position(&mut self, account: CustomerID, position: Position, volume: Volume) -> Result<Result<(), Error>, Error> {
        let out = self.pos_manager.credit_position(account, position, volume);
        self.log_message(&PosManagerMessage::CreditPosition(account, position, volume))?;
        Ok(out)
    }

    pub fn handle_transaction(&mut self, instrument: InstrumentID, volume: Volume, contribs: Vec<(CustomerID, Contribution)>) -> Result<Result<(), Error>, Error> {
        let out = self.pos_manager.handle_transaction(&instrument, volume, &contribs);
        self.log_message(&PosManagerMessage::HandleTransaction(instrument, volume, contribs))?;
        Ok(out)
    }

    pub fn get_manager(&self) -> &PosManager {
        &self.pos_manager
    }
}


struct LoggedMatchingEngine<L: Write> {
    matching_engine: MatchingEngine,
    log_sink: L,
}
