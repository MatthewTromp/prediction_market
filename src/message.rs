use crate::{book::Identifier, units::{CompletionRequirement, Contribution, CustomerID, InstrumentID, Money, Position, Side, Timing, Volume}};

use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize)]
pub enum PosManagerMessage {
    AddCustomer(CustomerID, Money),
    ModifyBalance(CustomerID, Money),
    CreateInstrument(InstrumentID),
    CreditPosition(CustomerID, Position, Volume),
    HandleTransaction(InstrumentID, Volume, Vec<(CustomerID, Contribution)>),
}

pub enum MatchingEngineMessage {
    MakeOrder(CompletionRequirement, Timing, InstrumentID, Vec<Side>, Volume, Contribution),
    CancelOrder(Side, Identifier)
}
