use crate::units::{Contribution, CustomerID, InstrumentID, Money, Position, Volume};

use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize)]
pub enum Message {
    AddCustomer(CustomerID, Money),
    ModifyBalance(CustomerID, Money),
    CreateInstrument(InstrumentID),
    CreditPosition(CustomerID, Position, Volume),
    HandleTransaction(InstrumentID, Volume, Vec<(CustomerID, Contribution)>),
}
