// Keeps tracks of everyone's positions and balances

use std::{collections::{hash_map::Entry, HashMap}, io::Read};

use ciborium::from_reader_with_buffer;

use crate::{message::Message, Contribution, CustomerID, InstrumentID, Money, Position, Side, Volume};

type Error = Box<dyn std::error::Error + 'static>;

// Thought: How do we handle automatically cancelling out positions?
// Like if I own all sides of a disjoint set contract, they should cancel
// each other out and I should get money.
// What do about that?
// Instead of our hashmap<side, volume> nonsense, we have a special instrument
// handling thing, which keeps a count of the volume for each side, and
// crucially, a count of how many of these values are non-zero. When there are
// no longer any non-zero values, it finds the minimum volume across all those
// values, subtracts it from all of them, and returns how much volume was just
// cancelled out

struct PositionState {
    volumes: Vec<Volume>,
    non_zero: usize,
}

impl PositionState {
    pub fn new(num_sides: usize) -> Self {
        assert!(num_sides >= 2);
        Self {
            volumes: vec![Volume(0); num_sides],
            non_zero: num_sides,
        }
    }

    // Returns None if there was no cancellation, and the amount of volume
    // cancelled out if there was cancellation
    pub fn add_volume(&mut self, Side(index): Side, vol: Volume) -> Option<Volume> {
        assert!(index < self.volumes.len());
        let v = &mut self.volumes[index];
        *v += vol;
        if *v == vol {
            // This used to be a zero. Decrement our count
            self.non_zero -= 1;
            if self.non_zero == 0 {
                // We now have positive volume for all sides of this
                // instrument
                // Cancel out some volume
                let v_to_rem = *self.volumes.iter().min().expect("Can't have a zero-sided instrument");
                self.volumes.iter_mut().for_each(|v| *v -= v_to_rem);
                self.non_zero = self.volumes.iter().filter(|v| **v == Volume::ZERO).count();
                Some(v_to_rem)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn get_volume(&self, Side(index): Side) -> Volume {
        assert!(index < self.volumes.len());
        self.volumes[index]
    }
}

// Problem: no efficient way of finding all positions held by a customer
pub struct PosManager {
    balances: HashMap<CustomerID, Money>,
    positions: HashMap<InstrumentID, HashMap<CustomerID, PositionState>>,
}

impl PosManager {
    pub fn new() -> Self {
        PosManager {
            balances: HashMap::new(),
            positions: HashMap::new(),
        }
    }

    // Uses an UnexpectedEof to detect when we've read the last message
    // Does that work for other possible readers? Idk
    // Also if the last message is truncated, does that mean we won't
    // include it and won't signal an error? That's not great
    pub fn from_record<R: Read>(source: &mut R) -> Result<Self, Error> {
        let mut out = Self::new();
        let mut buffer = vec![0; 65536];
        loop {
            match from_reader_with_buffer::<Message, _>(&mut *source, &mut buffer) {
                Ok(message) => out.apply_message(message),
                Err(ciborium::de::Error::Io(io_error)) => {
                    match io_error.kind() {
                        std::io::ErrorKind::UnexpectedEof => {
                            break Ok(out)
                        },
                        _ => Err(ciborium::de::Error::Io(io_error))?,
                    }
                }
                Err(e) => Err(e)?,
            };
        }
    }
    
    fn apply_message(&mut self, message: Message) {
        match message {
            Message::AddCustomer(cid, bal) => {self.add_customer(cid, bal);},
            Message::ModifyBalance(cid, amt) => {let _ = self.modify_balance(&cid, amt);},
            Message::CreateInstrument(iid) => {let _ = self.create_instrument(iid);},
            Message::CreditPosition(cid, pos, vol) => {let _ = self.credit_position(cid, pos, vol);},
            Message::HandleTransaction(iid, vol, contribs) => {let _ = self.handle_transaction(&iid, vol, &contribs);}
        }
    }

    // Panics if customer already exists
    pub fn add_customer(&mut self, account: CustomerID, initial_balance: Money) {
        if self.balances.insert(account, initial_balance).is_some() {
            panic!("Same customerid inserted twice");
        }
    }

    pub fn modify_balance(&mut self, account: &CustomerID, amount: Money) -> Result<Money, Error> {
        let v = self.balances.get_mut(account).ok_or("No such account")?;
        *v += amount;
        Ok(*v)
    }

    pub fn get_balance(&self, account: &CustomerID) -> Option<Money> {
        self.balances.get(account).copied()
    }

    pub fn create_instrument(&mut self, instrument: InstrumentID) -> Result<(), Error> {
        match self.positions.entry(instrument) {
            Entry::Occupied(_) => Err("Duplicate instrumentid".into()),
            Entry::Vacant(v) => {
                v.insert(HashMap::new());
                Ok(())
            }
        }
    }

    // Probably shouldn't need to be called directly
    pub fn credit_position(&mut self, account: CustomerID, Position(instrument, side): Position, volume: Volume) -> Result<(), Error> {
        let i_map = self.positions.get_mut(&instrument).ok_or::<Error>("No such instrument".into())?;
        let c_map = match i_map.entry(account) {
            Entry::Occupied(o) => o.into_mut(),
            Entry::Vacant(v) => v.insert(PositionState::new(instrument.num_sides())),
        };
        match c_map.add_volume(side, volume) {
            None => {},
            Some(cancelled_volume) => {
                // Need to credit the account
                self.modify_balance(&account, cancelled_volume*Contribution::ONE)?;
            }
        }
        Ok(())
        }

    pub fn get_position(&self, account: &CustomerID, Position(instrument, side): &Position) -> Option<Volume> {
        Some(self.positions.get(instrument)?.get(account)?.get_volume(*side))
    }

    // Errors on invalid instrument, but panics on invalid customer
    pub fn handle_transaction(&mut self, instrument: &InstrumentID, volume: Volume, contribs: &[(CustomerID, Contribution)]) -> Result<(), Error> {
        let i_map = self.positions.get_mut(instrument).ok_or::<Error>("No such instrument".into())?;
        for (side, (cid, contrib)) in contribs.iter().enumerate() {
            let side = Side(side);
            // Modify position count
            let c_map = match i_map.entry(*cid) {
                Entry::Occupied(o) => o.into_mut(),
                Entry::Vacant(v) => v.insert(PositionState::new(instrument.num_sides())),
            };
            let cancelled = c_map.add_volume(side, volume).unwrap_or(Volume::ZERO);
            // Modify balance
            let b = self.balances.get_mut(cid).expect("Customer not found"); // Panic instead of error because we're now in an invalid state
            *b -= volume * *contrib;
            // the cancelled volume offsets how much we need to pay
            *b += cancelled * Contribution::ONE;
        }
        Ok(())
    }

    
}
