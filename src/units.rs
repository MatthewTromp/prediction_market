use std::{iter::Sum, ops::{Add, AddAssign, Mul, Sub, SubAssign}};

use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CompletionRequirement {
    FillOnly,
    PartialOk,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Timing {
    Instant,
    TillCancelled,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct InstrumentID {
    id: usize,
    num_sides: usize,
}

impl InstrumentID {
    pub fn num_sides(&self) -> usize {
        self.num_sides
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Side(pub usize);
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Position(pub InstrumentID, pub Side);

// Represents a number of (fractions of) shares, where a share is something
// that resolves to one dollar
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Volume(u32);

impl Add for Volume {
    type Output = Volume;

    fn add(self, rhs: Self) -> Self::Output {
        Volume(self.0 + rhs.0)
    }
}

impl Sub for Volume {
    type Output = Volume;

    fn sub(self, rhs: Self) -> Self::Output {
        Volume(self.0 - rhs.0)
    }
}

impl Mul<Contribution> for Volume {
    type Output = Money;

    fn mul(self, rhs: Contribution) -> Self::Output {
        Money((self.0 as i64) * (rhs.0 as i64))
    }
}

impl SubAssign for Volume {
    fn sub_assign(&mut self, rhs: Self) {
        self.0 -= rhs.0
    }
}

impl AddAssign for Volume {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0
    }
}

const ONE_SHARE: u32 = 1;

impl Volume {
    pub const ZERO: Self = Self(0);
    pub const ONE: Self = Self(ONE_SHARE);

    pub fn from_shares(shares: u32) -> Self {
        Volume(shares*ONE_SHARE)
    }
}

// Represents a "probability price": the share of the price of the instrument
// paid by each party to a disjoint set contract
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Contribution(i32);
const TOTAL_CONTRIB: i32 = 10_000;

// Represents an amount of money
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Money(i64);

const ONE_DOLLAR: i64 = (TOTAL_CONTRIB as i64) * (ONE_SHARE as i64);

impl Contribution {
    pub const ZERO: Contribution = Contribution(0);
    pub const ONE: Contribution = Contribution(TOTAL_CONTRIB);

    pub fn is_enough(c1: Contribution, c2: Contribution) -> bool {
        c1 + c2 >= Contribution::ONE
    }

    pub fn remainder(c: Contribution) -> Contribution {
        Self::ONE - c
    }
    
    fn from_raw(yes_proportion: i32) -> Self {
        Contribution(yes_proportion)
    }

    pub fn from_float(yes_proportion: f64) -> Self {
        Self::from_raw((yes_proportion*(TOTAL_CONTRIB as f64)).round() as i32)
    }

    pub fn get_total_price(&self, volume: &Volume) -> Money {
        Money((self.0 as i64) * (volume.0 as i64))
    }

    pub fn share_contribs<const N: usize>(contribs: [Contribution; N]) -> Option<[Contribution; N]> {
        // Normalize the contributions
        let sum: i32 = contribs.iter().map(|c| c.0).sum();
        if sum >= TOTAL_CONTRIB {
            let mut a = contribs.map(|c| ((c.0 as u64*TOTAL_CONTRIB as u64)/sum as u64) as i32);
            let missing = TOTAL_CONTRIB - a.iter().sum::<i32>();
            for i in 0..missing {
                a[i as usize] += 1;
            }
            Some(a.map(|v| Contribution(v)))
        } else {
            None
        }
    }

    pub fn share_contribs_dyn(contribs: Vec<Contribution>) -> Option<Vec<Contribution>> {
        // Normalize the contributions
        let sum: i32 = contribs.iter().map(|c| c.0).sum();
        if sum >= TOTAL_CONTRIB {
            let mut a = contribs.into_iter().map(|c| ((c.0 as u64*TOTAL_CONTRIB as u64)/sum as u64) as i32).collect::<Vec<_>>();
            let missing = TOTAL_CONTRIB - a.iter().sum::<i32>();
            for i in 0..missing {
                a[i as usize] += 1;
            }
            Some(a.into_iter().map(|v| Contribution(v)).collect())
        } else {
            None
        }
    }
}


impl Add for Money {
    type Output = Money;

    fn add(self, rhs: Self) -> Self::Output {
        Money(self.0 + rhs.0)
    }
}

impl Sub for Money {
    type Output = Money;

    fn sub(self, rhs: Self) -> Self::Output {
        Money(self.0 + rhs.0)
    }
}

impl SubAssign for Money {
    fn sub_assign(&mut self, rhs: Self) {
        self.0 -= rhs.0
    }
}

impl AddAssign for Money {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0
    }
}


impl Money {
    pub const ZERO: Money = Money(0);
    
    fn from_raw(r: i64) -> Self {
        Money(r)
    }

    pub fn from_dollars(dollars: i64) -> Self {
        Money(dollars*ONE_DOLLAR)
    }

    fn from_cents(cents: i64) -> Self {
        Money(cents*ONE_DOLLAR/100)
    }
}

impl Sum<Money> for Money {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Self::ZERO, |t, v| t + v)
    }
}

impl Add for Contribution {
    type Output = Contribution;

    fn add(self, rhs: Self) -> Self::Output {
        Contribution(self.0 + rhs.0)
    }
}

impl Sub for Contribution {
    type Output = Contribution;

    fn sub(self, rhs: Self) -> Self::Output {
        Contribution(self.0 - rhs.0)
    }
}

impl AddAssign for Contribution {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0
    }
}

impl SubAssign for Contribution {
    fn sub_assign(&mut self, rhs: Self) {
        self.0 -= rhs.0
    }
}

impl Mul<Volume> for Contribution {
    type Output = Money;
    
    fn mul(self, rhs: Volume) -> Money {
        Money((self.0 as i64) * (rhs.0 as i64))
    }
}

impl Sum<Self> for Contribution {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Self::ZERO, |t, v| t + v)
    }
}

impl Sum<Self> for Volume {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Self::ZERO, |t, v| t + v)
    }
}



#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct CustomerID(pub usize);
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_contribution_is_enough() {
        let c1 = Contribution::from_float(0.3);
        let c2 = Contribution::from_float(0.4);
        let c3 = Contribution::from_float(0.699);
        let c4 = Contribution::from_float(0.7);

        assert!(!Contribution::is_enough(c1, c2));
        assert!(!Contribution::is_enough(c1, c3));
        assert!(Contribution::is_enough(c1, c4));
    }
}
