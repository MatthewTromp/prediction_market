//  So, once a transaction has happened, we need to keep track of it,
// keep track of who owes how much, keep track of how much money
// everyone has left, maybe flag when a customer runs out of funds and
// prevent them from trading to keep them from ruining the market.
//  Also we need to keep track of all the contracts everyone owns,
// resolve them, credit people's accounts accordingly
//  And ofc it all needs to be robust to power failures but I think
// that can mostly be done by just using an ACID database for our
// operations. Although it's fine to forget some trades, as long as
// we forget all sides.

//  Position management: every side of every bet has an ID. We keep
// track of how much of every instrument every customer has.
// Conceptually, we have a defaultdict that's indexed by instrument,
// then by customer. When we resolve an instrument, we go through all
// customers that hold that instrument and credit their accounts
// accordingly. All non-winning sides of a bet just get erased. But
// probably it should still be maintained somewhere, just in case of
// a mistake or if we need to go back, and also so users can review
// their previous trades (although keeping track of that is probably
// their problem).

//  Some things that need to be dealt with:
//  - Automatic cancellation of bets. If you own all sides of a bet,
//    the annihilate and you get a dollar out of it
//  - If there's a lot of transactions happening we'll need some sort
//    of high performance collect and aggregate system before actually
//    writing to the database


use diesel::dsl::sum;
use diesel::sql_types::Integer;
use diesel::upsert::excluded;
use diesel::{associations::HasTable, sqlite::SqliteConnection};
use diesel::prelude::*;
use dotenvy::dotenv;
use std::borrow::BorrowMut;
use std::env;
use crate::schema::positions::instrumentid;
use crate::TOTAL_CONTRIB;
use crate::{book::{Transaction, PH1}, Contribution, CustomerID, Volume};
use crate::models::{Position, PostionId};

pub struct PositionChange {
    instrumentid: i32,
    side: i32,
    customer_id: CustomerID,
    volume: Volume,
    contribution: Contribution,
}

pub fn establish_connection() -> SqliteConnection {
    dotenv().ok();
    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    SqliteConnection::establish(&database_url)
        .unwrap_or_else(|_| panic!("Error connecting to {}", database_url))
}

type Error = Box<dyn std::error::Error + 'static>;

// It should be possible to do all these things in the database directly
// There should be no need for round trips. And yet.

// A transaction consists of a collection of volume/contribution pairs that
// must all be executed (positions created and balances updated) atomically,
// as when an instrument is created
// Does n*2 transactions, where n is the number of position changes
pub fn handle_transaction(conn: &mut SqliteConnection, transaction: &[PositionChange]) -> Result<(), Error> {
    conn.transaction(|conn| {
        use crate::schema::customers::dsl::*;
        use crate::schema::positions::{self, dsl::*};
        // TODO: Figure out how to do all of this in one execution, instead
        // of needing n*2 round trips
        // Create the empty position if it doesn't already exist
        let empty_poses = transaction.iter()
            .map(|PositionChange { instrumentid: iid, side: sde, customer_id: cid, volume: vol, contribution: _ }| Position {
                instrumentid: *iid,
                side: *sde,
                customer_id: cid.0 as i32,
                volume: vol.0 as i32,
            })
            .collect::<Vec<Position>>();
        // TODO: It should be possible to replace this with a single query but sqlite is bad???
        for empty_pos in empty_poses {
            diesel::insert_into(positions::table)
                .values(&empty_pos)
                .on_conflict((instrumentid, side, customer_id))
                .do_update()
                .set(volume.eq(volume + excluded(volume)))
                .execute(conn)?;
        }
            
        for PositionChange { instrumentid: iid, side: sde, customer_id: cid, volume: vol, contribution } in transaction {
            // Update the customer's balance
            let cid = crate::models::CustomerId { id: cid.0 as i32 };
            diesel::update(&cid)
                .set(balance.eq(balance - (contribution.get_total_price(vol)).0 as i32))
                .execute(conn)?;
            
        }
        Ok(())
    })
}

// Resolves an instrument: credits the accounts of all the holders of
// the winning side, and deletes all other sides
// Performance: n+2 queries where n is the number of winning positions
// of this instrument
pub fn resolve_instrument(conn: &mut SqliteConnection, iid: i32, winning_side: i32) -> Result<(), Error> {
    conn.transaction(|conn| {
        use crate::schema::customers::{self, dsl::*};
        use crate::schema::positions::{self, dsl::*};
        // What do I want? I want to take all positions which have a given
        // instrumentid and side, calculate their value as volume*TOTAL_CONTRIB,
        // then update all the customers' balances to their new values

        // Single-query attempt
        // diesel::replace_into(
        diesel::update(customers::table)
            .set(balance.eq(balance - (positions::table
                .select(sum(volume))
                .filter(instrumentid.eq(iid).and(side.eq(winning_side)).and(customer_id.eq(id)))
                .single_value()
                .assume_not_null()
            )));

        // Get the winning positions
        let poses: Vec<Position> = positions::table
            .filter(instrumentid.eq(iid).and(side.eq(winning_side)))
            .select(Position::as_select())
            .load::<Position>(conn)?;
        
        // Update the balances of the customers
        for pos in poses {
            diesel::update(customers::table)
                .filter(id.eq(pos.customer_id))
                .set(balance.eq(balance + (pos.volume*TOTAL_CONTRIB as i32)))
                .execute(conn)?;
        }
        
        // Then delete all positions with this instrumentid
        diesel::delete(positions::table)
            .filter(instrumentid.eq(iid))
            .execute(conn)?;
        Ok(())
    })
}
