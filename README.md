This is a prediction market matching engine and a system for tracking positions
(i.e. how much cash every client has and how much of each side of each contract they own).
Currently it's just the implementation of the relevant data structures,
with no frontend or handling for connections or anything.

# Mutually exclusive event contracts
The structures provided in this repository are designed to improve on the handling of contracts on mutually exclusive events,
compared to current prediction markets.

Conventional prediction markets are based on binary options contracts:
Two parties,
a "yes" and a "no",
each put some money into a pot.
If the event happens, the money goes to the "yes" party,
and if it doesn't, it goes to the "no" party.

But there are plenty of events that aren't binary.
For instance, "what will the daily high be in New York city tomorrow?" has as many outcomes as there are temperatures,
and the outcomes are mutually exclusive.
As far as I'm aware, current markets deal with this by creating a binary option for each outcome (or group of outcomes).
e.g. "under 40", "40 - 50", "50 - 60" and "over 60".

This has some problems:
for one, it creates redundancy.
Owning 100 "no" shares of "50 - 60" is the same as owning 100 "yes" shares each of "under 40", "40 - 50" and "over 60",
but with binary options, they're represented differently,
and it's possible for one to be cheaper than the other.

This means you can do arbitrage:
if the asking price for "yes" on all outcomes is less than a dollar,
you can make guaranteed free money by buying "yes" on all sides simultaneously,
since one of the outcomes is guaranteed to happen.
This shouldn't be possible.

It also means that if you own "no" contracts on multiple different outcomes,
you're guaranteed to make at least some money,
but you can't (without additional infrastructure) recover that money or use it for more trading until the market actually resolves.
Kalshi handles this with ["collateral return"](https://kalshi.com/learn/collateral-return):
if you own "no" on multiple outcomes of a mutually exclusive event,
they give you some of your payout in advance.
This creates its own complications:
if you later sell some of your "no" contracts,
you might need to pay some of this collateral back,
because that part of your payout is no longer guaranteed.

In contrast, this prediction market's fundamental objects are mutually exclusive events.
There are as many parties to a contract as there are possible outcomes:
each party pays some money into the pot,
and whoever owns the outcome that happens gets paid.
To buy a "no" contract on an outcome,
you buy "yes" contracts on all other possible options.
You could also buy any other combination of sides:
if you think the temperature is going to be over 50 tomorrow,
you buy "50 - 60" and "over 60",
creating a contract with some other party or parties who are buying "under 40" and "40 - 50" in a single atomic transaction.

To "sell" your side of a contract,
you just buy shares in all the other possible outcomes.
If you own shares in all possible outcomes of an event,
your shares are automatically converted back into cash.
This handles the concept of "collateral return" implicitly.

If there is some contract with outcomes A, B, C and D,
and you buy 100 shares of "NO A" and 200 shares of "NO B",
what you've actually bought is
- 100 shares each of "YES B", "YES C" and "YES D"
- 200 shares each of "YES A", "YES C" and "YES D"

So you now have 200 shares of A, 100 shares of B, 300 shares of C and 300 shares of D.

100 of each of these shares cancel each other out,
leaving you with 100 A, 0 B, 200 C and 200 D and $100 in cash.
In this system, you actually can't sell your NO A anymore.
That would require buying shares of A,
which would cancel out your B, C and D shares,
but your B shares have already all been cancelled out!

# Matching engines
This repository contains two types of matching engines:
- A standard order book matching engine (mostly functional but undertested)
- A periodic auction system in the style of OneChronos (not really functional)

# Order book engine
## Performance
The book for a contract is stored as a collection of B-trees, one for each outcome.
This gives O(logk) insertions and cancellations.

Matching and automatically converting outcomes back into cash are both handled with priority-queues.

To buy shares in an outcome,
you specify the outcomes you want to buy,
the maximum volume you're willing to buy,
and the maximum percentage of each share you're willing to contribute.
The matching engine retrieves limit orders for every other outcome from the book.

Matching is done (I think) in O(n*(logk+logn) + mlogn) time, where
- n is the number of other outcomes for the event (outcomes other than the ones you're buying)
- m is the number of orders in the book involved in the transaction
- k is the maximum number of orders in the book for any other outcome

Updating a customer's position
(storing the fact that they own some shares in an outcome)
and crediting them for any money they need to be refunded
(if they own shares in all possible outcomes)
is done in O(logn) time where n is the number of outcomes for the event.
Note that if someone buys "no" in some outcome,
they actually bought "yes" in every other outcome,
so updating their position takes O(nlogn) time.

So matching gets more expensive the more outcomes there are to a contract,
but not hideously more expensive.

I have not done any performance benchmarks.

## Limitations
Currently, the only orders that can be put in the book are orders for a single outcome.
You _can_ atomically make purchases like "buy outcomes 2, 3 and 5 in this event for no more than 40 cents"
but only as an immediate-or-cancel order.

In particular, this means you can't book "no" orders on an outcome.
What I really want is to be able to book orders for any subset of outcomes,
but this is in general a hard problem,
at least as hard as [exact cover](https://en.wikipedia.org/wiki/Exact_cover),
which is NP-complete.
I'm considering adding no orders as a special case,
or looking into supporting any collection of subsets as special cases and precomputing the exact cover solution.

You can make fill-or-kill orders, but the book doesn't support all-or-nothing orders.
AON orders create a lot of complications and they strike me as somewhat unprincipled in a prediction market anyway,
so I have no plans to add support.

# Auction engine
The auction engine is not really functional and has fewer features than the order book engine.
Currently, it only supports orders for a single outcome.

Resolving an auction is pretty simple:
1. Sort the orders for each outcome in descending order of contribution (how much they're willing to contribute to the overall pot for the contract)
2. Build a contribution/volume graph for each outcome: that is, a map between a number of shares and the maximum contribution that the marginal order is willing to make
3. Combine these graphs for all outcomes
4. Find the largest volume for which the total marginal contribution is greater than 1
5. Walk through each of the sorted lists of orders, filling orders until reaching this volume

The runtime of these steps is
1. O(klogk) for each outcome where k is the number of orders for that outcome. Overall this is O(mlogk) where m is the total number of orders and k is the maximum number of orders for any outcome
2. O(k) for each outcome. Overall O(m).
3. O(mlogn) in theory using merge sort, where n is the number of outcomes, but currently O(mlogm) by just concatenating the lists and resorting
4. O(logm) with binary search
5. O(k) for each outcome, O(m) overall.

So the overall performance is O(mlogm), but could be improved to O(mlogk).
But one of the biggest advantages of an auction system is that it allows for more expressive constraints and order types,
since the system can process all the information at once and doesn't need to resolve orders in real time.
