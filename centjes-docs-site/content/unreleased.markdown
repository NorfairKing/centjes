## Added

* Lot prices: `lot @ <rate> <currency>` on a posting or an assertion.
  An asset held at a lot price is its own commodity, so buying the same symbol
  at two prices gives two balances instead of one.
  It balances at its lot rate the way `@` does, and it is valued as whatever
  the underlying commodity is valued at.
  See the syntax page for details.

  A currency can no longer be called `lot`.
