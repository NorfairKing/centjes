## Added

* Lot prices: `lot @ <rate> <currency>` on a posting or an assertion.
  An asset held at a lot price is its own commodity, so buying the same symbol
  at two prices gives two balances instead of one.
  It balances at its lot rate the way `@` does, and it is valued as whatever
  the underlying commodity is valued at.
  See the syntax page for details.

  A currency can no longer be called `lot`.

* `+ assert virtual`, which asserts the balance that includes virtual postings.
  Plain `+ assert` still counts only real postings, so what an assertion means
  does not depend on `--virtual`.
  An account declared `virtual-only` had no assertable balance at all before
  this.

## Fixed

* An assertion naming an account that was never declared used to pass silently.
  It is now `CE_UNDECLARED_ACCOUNT`, like every other name in the language.
