## Added

* Comments within a declaration, on indented lines of their own.
  A comment belongs to the line below it, so it cannot be the last line of a
  declaration, and the formatter leaves it where you put it.
  Consecutive comment lines are one comment.

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

## Changed

* A description can no longer be empty, nor contain an empty line, because
  neither can be written down.

## Fixed

* A comment whose text was only whitespace used to swallow the declaration
  below it, so `centjes format` deleted that declaration.

* An assertion naming an account that was never declared used to pass silently.
  It is now `CE_UNDECLARED_ACCOUNT`, like every other name in the language.
