---
title: Set Aside
description: Documentation about the centjes-switzerland set-aside command, for checking whether enough money is set aside for taxes
---

`centjes-switzerland set-aside` compares what is set aside for this year's
taxes with what they will cost, and ends with what to save or pre-pay.

It needs three things from the ledger:

* Revenue is every income account without the `undeclared` tag, the same tag
  that the `taxes` command uses.
  SVA, cantonal and federal taxes each cost a share of it.
* Every asset and liability account with a balance is tagged either `taxable`
  or `not-taxable`, like a third pillar account.
  The Zürich wealth tax is computed from the `taxable` ones, valued at the
  ESTV's rates.
* Reserve accounts hold the money that is set aside, and count with their
  balance.

With these settings in `switzerland.yaml`:

<!-- [check:ref DocsSetAsideInput] -->
``` yaml
set-aside:
  sva:
    rate: 0.12
    reserves:
    - assets:sva
  cantonal:
    rate: 0.08
    reserves:
    - expenses:taxes:cantonal
  federal:
    rate: 0.02
    reserves:
    - assets:federal
  wealth:
    reserves:
    - assets:wealth-tax
```

and this ledger, the report for 2026 looks like this:

``` centjes set-aside
currency CHF 0.01

tag undeclared
tag taxable
tag not-taxable

account equity:opening-balances
account assets:bank
  + tag taxable
account assets:pillar-3a
  + tag not-taxable
account assets:sva
  + tag taxable
account assets:federal
  + tag taxable
account assets:wealth-tax
  + tag taxable
account expenses:taxes:cantonal
account income:clients
account income:gifts
  + tag undeclared

2025-12-31
    | Opening balances
    * assets:bank                   400000 CHF
    * assets:pillar-3a               50000 CHF
    * equity:opening-balances      -450000 CHF

2026-03-31
    | Invoice paid
    * assets:bank                    60000 CHF
    * income:clients                -60000 CHF

2026-05-10
    | Birthday gift, which is not revenue
    * assets:bank                      500 CHF
    * income:gifts                    -500 CHF

2026-06-30
    | Saving for taxes
    * assets:bank                   -10000 CHF
    * assets:sva                      6000 CHF
    * assets:federal                  1000 CHF
    * assets:wealth-tax               1000 CHF
    * expenses:taxes:cantonal         2000 CHF
```
