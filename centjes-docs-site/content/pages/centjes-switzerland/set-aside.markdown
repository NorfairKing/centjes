---
title: Set Aside
description: Documentation about the centjes-switzerland set-aside command, for checking whether enough money is set aside for taxes
---

`centjes-switzerland set-aside` compares what is set aside for this year's
taxes with what they will cost, and ends with what to save or pre-pay.

It computes the year's taxes report first, with the settings of the `taxes`
command, so it fails wherever the tax declaration would.
It needs these things from the ledger:

* Revenue is what the taxes report declares: every income account without the
  `undeclared` tag.
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
first-name: John
last-name: Doe
ahv-id: '7561111222233'
third-pillar-assets-account: assets:pillar-3a

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
tag deductible
tag not-deductible

account equity:opening-balances
account assets:bank
  + attach bank-statement.pdf
  + tag taxable
account assets:pillar-3a
  + attach pillar-3a-statement.pdf
  + tag not-taxable
account assets:sva
  + attach bank-statement.pdf
  + tag taxable
account assets:federal
  + attach bank-statement.pdf
  + tag taxable
account assets:wealth-tax
  + attach bank-statement.pdf
  + tag taxable
account assets:movables
  + attach last-declaration.pdf
  + tag taxable
account assets:machinery
  + attach last-declaration.pdf
  + tag taxable
account expenses:rent:homeoffice
account expenses:rent:private
account expenses:taxes:cantonal
account income:clients
account income:gifts
  + tag undeclared

2025-12-31
    | Opening balances
    * assets:bank                   400000 CHF
    * assets:pillar-3a               50000 CHF
    * assets:machinery                3000 CHF
    * assets:movables                    0 CHF
    * equity:opening-balances      -453000 CHF

2026-03-31
    | Invoice paid
    * assets:bank                    60000 CHF
    * income:clients                -60000 CHF
    + attach invoice.pdf

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
    + tag not-deductible

2026-07-31
    | Rent, a quarter of which is for the home office
    * assets:bank                    -4800 CHF
    * expenses:rent:homeoffice        1200 CHF
    * expenses:rent:private           3600 CHF
    + tag deductible

2026-09-30
    | Third pillar contribution
    * assets:bank                    -5000 CHF
    * assets:pillar-3a                5000 CHF
    + tag deductible
    + attach pillar-3a-receipt.pdf
```

## Third pillar

Someone who is self-employed and not in a pension fund can pay a fifth of
their net earned income into the third pillar, up to a maximum that the BSV
sets each year.
The net earned income is the revenue minus the business expenses and SVA.
The revenue, the business expenses, which include depreciation, and the
contributions are what the taxes report declares.
SVA counts at its set-aside rate, so the limit is only on the safe side when
that rate does not under-estimate SVA.

The section ends with the room that is left after this year's contributions,
and the to-do list says how much to pay in, rounded down to whole hundreds,
or exactly what is left when that is less than a hundred.
