---
title: Import
description: Documentation about the centjes-stripe import command
---

Adds every month that is ready and not already in the output file.
Running it twice is safe: months already there are left exactly as they stand,
so hand edits survive.

Before the first run, add an `import` of the output file to the ledger and
declare the accounts it posts to.
With the defaults that is:

``` plain
account assets:stripe
  + assert currency CHF

account expenses:banking:stripe
```

plus `expenses:refunds` and the payout account once a month needs them.

Then, each month, save Stripe's tax invoice into the documents directory under
the name `fees-attachment` gives, run the importer, and get `centjes check` to
pass.
That invoice is the only file to fetch by hand; the reports a month is computed
from are saved into the `reports` directory and attached as its evidence.

A month is held back until Stripe has finished computing it and its tax invoice
is on disk, and so is every month after it.
The run says which months and why.
