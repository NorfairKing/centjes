---
title: The Centjes importer for Stripe
description: Documentation about the Centjes importer for Stripe
---

`centjes-stripe` books a Stripe account one month at a time, from Stripe's own
monthly reports rather than from individual charges.
Per month it writes the sales split by country with the VAT on the domestic
part, the refunds, Stripe's fees with the VAT on them, and one transaction per
payout.
The last transaction of each month asserts the balance Stripe closed it on.

It needs a restricted key, made in the dashboard under Developers, API keys,
that can read the balance and payouts and can *create* a report run.
That last one is not a mistake: a report has to be asked for before it can be
read, and it is the only write this importer makes.

Two things to know before filing a return from it:

* Revenue that Stripe's tax report has no row for is booked as foreign, because
  Stripe only reports jurisdictions the account is registered in.
  Each run says how much of the month that was.
* Refunds are booked as an expense with the VAT on them as input tax, so
  turnover stays gross of them.
  Each month with refunds is flagged.
