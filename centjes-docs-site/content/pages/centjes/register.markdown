---
title: Register report
description: Documentation about the centjes register command, for printing a register overview
---

``` centjes register
currency USD 0.01

account assets:cash
account equity:opening-balances
account expenses:coffee

2025-01-28
    | Opening balances
    * assets:cash 10 USD
    * equity:opening-balances -10 USD

2025-01-29
    | Coffee
    * assets:cash -5 USD
    * expenses:coffee 5 USD
```
