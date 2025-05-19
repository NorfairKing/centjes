---
title: Format
description: Documentation about the centjes rates-graph command, for graphing exchange rates.
---

This command graphs the exchange rates that are defined by the `price`s in your ledger.

The output is a [Graphviz](https://graphviz.org/) file that you can render with `dot` like this:

``` plain
dot -Tsvg currency-rates.dot -ocurrency-rates.svg'
```


