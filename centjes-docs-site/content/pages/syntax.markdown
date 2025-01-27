---
title: Syntax
description: The Centjes DSL Syntax
---

# Introduction

The centjes DSL is used to describe transactions of money accross accounts.

The syntax is machine-readable, and auto-formattable.

# Modules

Centjes data is housed in files with the `.cent` extension called modules.
    
Modules consist of `import`s and declarations.

The main centjes file is called `ledger.cent` and is considered the entry-point for centjes' automation.

# Imports

Imports look like this:

``` centjes
import banks/ubs
```

This import will import all the declarations of the module at `./banks/ubs.cent` (relative to the current module) into the current module.


# Declarations

# Comments

Comments in centjes look like this:

``` centjes
-- This is a comment
```

There are only line comments, no block comments, and they can only appear as standalone declarations.


# Currency declaration

In order to account for money of a certain currency, you need to declare that currency up front.
This helps prevent typos in currencies.

A currency is declared like so:

``` centjes
currency USD 0.01
```

This declaration says "There is a currency with symbol `USD` that has a smallest valid amount of `0.01` units.".

# Account

In order to declare how money moved between accounts, we first have to declare accounts as well, just lik we have to with currencies.

``` centjes
account assets:bank
```

This declaration says "There is an account called `assets:bank`.".

# Account types

An account must be one of the following types.

Each type has an associated assertion on the contents of the account.

* `assets`: never strictly negative
* `liabilities`: never strictly positive
* `equity`: never strictly positive
* `expenses`: never strictly negative
* `income`: never strictly positive
* `other`: no assertions

Centjes automatically guesses the type of an account using a substring search.

For example, an account name `assets:bank` will imply that the account type is `assets`.

To annotate an account with a type manually, you can add it like so:

``` centjes
account foo:bar assets
```

# Account assertions

An account can have associated assertions.

# Currency assertion

An account currency assertion says that the money in the given account must be of a given currency.
It is declared as follows:

``` centjes
account assets:bank
    + currency USD
```

# Transaction

Transactions are declared as using a list of postings that have to balance to zero.

TODO.

# Tag

Transactions can have associated tags.

TODO

# Price

We can declare the price of a currency expressed in another currency on a given day.

TODO
