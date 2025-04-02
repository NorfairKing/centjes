---
title: Syntax
description: The Centjes DSL Syntax
---


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

## Account types

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

## Account extras

### Account attachments

An account can have attachments.
You can use this to attach bank statements during the given tax period, for example.

``` centjes
account assets:bank
    + attach statement.pdf
```

These attachments are checked with `centjes check` and can be used for automation, such as in `centjes-switzterland`.


### Account assertions

An account can have associated assertions.

#### Currency assertion

An account currency assertion says that the money in the given account must be of a given currency.
It is declared as follows:

``` centjes
account assets:bank
  + assert currency USD
```

### Tags

Accounts can be tagged:

``` centjes
account assets
    + tag legal
```

However, tags must be declared first, too.

# Transaction

Transactions are declared as using a list of postings that have to balance to zero.

A transaction starts with a timestamp and description:

``` centjes
2025-01-27
    | This is an example timestamp with no postings.
```

We add postings to it by describing a change to an account for each account:

``` centjes
2025-01-27
    | I bought a coffee.
    * assets:cash -5.00 USD
    * expenses:coffee 5.00 USD
```

This example describes that I bought coffee with cash on `2025-01-27`.

## Virtual postings

Postings starting with `*` are considered "real" postings.
Transactions can also contain virtual postings, which start with `!`.

``` centjes
2025-01-27
    | I bought a coffee.
    * assets:cash -5.00 USD
    * expenses:coffee 5.00 USD
    ! expenses:vat 0.50 USD
```

Virtual postings are ignored when checking if the transaction balances.

## Currency exchange

When accounting for transactions involving multiple currencies, postings need a conversion factor to balance.
You can add those with an `@` sign.

``` centjes
2025-01-27
    | Exchange USD for CHF
    * assets:bank -5.00 USD @ 1.25 CHF
    * assets:bank  6.25 CHF
```

You can also describe these conversion factors with a rational fraction in case it cannot be expressed as a finite decimal number:

``` centjes
2025-01-27
    | Exchange USD for CHF
    * assets:bank -7.00 USD @ 1 / 7 CHF
    * assets:bank  1.00 CHF
```

## Percentage

Postings can have an annotated percentage to express that they represent a given fraction of the previous percentage.

``` centjes
2025-01-27
    | I bought a coffee.
    * assets:cash -5.50 USD
    * expenses:coffee 5.50 USD
    ! expenses:vat 0.50 USD ~ 10%
```

These percentages can be calculated in different ways:

* `~` or `~i`:
  The amount is an inclusive percentage of the above posting. 
  For example: `0.50` is `10%` of an amount (`5.00`) that, together with the `0.50` sum to `5.50`
* `~e`: The amount is an exclusive percentage of the above posting.
  For example: `0.50` is `10%` of `5.00`.

``` centjes
2025-01-27
    | I bought a coffee.
    * assets:cash -5.00 USD
    * expenses:coffee 5.00 USD
    ! expenses:vat 0.50 USD ~e 10%
```


## Transaction extras

Transactions can be annotated with extra metadata.

### Attachments

You can attach files to a transactions.

``` centjes
2025-01-27
    | I bought a coffee.
    * assets:cash -5.00 USD
    * expenses:coffee 5.00 USD
    + attach receipt.pdf
```

These attachments are checked with `centjes check` and can be used for automation, such as in `centjes-switzterland`.

### Assertions

You can attach assertions to transactions.
These assertions are checked after balancing the transaction.

``` centjes
2025-01-27
    | I bought a coffee, now I'm broke.
    * assets:cash -5.00 USD
    * expenses:coffee 5.00 USD
    + assert assets:cash = 0.00 USD
```

### Tags

Transactions can be tagged:

``` centjes
2025-01-27
    | I bought a coffee, now I'm broke.
    * assets:cash -5.00 USD
    * expenses:coffee 5.00 USD
    + tag not-tax-deductable
```

However, tags must be declared first, too.

# Tag

Tags can be declared like so:

``` centjes
tag tax-deductable
```

# Price

We can declare the price of a currency expressed in another currency on a given day.

``` centjes
price 2023-12-30 USD 0.85133 CHF
```
