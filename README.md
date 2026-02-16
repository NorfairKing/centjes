# Centjes

Plaintext double-entry accounting for the paranoid.

Status: Does all my tax returns.

## Goals

* Make it easy to file tax declarations.
* Automate as much as possible.
* Make it _very_ hard to make mistakes.

## Pipeline


External Providers
  --[ Import ]-> Raw Data
  --[ Cleaning and Standardising ]-> Baked data (`.cent`)
  --[ Query ]-> Reports

* Raw data is not committed
* Baked data is committed, as declaratively as pragmatic


Assumptions:
* Raw data is going to be in shitty condition.
  For example:
  1. Not UTF-8
  1. Csv but not separated by commas
  1. Raw PDF files
* Raw data will not be correct.
  We will need a mechanism to correct it, and not forget about the correction.
  Banks and Brokers are notoriously terrible at getting currency conversions correct.


## FAQ

* Q: Why do I need to declare accounts?
  A: To help avoid typos

* Q: Why do I need to declare currencies?
  A: To help avoid typos, and so that `centjes` can figure out the right precision to use in calculations.

