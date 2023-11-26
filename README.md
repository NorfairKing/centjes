# Centjes


A comprehensive personal finances tool.


External Providers
  --[ Import ]-> Raw Data
  --[ Cleaning and Standardising ]-> Baked data
  --[ Query ]-> (Interactive) Reports and Graphs

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


## A ledger language as a compiler target

TODO
