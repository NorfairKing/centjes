#let input = json("input.json")

= Taxes #{ input.year }

Name: #{ input.first_name } #{ input.last_name }

== Exchange rates

These exchange rates are used for valuations on #{ input.year }-12-31:

#for (currency, rate) in input.rates [
  - #raw(currency): #{ rate } #raw("CHF") / #raw(currency)
]

== Assets

#for asset in input.assets [
  === #raw(asset.name)

  #if asset.balances.len() == 1 and "CHF" in asset.balances [

    Balance: #{ asset.balance } #raw("CHF")

  ] else [

    Balance:

    #for (currency, balance) in asset.balances [
      - #{ balance } #raw(currency)
    ]

    Converted: #{ asset.balance } #raw("CHF")

  ]

  #for evidence in asset.evidence [
    - #raw(evidence)
  ]
]
