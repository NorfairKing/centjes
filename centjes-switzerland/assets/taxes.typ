#let input = json("input.json")

= Taxes #{ input.year }

Name: #{ input.first_name } #{ input.last_name }

== Income

#table(
  columns: (auto, 1fr, auto, auto), align: (left, left, right, right), ..input.revenues.map(
    revenue =>
    (
      revenue.day, [ #{ revenue.description }
        #linebreak()
        #if revenue.evidence.len() == 1 [
          #for evidence in revenue.evidence [
            #link(evidence, raw(evidence))
          ]
        ] else [
          #for evidence in revenue.evidence [
            - #link(evidence, raw(evidence))
          ]
        ] ], [#{ revenue.amount.formatted } #{ revenue.amount.symbol }], [#{ revenue.amount_chf } CHF],
    ),
  ).flatten(), [], text(weight: "bold", [Total]), [], [#text(weight: "bold", input.total_revenues) CHF],
)

== Assets

#table(
  columns: (auto, auto), align: (left, right), ..input.assets.map(asset =>
  (raw(asset.name), [ #{ asset.balance } CHF ])).flatten(), text(weight: "bold", [Total]), [#text(weight: "bold", input.total_assets) CHF],
)

#for asset in input.assets [
  === #raw(asset.name)

  #if asset.balances.len() == 1 and "CHF" in asset.balances [

    Balance: #{ asset.balance } #raw("CHF")

  ] else [

    Balances:

    #table(
      columns: (auto, auto, auto), align: (left, right, right), ..(
        asset.balances.pairs().map(((currency, balance)) =>
        (raw(currency), balance.original, [ #{ balance.converted } CHF ],)).flatten()
      ), ..([], [Total: ], [#{ asset.balance } CHF]),
    )

  ]

  #for evidence in asset.evidence [
    - #link(evidence, raw(evidence))
  ]
]

== Exchange rates

These exchange rates are used for valuations on #datetime(year: input.year, month: 12, day: 31).display()

#table(
  columns: (auto, auto), align: (left, right), ..(input.rates.pairs().map(((currency, rate)) =>
  (raw(currency), [#{ rate } CHF])).flatten()),
)
