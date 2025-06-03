#set text(font: "DejaVu Sans Mono", size: 8pt)

#let input = json("input.json")

#set document(title: [Tax Packet #{ input.year }], date: none)

= Taxes #{ input.year }

This document is the index of all the attachment for the tax declaration of #{ input.first_name } #{ input.last_name } for
the year #{ input.year }.

== Income

All income is reported in CHF, using the exchange of the day of the transaction.

#table(
  stroke: 0.5pt, columns: (auto, 1fr, auto, auto), align: (left, left, right, right), ..input.revenues.map(
    revenue =>
    (
      revenue.day, [ #{ revenue.description }
        #linebreak()
        #if revenue.evidence.len() == 1 [
          #for evidence in revenue.evidence [
            #link(evidence, evidence)
          ]
        ] else [
          #for evidence in revenue.evidence [
            - #link(evidence, evidence)
          ]
        ] ], [#{ revenue.amount.formatted } #{ revenue.amount.symbol }], [#{ revenue.amount_chf } CHF],
    ),
  ).flatten(), [], text(weight: "bold", [Total]), [], [#text(weight: "bold", input.total_revenues) CHF],
)

== Assets

#table(
  stroke: 0.5pt, columns: (auto, auto), align: (left, right), ..input.assets.map(asset =>
  (asset.name, [ #{ asset.balance } CHF ])).flatten(), text(weight: "bold", [Total]), [#text(weight: "bold", input.total_assets) CHF],
)

#for asset in input.assets [
  === #{ asset.name }

  #if asset.balances.len() == 1 and "CHF" in asset.balances [

    Balance: #{ asset.balance } CHF

  ] else [

    Balances:

    #table(
      stroke: 0.5pt, columns: (auto, auto, auto), align: (left, right, right), ..(
        asset.balances.pairs().map(((currency, balance)) =>
        (currency, balance.original, [ #{ balance.converted } CHF ],)).flatten()
      ), ..([], [Total: ], [#{ asset.balance } CHF]),
    )

  ]

  #for evidence in asset.evidence [
    - #link(evidence, evidence)
  ]
]

== Exchange rates

These exchange rates are used for valuations on #datetime(year: input.year, month: 12, day: 31).display()

#table(
  stroke: 0.5pt, columns: (auto, auto), align: (left, right), ..(input.rates.pairs().map(((currency, rate)) =>
  (currency, [#{ rate } CHF])).flatten()),
)
