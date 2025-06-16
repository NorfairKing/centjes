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

== Deductions

=== Third pillar

#table(
  stroke: 0.5pt, columns: (auto, 1fr, auto), align: (left, left, right), ..input.third_pillar_contributions.map(expense =>
  (expense.day, [
    #{ expense.description }
    #for evidence in expense.evidence [
      #linebreak()
      #link(evidence, evidence)
    ]
  ], [ #{ expense.amount_chf } CHF ],)).flatten(), text(weight: "bold", [Total]), [], [#text(weight: "bold", input.total_third_pillar_contributions) CHF],
)

=== Self-employment

==== Expenses

===== Homeoffice

#table(
  stroke: 0.5pt, columns: (auto, 1fr, auto, auto), align: (left, left, right, right), ..input.homeoffice_expenses.map(
    expense =>
    (
      expense.day, [ #{ expense.description }
        #linebreak()
        #if expense.evidence.len() == 1 [
          #for evidence in expense.evidence [
            #link(evidence, evidence)
          ]
        ] else [
          #for evidence in expense.evidence [
            - #link(evidence, evidence)
          ]
        ] ], [ #{ expense.amount.formatted } #{ expense.amount.symbol } ], [ #{ expense.amount_chf } CHF ],
    ),
  ).flatten(), text(weight: "bold", [Total]), [], [], [#text(weight: "bold", input.total_homeoffice_expenses) CHF],
)

===== Electricity

#table(
  stroke: 0.5pt, columns: (auto, 1fr, auto, auto), align: (left, left, right, right), ..input.electricity_expenses.map(
    expense =>
    (
      expense.day, [ #{ expense.description }
        #linebreak()
        #if expense.evidence.len() == 1 [
          #for evidence in expense.evidence [
            #link(evidence, evidence)
          ]
        ] else [
          #for evidence in expense.evidence [
            - #link(evidence, evidence)
          ]
        ] ], [ #{ expense.amount.formatted } #{ expense.amount.symbol } ], [ #{ expense.amount_chf } CHF ],
    ),
  ).flatten(), text(weight: "bold", [Total]), [], [], [#text(weight: "bold", input.total_electricity_expenses) CHF],
)

===== Internet

#table(
  stroke: 0.5pt, columns: (auto, 1fr, auto, auto), align: (left, left, right, right), ..input.internet_expenses.map(
    expense =>
    (
      expense.day, [ #{ expense.description }
        #linebreak()
        #if expense.evidence.len() == 1 [
          #for evidence in expense.evidence [
            #link(evidence, evidence)
          ]
        ] else [
          #for evidence in expense.evidence [
            - #link(evidence, evidence)
          ]
        ] ], [ #{ expense.amount.formatted } #{ expense.amount.symbol } ], [ #{ expense.amount_chf } CHF ],
    ),
  ).flatten(), text(weight: "bold", [Total]), [], [], [#text(weight: "bold", input.total_internet_expenses) CHF],
)

===== Phone

#table(
  stroke: 0.5pt, columns: (auto, 1fr, auto, auto), align: (left, left, right, right), ..input.phone_expenses.map(
    expense =>
    (
      expense.day, [ #{ expense.description }
        #linebreak()
        #if expense.evidence.len() == 1 [
          #for evidence in expense.evidence [
            #link(evidence, evidence)
          ]
        ] else [
          #for evidence in expense.evidence [
            - #link(evidence, evidence)
          ]
        ] ], [ #{ expense.amount.formatted } #{ expense.amount.symbol } ], [ #{ expense.amount_chf } CHF ],
    ),
  ).flatten(), text(weight: "bold", [Total]), [], [], [#text(weight: "bold", input.total_phone_expenses) CHF],
)

===== Travel

#table(
  stroke: 0.5pt, columns: (auto, 1fr, auto, auto), align: (left, left, right, right), ..input.travel_expenses.map(
    expense =>
    (
      expense.day, [ #{ expense.description }
        #linebreak()
        #if expense.evidence.len() == 1 [
          #for evidence in expense.evidence [
            #link(evidence, evidence)
          ]
        ] else [
          #for evidence in expense.evidence [
            - #link(evidence, evidence)
          ]
        ] ], [ #{ expense.amount.formatted } #{ expense.amount.symbol } ], [ #{ expense.amount_chf } CHF ],
    ),
  ).flatten(), text(weight: "bold", [Total]), [], [], [#text(weight: "bold", input.total_travel_expenses) CHF],
)

=== Health insurance and doctor costs

Health insurance costs are declared using the "tax extract" document from the
health insurance company.

These are the uninsured doctor and dentist costs, which need to be declared
separately.

#table(
  stroke: 0.5pt, columns: (auto, 1fr, auto, auto), align: (left, left, right, right), ..input.health_expenses.map(
    expense =>
    (
      expense.day, [ #{ expense.description }
        #linebreak()
        #if expense.evidence.len() == 1 [
          #for evidence in expense.evidence [
            #link(evidence, evidence)
          ]
        ] else [
          #for evidence in expense.evidence [
            - #link(evidence, evidence)
          ]
        ] ], [ #{ expense.amount.formatted } #{ expense.amount.symbol } ], [ #{ expense.amount_chf } CHF ],
    ),
  ).flatten(), text(weight: "bold", [Total]), [], [], [#text(weight: "bold", input.total_health_expenses) CHF],
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

These exchange rates are used for asset valuations on #datetime(year: input.year, month: 12, day: 31).display()

#table(
  stroke: 0.5pt, columns: (auto, auto), align: (left, right), ..(input.rates.pairs().map(((currency, rate)) =>
  (currency, [#{ rate } CHF])).flatten()),
)
