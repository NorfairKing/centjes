#set text(font: "DejaVu Sans Mono", size: 8pt)

#let input = json("input.json")

#set document(title: [Tax Packet #{ input.year }], date: none)

#let amount_table(lines, total) = {
  // Determine if third column is needed
  let line_amount = (
    line => line.at(
      "amount",
      default: (symbol: "CHF", formatted: line.amount_chf),
    )
  )
  let is_chf = line => (
    line_amount(line).symbol == "CHF" and line_amount(line).formatted == line.amount_chf
  )
  let show_original = lines.any(line => not is_chf(line))

  let columns = if show_original {
    (auto, 1fr, auto, auto)
  } else {
    (auto, 1fr, auto)
  }

  let aligns = if show_original {
    (left, left, right, right)
  } else {
    (left, left, right)
  }

  let rows = lines
    .map(
      line => {
        let common = (
          line.day,
          [ #line.description
            #for evidence in line.evidence [
              #linebreak()
              #link(evidence, evidence)
            ]
          ],
        )

        if show_original {
          (
            ..common,
            if is_chf(line) [] else [
              #line_amount(line).formatted
              #line_amount(line).symbol
            ],
            [#line.amount_chf CHF],
          )
        } else {
          (
            ..common,
            [#line.amount_chf CHF],
          )
        }
      },
    )
    .flatten()

  let last_row = if show_original {
    (
      text(weight: "bold", [Total]),
      [],
      [],
      [#text(weight: "bold", total) CHF],
    )
  } else {
    (text(weight: "bold", [Total]), [], [#text(weight: "bold", total) CHF])
  }

  table(stroke: 0.5pt, columns: columns, align: aligns, ..rows, ..last_row)
}

= Taxes #{ input.year }

This document is the index of all the attachment for the tax declaration of #{ input.first_name } #input.last_name for
the year #{ input.year }.

== Income

All income is reported in CHF, using the exchange of the day of the transaction.

#amount_table(input.revenues, input.total_revenues)

== Deductions

=== Third pillar

#amount_table(
  input.third_pillar_contributions,
  input.total_third_pillar_contributions,
)

=== Self-employment expenses

==== Homeoffice

#amount_table(input.homeoffice_expenses, input.total_homeoffice_expenses)

==== Electricity

#amount_table(input.electricity_expenses, input.total_electricity_expenses)

==== Internet

#amount_table(input.internet_expenses, input.total_internet_expenses)

==== Phone

#amount_table(input.phone_expenses, input.total_phone_expenses)

==== Travel

#amount_table(input.travel_expenses, input.total_travel_expenses)

=== Health insurance and doctor costs

Health insurance costs are declared using the "tax extract" document from the
health insurance company.

These are the uninsured doctor and dentist costs, which need to be declared
separately.

#amount_table(input.health_expenses, input.total_health_expenses)

== Assets

#table(
  stroke: 0.5pt, columns: (1fr, auto), align: (
    left,
    right,
  ), ..input.assets.map(asset => (
    asset.name,
    [ #asset.balance CHF ],
  )).flatten(), text(
    weight: "bold",
    [Total],
  ), [#text(weight: "bold", input.total_assets) CHF],
)

#for asset in input.assets [
  === #asset.name

  #if asset.balances.len() == 1 and "CHF" in asset.balances [

    Balance: #asset.balance CHF

  ] else [

    Balances:

    #table(
      stroke: 0.5pt, columns: (auto, auto, auto), align: (
        right,
        left,
        right,
      ), ..(
        asset.balances.pairs().map(((currency, balance)) => (
          [ #balance.original ],
          [ #currency],
          [ #balance.converted CHF ],
        )).flatten()
      ), ..(
        [Total: ],
        [],
        [#asset.balance CHF],
      ),
    )

  ]

  #for evidence in asset.evidence [
    - #link(evidence, evidence)
  ]
]

== Exchange rates

These exchange rates are used for asset valuations on #datetime(year: input.year, month: 12, day: 31).display()

#table(
  stroke: 0.5pt, columns: (auto, auto), align: (left, right), ..(
    input.rates.pairs().map(((currency, rate)) => (
      currency,
      [#rate CHF],
    )).flatten()
  ),
)
