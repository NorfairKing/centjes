#set text(font: "DejaVu Sans Mono", size: 8pt)
#set heading(numbering: "1.1.")

#set page(numbering: (
  (current, total) => align(
    right,
    {
      "Page "
      str(current)
      " of "
      str(total)
    },
  )
))

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
    line_amount(line).symbol == "CHF"
      and line_amount(line).formatted == line.amount_chf
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

#pagebreak()

== Deductions

=== Depreciation

#let depreciation_schedule(title, schedule) = {
  [==== #title]

  table(
    stroke: 0.5pt,
    columns: (1fr, auto),
    align: (left, right),
    [Balance at the end of last year
      #for evidence in schedule.opening_balance_evidence [
        #linebreak()
        #link(evidence, evidence)
      ]
    ],
    [#schedule.opening_balance CHF],

    [Purchases (see below)], [#schedule.total_purchases CHF],
    [Depreciation (#schedule.depreciation_rate)],
    [\-#schedule.depreciation CHF],

    text(weight: "bold", [Balance at the end of the year]),
    [#text(weight: "bold", schedule.closing_balance) CHF],
  )

  if schedule.purchases.len() > 0 {
    amount_table(schedule.purchases, schedule.total_purchases)
  }
}

#depreciation_schedule("Movables", input.movables)

#depreciation_schedule("Machinery", input.machinery)

#pagebreak()

=== Self-employment expenses

#let expense_section(title, partitioned) = {
  [==== #title]

  table(
    stroke: 0.5pt,
    columns: (1fr, 1fr, 1fr),
    align: (right, right, right),
    text(weight: "bold", [Total]),
    text(weight: "bold", [Private]),
    text(weight: "bold", [Business]),

    [#partitioned.total_expenses CHF],
    [#partitioned.total_private_expenses CHF],
    [#partitioned.total_business_expenses CHF],
  )

  if partitioned.business_expenses.len() > 0 {
    [===== Business expenses]
    amount_table(
      partitioned.business_expenses,
      partitioned.total_business_expenses,
    )
  }

  if partitioned.private_expenses.len() > 0 {
    [===== Private expenses]
    amount_table(
      partitioned.private_expenses,
      partitioned.total_private_expenses,
    )
  }
}

#expense_section("Rent", input.homeoffice_expenses)

#expense_section("Phone", input.phone_expenses)

#expense_section("Travel", input.travel_expenses)

#expense_section("Internet", input.internet_expenses)

#expense_section("Electricity", input.electricity_expenses)

#expense_section("Insurance", input.insurance_expenses)

=== Third pillar

These are declared according to the "tax extract" documents from the
third-pillar providers.
Here they are listed again, but the total may differ depending on the exact dates of
the contributions.

#amount_table(
  input.third_pillar_contributions,
  input.total_third_pillar_contributions,
)

=== Health insurance and medical costs

==== Health insurance premiums

These are described in the document from the health insurance company.

#amount_table(
  input.health_costs.insurance_premiums,
  input.health_costs.total_insurance_premiums,
)

#pagebreak()
==== Illness and accident costs

These are described in the document from the health insurance company.

===== Further costs

#if input.health_costs.dentist.len() > 0 {
  [
    ====== Dentist costs
    Dentist costs
  ]
  amount_table(input.health_costs.dentist, input.health_costs.total_dentist)
}

#if input.health_costs.doctor.len() > 0 {
  [
    ====== Doctor and prescriptions
    Doctors and doctor-prescribed medication
  ]
  amount_table(input.health_costs.doctor, input.health_costs.total_doctor)
}

#if input.health_costs.hospital.len() > 0 {
  [
    ====== Hospital stays
    Hospital stays
  ]
  amount_table(input.health_costs.hospital, input.health_costs.total_hospital)
}

#if input.health_costs.therapy.len() > 0 {
  [
    ====== Therapies
    Doctor-prescribed therapies
  ]
  amount_table(input.health_costs.therapy, input.health_costs.total_therapy)
}

#if input.health_costs.other.len() > 0 {
  [
    ====== Other health expenses
    Other health expenses that are not part of the other categories
  ]
  amount_table(input.health_costs.other, input.health_costs.total_other)
}

#pagebreak()
== Assets

#table(
  stroke: 0.5pt,
  columns: (1fr, auto),
  align: (
    left,
    right,
  ),
  ..input
    .assets
    .map(asset => (
      asset.name,
      [ #asset.balance CHF ],
    ))
    .flatten(),
  text(
    weight: "bold",
    [Total],
  ),
  [#text(weight: "bold", input.total_assets) CHF],
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
        asset
          .balances
          .pairs()
          .map(((currency, balance)) => (
            [ #balance.original ],
            [ #currency],
            [ #balance.converted CHF ],
          ))
          .flatten()
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
    input
      .rates
      .pairs()
      .map(((currency, rate)) => (
        currency,
        [#rate CHF],
      ))
      .flatten()
  ),
)
