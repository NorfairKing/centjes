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

#page(numbering: none)[
  #v(1fr)
  #align(center)[
    #text(size: 28pt, weight: "bold")[Tax Packet #{ input.year }]

    #v(1em)

    #text(size: 16pt)[#{ input.first_name } #input.last_name]

    #v(2em)

    #text(size: 12pt)[
      Attachments for the tax declaration \
      for the year #{ input.year }
    ]
  ]
  #v(1fr)
]

#pagebreak()
= Children (Kinder)

== Daycare costs (Kinderbetreuungskosten)

#if input.children_costs.daycare.len() > 0 {
  amount_table(
    input.children_costs.daycare,
    input.children_costs.total_daycare,
  )
} else [
  No daycare costs.
]

#pagebreak()
= Depreciation (Abschreibungen)

#let depreciation_schedule(title, schedule) = {
  [== #title]

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

#depreciation_schedule("Movables (Mobilien)", input.movables)

#depreciation_schedule("Machinery (Maschinen / Werkzeuge)", input.machinery)

#pagebreak()

= Income (Einkünfte)

All income is reported in CHF, using the exchange of the day of the transaction.

#amount_table(input.revenues, input.total_revenues)

#pagebreak()

= Deductions (Abzüge)

== Self-employment expenses (Selbständige Erwerbstätigkeit)

#let expense_section(title, partitioned) = {
  [=== #title]

  table(
    stroke: 0.5pt,
    columns: (1fr, 1fr, 1fr),
    align: (right, right, right),
    text(weight: "bold", [Total (Betrag)]),
    text(weight: "bold", [Private (Privatanteil)]),
    text(weight: "bold", [Business (Geschäftsanteil)]),

    [#partitioned.total_expenses CHF],
    [#partitioned.total_private_expenses CHF],
    [#partitioned.total_business_expenses CHF],
  )

  if partitioned.business_expenses.len() > 0 {
    [==== Business expenses (Geschäftsaufwand)]
    amount_table(
      partitioned.business_expenses,
      partitioned.total_business_expenses,
    )
  }

  if partitioned.private_expenses.len() > 0 {
    [==== Private expenses (Privataufwand)]
    amount_table(
      partitioned.private_expenses,
      partitioned.total_private_expenses,
    )
  }
}

#expense_section("Rent (Miete)", input.homeoffice_expenses)

#expense_section("Phone (Telefon)", input.phone_expenses)

#expense_section("Travel (Reisen)", input.travel_expenses)

#expense_section("Internet (Internet)", input.internet_expenses)

#expense_section("Electricity (Strom)", input.electricity_expenses)

#expense_section("Insurance (Versicherungen)", input.insurance_expenses)

== Third pillar (Säule 3a)

These are declared according to the "tax extract" documents from the
third-pillar providers.
Here they are listed again, but the total may differ depending on the exact dates of
the contributions.

#amount_table(
  input.third_pillar_contributions,
  input.total_third_pillar_contributions,
)

== Health insurance and medical costs (Krankheits- und Unfallkosten)

=== Health insurance premiums (Krankenkassenprämien)

These are described in the document from the health insurance company.

#amount_table(
  input.health_costs.insurance_premiums,
  input.health_costs.total_insurance_premiums,
)

#pagebreak()
=== Illness and accident costs (Krankheits- und Unfallkosten)

These are described in the document from the health insurance company.

==== Further costs (Weitere Kosten)

#if input.health_costs.dentist.len() > 0 {
  [
    ===== Dentist costs (Zahnarztkosten)
    Dentist costs
  ]
  amount_table(input.health_costs.dentist, input.health_costs.total_dentist)
}

#if input.health_costs.doctor.len() > 0 {
  [
    ===== Doctor and prescriptions (Arzt und Medikamente)
    Doctors and doctor-prescribed medication
  ]
  amount_table(input.health_costs.doctor, input.health_costs.total_doctor)
}

#if input.health_costs.hospital.len() > 0 {
  [
    ===== Hospital stays (Spitalaufenthalte)
    Hospital stays
  ]
  amount_table(input.health_costs.hospital, input.health_costs.total_hospital)
}

#if input.health_costs.therapy.len() > 0 {
  [
    ===== Therapies (Therapien)
    Doctor-prescribed therapies
  ]
  amount_table(input.health_costs.therapy, input.health_costs.total_therapy)
}

#if input.health_costs.other.len() > 0 {
  [
    ===== Other health expenses (Andere Gesundheitskosten)
    Other health expenses that are not part of the other categories
  ]
  amount_table(input.health_costs.other, input.health_costs.total_other)
}

#pagebreak()
= Assets (Vermögen)

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
  == #asset.name

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

= Exchange rates (Wechselkurse)

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
