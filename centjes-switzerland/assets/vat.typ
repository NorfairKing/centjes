#let input = json("input.json")

= VAT #{ upper(input.quarter) }

== Overview

Name: #{ input.person_name }

Organisation: #{ input.organisation_name }

VAT-ID: #raw("CHF-")#raw(input.vat_id)#raw(" MWST")

#set text(size: 10pt)

=== Umsatz

Alle Umsatzangaben sind netto

==== Entgelte

#table(
  columns: (auto, 3fr, 1fr, 1fr),
  align: (x, y) => (left, left, right, right).at(x),
  ..("", "", "Umsatz CHF", "Umsatz CHF").map(h => text(h, weight: "bold")),
  "200",
  "Total der vereinbarten bzw. vereinnahmten Entgelte, inkl. optierte Leistungen, Entgelte aus Übertragungen im Meldeverfahren sowie aus Leistungen im Ausland (weltweiter Umsatz)",
  "",
  input.total_revenue,
)

==== Abzüge

#table(
  columns: (auto, 3fr, 1fr, 1fr),
  align: (x, y) => (left, left, right, right).at(x),
  "220",
  "Leistungen ins Ausland",
  input.total_exports_revenue,
  "",
  "221",
  "Leistungen im Ausland (Ort der Leistung im Ausland)",
  input.total_foreign_revenue,
  "",
  "",
  "",
  "",
  "",
  "289",
  "Total Abzüge Ziffer 220 bis 280",
  "",
  input.total_foreign_deductions,
)

==== Steuerbarer Gesamtumsatz

#table(
  columns: (auto, 3fr, 1fr, 1fr),
  align: (x, y) => (left, left, right, right).at(x),
  "299",
  "Steuerbarer Gesamtumsatz (Ziff. 200 abzüglich Ziff. 289)",
  "",
  input.total_domestic_revenue,
)

=== Steuerberechnung
==== Leistungen ab 01.01.2018

#table(
  columns: (auto, 3fr, 1fr, 1fr),
  align: (x, y) => (left, left, right, right).at(x),
  ..("", "", "Leistungen CHF", "Steuer CHF").map(h => text(h, weight: "bold")),
  "302",
  "Leistungen zum Normalsatz 7.7%",
  input.domestic_revenue_2023,
  input.vat_revenue_standard_2023,
  "303",
  "Leistungen zum Normalsatz 8.1%",
  input.domestic_revenue_2024,
  input.vat_revenue_standard_2024,
)

==== Total geschuldete Steuer

#table(
  columns: (auto, 3fr, 1fr, 1fr),
  align: (x, y) => (left, left, right, right).at(x),
  "399",
  "Total geschuldete Steuer (Ziff. 301 bis Ziff. 382)",
  "",
  input.total_vat_revenue,
)

==== Steueranrechnung

#table(
  columns: (auto, 3fr, 1fr, 1fr),
  align: (x, y) => (left, left, right, right).at(x),
  ..("", "", "Steuer CHF", "Steuer CHF").map(h => text(h, weight: "bold")),
  "405",
  "Vorsteuer auf Investitionen und übrigem Betriebsaufwand",
  "",
  input.vat_paid,
)

==== Zu bezahlender Betrag / Guthaben

#table(
  columns: (auto, 3fr, 1fr, 1fr),
  align: (x, y) => (left, left, right, right).at(x),
  "500",
  "Zu bezahlender Betrag",
  "",
  input.payable,
)

#pagebreak()
== Income

#for revenue in input.revenues [
  === #{ revenue.description }

  Day: #{ revenue.day }

  #if revenue.amount.symbol == "CHF" [
    Amount: #{ revenue.amount.formatted } #{ revenue.amount.symbol }
  ] else [
    Amount: #{ revenue.amount.formatted } #{ revenue.amount.symbol }: #{ revenue.amount_chf } CHF
  ]

  #if revenue.keys().contains("vat_amount") [
    #if revenue.amount.symbol == "CHF" [
      VAT: #{ revenue.vat_amount.formatted } #{ revenue.vat_amount.symbol }
    ] else [
      VAT: #{ revenue.vat_amount.formatted } #{ revenue.vat_amount.symbol }: #{ revenue.vat_amount_chf } CHF
    ]
  ]

  #for evidence in revenue.evidence [
    - #{ raw(evidence) }
  ]
]

#pagebreak()
== Expenses

#for expense in input.expenses [
  === #{ expense.description }

  Day: #{ expense.day }

  #if expense.amount.symbol == "CHF" [
    Amount: #{ expense.amount.formatted } #{ expense.amount.symbol }
  ] else [
    Amount: #{ expense.amount.formatted } #{ expense.amount.symbol }: #{ expense.amount_chf } CHF
  ]

  #if expense.amount.symbol == "CHF" [
    VAT: #{ expense.vat_amount.formatted } #{ expense.vat_amount.symbol }
  ] else [
    VAT: #{ expense.vat_amount.formatted } #{ expense.vat_amount.symbol }: #{ expense.vat_amount_chf } CHF
  ]

  #for evidence in expense.evidence [
    - #{ raw(evidence) }
  ]
]
