#show raw: set text(font: "DejaVu Sans Mono", size: 8pt)
#set text(font: "DejaVu Sans Mono", size: 9pt)
#set table(stroke: 0.5pt)

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

#let evidence_list = (
  list => for evidence in list [
    #link(evidence, evidence) \
  ]
)

= VAT #upper(input.quarter)

== Overview

Name: #input.person_name

Organisation: #input.organisation_name

VAT-ID: CHF-#input.vat_id MWST

=== Umsatz

Alle Umsatzangaben sind netto

==== Entgelte

#table(
  columns: (auto, 3fr, 1fr, 1fr), align: (x, y) => (
    left,
    left,
    right,
    right,
  ).at(x), ..("", "", "Umsatz CHF", "Umsatz CHF").map(h => text(
    h,
    weight: "bold",
  )), "200", "Total der vereinbarten bzw. vereinnahmten Entgelte, inkl. optierte Leistungen, Entgelte aus Übertragungen im Meldeverfahren sowie aus Leistungen im Ausland (weltweiter Umsatz)", "", input.total_revenue,
)

==== Abzüge

#table(
  columns: (auto, 3fr, 1fr, 1fr), align: (x, y) => (left, left, right, right).at(x), "220", "Leistungen ins Ausland", input.total_exports_revenue, "", "221", "Leistungen im Ausland (Ort der Leistung im Ausland)", input.total_foreign_revenue, "", "225", "Übertragung im Meldeverfahren", "", // TODO
  "", "230", "Von der Steuer ausgenommene Inlandleistungen, für die nicht optiert wird", "", // TODO
  "", "235", "Entgeltsminderungen wie Skonti, Rabatte usw.", "", // TODO
  "", "280", "Diverses (z.B. Wert des Bodens, Ankaufspreise Margenbesteuerung)", "", // TODO
  "", "", "", "", "", "289", "Total Abzüge Ziffer 220 bis 280", "", input.total_foreign_deductions,
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
  "303", "Leistungen zum Normalsatz 8.1%", input.domestic_revenue_2024, input.vat_revenue_standard_2024,
  "313", "Leistungen zum reduzierten Satz 2.6%", "", // TODO
  "", // TODO
  "343", "Leistungen zum Beherbergungssatz Satz 3.8%", "", // TODO
  "", // TODO
  "", "", "", "", "379", "Steuerbarer Gesamtumsatz (wie Ziff. 299)", input.total_domestic_revenue, "", "", "", "", "", "382", "Bezugsteuer (netto, exkl. MWST)", "", // TODO
  "", // TODO
)

==== Total geschuldete Steuer

#table(
  stroke: 0.5pt,
  columns: (auto, 3fr, 1fr, 1fr),
  align: (x, y) => (left, left, right, right).at(x),
  "399",
  "Total geschuldete Steuer (Ziff. 301 bis Ziff. 382)",
  "",
  input.total_vat_revenue,
)

==== Steueranrechnung

#table(
  columns: (auto, 3fr, 1fr, 1fr), align: (x, y) => (left, left, right, right).at(x), ..("", "", "Steuer CHF", "Steuer CHF").map(h => text(h, weight: "bold")), "400", "Vorsteuer auf Material- und Dienstleistungsaufwand", "", // TODO
  "", // TODO
  "405", "Vorsteuer auf Investitionen und übrigem Betriebsaufwand", input.vat_paid, "", "410", "Einlageentsteuerung (Art. 32, bitte detaillierte Aufstellung einreichen)", "", // TODO
  "", // TODO
  "415", "Vorsteuerkorrekturen: gemischte Verwendung (Art. 30), Eigenverbrauch (Art. 31)", "", // TODO
  "", // TODO
  "420", "Vorsteuerkürzungen: Nicht-Entgelte wie Subventionen, Tourismusabgaben usw. (Art. 33 Abs. 2)", "", // TODO
  "", // TODO
  "", "", "", "", "479", "Totale Abzuge", "", input.total_vat_deductions,
)

==== Zu bezahlender Betrag / Guthaben

#table(
  stroke: 0.5pt,
  columns: (auto, 3fr, 1fr, 1fr),
  align: (x, y) => (left, left, right, right).at(x),
  "500", "Zu bezahlender Betrag", "", input.payable,
  "510", "Guthaben der steuerpflichtigen Person", "", input.receivable,
)

=== Andere Mittelflüsse (Art. 18 Abs. 2)

#table(
  columns: (auto, 3fr, 1fr, 1fr), align: (x, y) => (left, left, right, right).at(x), ..("", "", "Betrag CHF", "").map(h => text(h, weight: "bold")), "900", "Subventionen, durch Kurvereine eingenommene Tourismusabgaben, Entsorgungs- und Wasserwerkbeiträge (Bst. a-c)", "", // TODO
  "", // TODO
  "910", "Spenden, Dividenden, Schadenersatz usw. (Bst. d-l)", "", // TODO
  "", // TODO
)

#pagebreak()
== Income

#table(
  columns: (auto, auto, auto, auto, auto), align: (
    left,
    right,
    right,
    right,
    right,
  ),
  table.header(
    "Date",
    "Amount",
    "Amount CHF",
    "VAT",
    "VAT CHF",
  ),
  ..input.revenues.map(revenue => (
    revenue.day,
    [ #revenue.amount.formatted #revenue.amount.symbol ],
    [ #revenue.amount_chf CHF ],
    {
      if "vat_amount" in revenue [
        #revenue.vat_amount.formatted #revenue.vat_amount.symbol
      ]
    },
    {
      if "vat_amount_chf" in revenue [
        #revenue.vat_amount_chf CHF
      ] else [ 0 CHF ]
    },
  )).flatten()
)

#for revenue in input.revenues [
  === #revenue.description

  Day: #revenue.day

  #if revenue.amount.symbol == "CHF" [
    Amount: #revenue.amount.formatted #revenue.amount.symbol
  ] else [
    Amount: #revenue.amount.formatted #revenue.amount.symbol: #revenue.amount_chf CHF
  ]

  #if revenue.keys().contains("vat_amount") [
    #if revenue.amount.symbol == "CHF" [
      VAT: #revenue.vat_amount.formatted #revenue.vat_amount.symbol
    ] else [
      VAT: #revenue.vat_amount.formatted #revenue.vat_amount.symbol: #revenue.vat_amount_chf CHF
    ]
  ]

  #evidence_list(revenue.evidence)
]

#pagebreak()
== Expenses

#table(
  columns: (auto, auto, auto, auto, auto), align: (
    left,
    right,
    right,
    right,
    right,
  ),
  table.header(
    "Date",
    "Amount",
    "Amount CHF",
    "VAT",
    "VAT CHF",
  ),
  ..input.expenses.map(expense => (
    expense.day,
    [ #expense.amount.formatted #expense.amount.symbol ],
    [ #expense.amount_chf CHF ],
    {
      if "vat_amount" in expense [
        #expense.vat_amount.formatted #expense.vat_amount.symbol
      ]
    },
    {
      if "vat_amount_chf" in expense [
        #expense.vat_amount_chf CHF
      ] else [ 0 CHF ]
    },
  )).flatten()
)

#for expense in input.expenses [
  === #expense.description

  Day: #expense.day

  #if expense.amount.symbol == "CHF" [
    Amount: #expense.amount.formatted #expense.amount.symbol
  ] else [
    Amount: #expense.amount.formatted #expense.amount.symbol: #expense.amount_chf CHF
  ]

  #if expense.amount.symbol == "CHF" [
    VAT: #expense.vat_amount.formatted #expense.vat_amount.symbol
  ] else [
    VAT: #expense.vat_amount.formatted #expense.vat_amount.symbol: #expense.vat_amount_chf CHF
  ]

  #evidence_list(expense.evidence)
]
