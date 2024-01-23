#let input = json("input.json")

= VAT #{ upper(input.quarter) }

Name: #{ input.name }

== Umsatz

Alle Umsatzangaben sind netto

=== Entgelte

#table(
  columns: (auto, 1fr, auto, auto),
  ..("", "", "Umsatz CHF", "Umsatz CHF").map(h => text(h, weight: "bold")),
  "200",
  "Total der vereinbarten bzw. vereinnahmten Entgelte, inkl. optierte Leistungen, Entgelte aus Übertragungen im Meldeverfahren sowie aus Leistungen im Ausland (weltweiter Umsatz)",
  "",
  input.total_revenue.formatted,
)

=== Abzüge

#table(
  columns: (auto, 1fr, auto, auto),
  ..("", "", "Umsatz CHF", "Umsatz CHF").map(h => text(h, weight: "bold")),
  "221",
  "Leistungen im Ausland (Ort der Leistung im Ausland)",
  input.foreign_revenue.formatted,
  "",
  "",
  "",
  "",
  "",
  "289",
  "Total Abzüge Ziffer 220 bis 280",
  "",
  input.foreign_revenue.formatted,
)

=== Steuerbarer Gesamtumsatz

#table(
  columns: (auto, 1fr, auto, auto),
  ..("", "", "Umsatz CHF", "Umsatz CHF").map(h => text(h, weight: "bold")),
  "299",
  "Steuerbarer Gesamtumsatz (Ziff. 200 abzüglich Ziff. 289)",
  "",
  input.domestic_revenue.formatted,
)

== Steuerberechnung
=== Leistungen ab 01.01.2018
=== Total geschuldete Steuer
=== Steueranrechnung
=== Zu bezahlender Betrag / Guthaben
