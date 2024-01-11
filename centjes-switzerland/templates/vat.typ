#let input = json("input.json")

= VAT #{ upper(input.quarter) }

Name: #{ input.name }

#pagebreak()
== Income

#table(
  columns: (auto, 1fr, auto, auto, auto, auto),
  ..("Day", "Description", "Amount", "VAT", "CHF", "File").map(h => text(h, weight: "bold")),
  ..input.income.map(
    income => (
      income.day,
      income.description,
      [ #{ income.amount.formatted } #{ income.amount.symbol } ],
      if income.vat == none [ ] else [ #{ income.vat.formatted } #{ income.vat.symbol } ],
      [ #{ income.chf.formatted } #{ income.chf.symbol } ],
      raw(income.evidence),
    ),
  ).flatten(),
)
