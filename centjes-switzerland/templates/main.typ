#let input = json("input.json")

= Taxes 2024


Name: #{input.name}

== Income


#table(
  columns: (auto, auto, auto,auto),
  ..("Day", "Amount", "Currency", "File").map(h => text(h, weight: "bold")),
  ..input.income.map(income => (
      income.day,
      income.amount.formatted,
      income.amount.symbol,
      raw(income.evidence)
      )).flatten()
)

== Appendix: Raw input

#{input}
