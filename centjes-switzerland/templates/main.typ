#let input = json("input.json")

= Taxes 2024

Name: #{ input.name }

== Income

#table(
  columns: (auto, 1fr, auto, auto, auto),
  ..("Day", "Description", "Amount", "Currency", "File").map(h => text(h, weight: "bold")),
  ..input.income.map(income => (
    income.day,
    income.description,
    income.amount.formatted,
    income.amount.symbol,
    raw(income.evidence),
  )).flatten(),
)

== Appendix: Raw input

#{ input }
