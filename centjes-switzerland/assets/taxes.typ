#let input = json("input.json")

= Taxes #{ input.year }

Name: #{ input.first_name } #{ input.last_name }

Self employment revenue: #{ input.self_employment_revenue }

#pagebreak()
== Income

#for revenue in input.revenues [
  === #{ revenue.description }

  Day: #{ revenue.day }

  #if revenue.gross_amount.symbol == "CHF" [
    Gross Amount: #{ revenue.gross_amount.formatted } #{ revenue.gross_amount.symbol }
  ] else [
    Gross Amount: #{ revenue.gross_amount.formatted } #{ revenue.gross_amount.symbol }: #{ revenue.gross_amount_chf } CHF
  ]

  #if revenue.keys().contains("vat_amount") [
    #if revenue.gross_amount.symbol == "CHF" [
      VAT: #{ revenue.vat_amount.formatted } #{ revenue.vat_amount.symbol }
    ] else [
      VAT: #{ revenue.vat_amount.formatted } #{ revenue.vat_amount.symbol }: #{ revenue.vat_amount_chf } CHF
    ]
  ]

  Netto Amount: #{ revenue.netto_amount } CHF

  #for evidence in revenue.evidence [
    - #{ raw(evidence) }
  ]
]
