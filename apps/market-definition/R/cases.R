# Case presets for the market-definition tool.
# Case facts (who, which market, outcome) are real; all numbers for losses, margins
# and diversion are illustrative and labeled as such in the app.

src_link <- function(text, href) htmltools::a(text, href = href, target = "_blank")

steps_cases <- list(
  perrier = list(
    label = "Nestlé/Perrier (France, 1992)",
    steps = c("Still source water (all brands)", "+ Sparkling source water", "+ Soft drinks", "+ All other cold drinks"),
    A = c(8, 35, 45, 55), m = 50, dp = 0.10,
    market_name = c("Still source water", "Bottled source water, still and sparkling", "Bottled water and soft drinks", "All cold drinks"),
    story = htmltools::tagList(
      htmltools::p("Nestlé wants to buy Perrier. Together with BSN (today Danone), the largest groups would sell most of the bottled water in France. Is bottled water a market of its own, or does it compete with soft drinks?"),
      htmltools::p(htmltools::strong("Before you move a slider: "), "are still and sparkling water in the same market? Are soft drinks?")),
    decided = htmltools::tagList(
      htmltools::p("The European Commission defined the market as bottled source water, still and sparkling, sold in France (it noted that its assessment would not change if still water were taken alone). Soft drinks were left out: they cost two to three times as much, their prices moved differently from water prices, and shoppers bought water as a natural, healthy product."),
      htmltools::p("Supply substitution did not widen the market either: soft-drink makers could not use their bottling lines, because source water must be bottled at the source; building a new brand would also have needed heavy advertising, so they could not enter quickly."),
      htmltools::p("The merger was cleared subject to conditions: Nestlé had to sell several Perrier brands (among them Vichy, Thonon and Saint-Yorre) to an approved third party; Volvic went to BSN.")),
    source = src_link("Commission Decision 92/553/EEC, Case IV/M.190", "https://eur-lex.europa.eu/eli/dec/1992/553/oj/eng")
  ),
  wholefoods = list(
    label = "Whole Foods/Wild Oats (United States, 2007)",
    steps = c("Whole Foods + Wild Oats", "+ Other premium natural and organic stores", "+ Conventional supermarkets", "+ Discounters and warehouse clubs"),
    A = c(5, 25, 60, 80), m = 45, dp = 0.05,
    market_name = c("Whole Foods and Wild Oats only", "Premium natural and organic supermarkets", "All supermarkets", "All food retail"),
    story = htmltools::tagList(
      htmltools::p("Whole Foods wants to buy Wild Oats, its closest rival among premium natural and organic supermarkets. The FTC says these stores form a market of their own. Whole Foods says it competes with every supermarket, and many of its shoppers also buy groceries elsewhere."),
      htmltools::p(htmltools::strong("Before you move a slider: "), "every supermarket sells groceries. Can a few premium stores still be a market of their own?")),
    decided = htmltools::tagList(
      htmltools::p("The district court (2007) sided with Whole Foods and refused to stop the merger. The Court of Appeals (2008) reversed. The lead opinion held that the lower court had looked only at shoppers at the margin, not at the core customers who are loyal to premium natural stores."),
      htmltools::p("The economics: with high margins, a price increase is profitable even if most switchers leave the group. With an illustrative 45 percent margin and a 5 percent price increase, keeping about one in ten switchers inside the group is enough (the worked example in Farrell and Shapiro 2008; the actual margin was not public). In 2009 Whole Foods settled with the FTC and agreed to divest 32 stores, 19 of them already closed, and the Wild Oats brand.")),
    source = htmltools::tagList(
      src_link("FTC v. Whole Foods Market, 548 F.3d 1028 (D.C. Cir. 2008)", "https://www.courtlistener.com/opinion/1287444/federal-trade-commission-v-whole-foods-market-inc/"),
      "; ", src_link("Farrell and Shapiro (2008), Improving critical loss analysis", "http://faculty.haas.berkeley.edu/Shapiro/critical2008.pdf"))
  ),
  custom = list(
    label = "Your own numbers",
    steps = c("Product A (all brands)", "+ Product B", "+ Product C", "+ Product D"),
    A = c(10, 35, 55, 70), m = 40, dp = 0.10,
    market_name = c("Product A", "A and B", "A, B, and C", "A to D"),
    story = htmltools::p("Order the products by how many customers switch to them, closest first. At each step, set the share of lost customers the group recaptures. At step 1 that is switching between brands of product A; with a single brand it is zero."),
    decided = NULL, source = NULL
  )
)

cl_cases <- list(
  perrier = list(label = "Nestlé/Perrier: still and sparkling water", m = 50, dp = 0.10, method = "div", A = 35, eps = 2,
    note = "The Commission found very high margins on bottled water. Suppose that 35% of the customers the group loses after a price increase switch to another water inside the group (the same number as step 2 in tab 2). Numbers are illustrative."),
  wholefoods = list(label = "Whole Foods/Wild Oats: premium natural stores", m = 45, dp = 0.05, method = "div", A = 25, eps = 1.8,
    note = "The district court reasoned that most customers who leave Whole Foods go to conventional supermarkets, so premium stores cannot be a market. With an illustrative 45% margin and a 5% price increase, the critical loss is only 10%: keeping a quarter of the switchers inside the group is enough. (The stricter test of whether the monopolist would actually raise price by 5% needs about 18%: still well below a quarter.)"),
  grocer = list(label = "Low-margin grocer (contrast)", m = 10, dp = 0.05, method = "div", A = 25, eps = 2,
    note = "Same switching pattern as Whole Foods, but a 10% margin. A low margin signals price-sensitive customers: a 5% price increase loses 37.5% of sales, more than the critical loss of 33.3%. The group would have to recapture more than a third of the switchers. Numbers are illustrative."),
  custom = list(label = "Your own numbers", m = 40, dp = 0.05, method = "div", A = 30, eps = 1.5, note = NULL)
)

cello_case <- htmltools::tagList(
  htmltools::p("Du Pont made about three quarters of the cellophane sold in the United States. In 1956 the Supreme Court found no monopoly: cellophane competed with other flexible wrapping materials, where its share was under 20 percent, because customers readily switched to other wraps."),
  htmltools::p("Critics replied that customers switched ", htmltools::em("because"), " du Pont already charged a monopoly price. The numbers below are stylized."),
  htmltools::p(src_link("United States v. E. I. du Pont de Nemours & Co., 351 U.S. 377 (1956)", "https://supreme.justia.com/cases/federal/us/351/377/")))
