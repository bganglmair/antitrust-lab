# Case texts and presets for the tacit collusion tool.
# Case facts are real. All profits and other numbers in the tool are illustrative and labeled.

src_link <- function(text, href) htmltools::a(text, href = href, target = "_blank", rel = "noopener")

danish_story <- htmltools::tagList(
  htmltools::p("In late 1993, the Danish competition authority decided to publish the prices that individual producers of ready-mixed concrete charged their customers. The first figures appeared in 1994, and new ones followed regularly. The aim was to make the market more transparent and to help buyers compare offers."),
  htmltools::p(htmltools::strong("Decide before you change a setting. "), "What happened to concrete prices after the publication?"))

danish_decided <- htmltools::tagList(
  htmltools::p("Within a year, average prices of the reported grades rose by 15 to 20 percent. No agreement among the producers was found. The likely reason, according to Albæk, Møllgaard and Overgaard, is that publication made discounts easy for rivals to see, so cutting prices no longer paid. Compare the two Danish concrete examples in this tab."))

danish_source <- htmltools::tagList(
  src_link("Albæk, Møllgaard and Overgaard (1997)", "https://doi.org/10.1111/1467-6451.00057"))

# Presets give the three profits. The sidebar asks for the profit after cheating and the two increments.
ic_presets <- list(
  example = list(label = "Two firms (worked example)", pc = 6, pd = 10, pp = 2, d = 0.6,
                 note = "Collusion pays 6 per period. Cheating pays 10 once, then competition pays 2."),
  smaller = list(label = "A smaller gain from cheating", pc = 6, pd = 8, pp = 2, d = 0.4,
                 note = "Cheating now pays only 8 once."),
  three = list(label = "Three firms", pc = 8, pd = 12, pp = 3, d = 0.5,
               note = "Three identical firms. Each earns 8 per period under collusion, 12 once when it cheats, and 3 under competition."),
  four = list(label = "A fourth firm enters", pc = 5, pd = 12, pp = 3, d = 0.5,
              note = "Collusion now pays only 5 per firm. A firm that cheats still earns at most 12, because its capacity limits how much it can sell. Illustrative numbers."))

fe_presets <- list(
  before = list(label = "Danish concrete before publication", n = 4, r = 20, chg = "3 months", seen = "2 years",
                note = "Illustrative numbers. Four producers change prices every quarter, but a rival's discount comes to light only after about two years."),
  after = list(label = "Danish concrete after publication", n = 4, r = 20, chg = "3 months", seen = "3 months",
               note = "Illustrative numbers. Prices are published regularly with a lag of a few months. The tool counts three months until a discount is seen."),
  match = list(label = "Price matching", n = 4, r = 20, chg = "1 day", seen = "1 day",
               note = "Illustrative numbers. Every seller matches a lower price at once. A price cut is answered within a day and wins almost nothing."))

mm_presets <- list(
  slides = list(label = "Two markets that differ", names = c("Market 1", "Market 2", "Market 3", "Market 4"), on = c(TRUE, TRUE, FALSE, FALSE),
                pc = c(3, 4, 3, 3), pd = c(5, 5, 5, 5), pp = c(1, 1, 1, 1), d = 0.4,
                note = "Two firms meet in two markets. Collusion pays 3 in market 1 and 4 in market 2."),
  same = list(label = "Two identical markets", names = c("Market 1", "Market 2", "Market 3", "Market 4"), on = c(TRUE, TRUE, FALSE, FALSE),
              pc = c(3, 3, 3, 3), pd = c(5, 5, 5, 5), pp = c(1, 1, 1, 1), d = 0.4,
              note = "Both markets have the same profits."),
  xy = list(label = "Airlines on routes X and Y", names = c("Route X", "Route Y", "Route Z", "Route W"), on = c(TRUE, TRUE, FALSE, FALSE),
            pc = c(5, 8, 8, 8), pd = c(9, 12, 10, 10), pp = c(1, 2, 2, 2), d = 0.42,
            note = "Illustrative numbers. Three airlines meet on routes X and Y."),
  xyzw = list(label = "Airlines on all four routes", names = c("Route X", "Route Y", "Route Z", "Route W"), on = c(TRUE, TRUE, TRUE, TRUE),
              pc = c(5, 8, 8, 8), pd = c(9, 12, 10, 10), pp = c(1, 2, 2, 2), d = 0.42,
              note = "Illustrative numbers. After a merger, the same three airlines meet on all four routes. The number of airlines on each route does not change."))

pw_note <- htmltools::tagList(
  htmltools::p("The railroads of the Joint Executive Committee, a cartel of the 1880s that operated openly and was legal at the time, had recurring price wars. Porter (1983) and Ellison (1994) find that the wars largely fit this model, although Ellison also finds some real cheating. The monitoring problem is the same for tacit collusion."),
  htmltools::p(htmltools::HTML(paste0("Sources: ", src_link("Porter (1983)", "https://doi.org/10.2307/3003634"), " and ", src_link("Ellison (1994)", "https://doi.org/10.2307/2555852"), "."))))
