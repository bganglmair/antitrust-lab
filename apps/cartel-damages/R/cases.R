# Case texts for the cartel damages tool.
# Case facts are real. All price series and numbers in the tool are illustrative and labeled.

src_link <- function(text, href) htmltools::a(text, href = href, target = "_blank")

lysine_story <- htmltools::tagList(
  htmltools::p("Lysine is an amino acid that feed manufacturers add to animal feed. From June 1992 to June 1995, the five producers (ADM, Ajinomoto, Kyowa Hakko, Sewon and Cheil) fixed prices and sales quotas worldwide. Before the cartel, ADM had entered with a very large plant. The price war that followed pushed the price down to about $0.64 per pound. Under the cartel, the price rose to between $1.10 and $1.20."),
  htmltools::p(htmltools::strong("Decide before you choose a method. "),
    "The buyers want their money back. What would lysine have cost without the cartel?"))

lysine_decided <- htmltools::tagList(
  htmltools::p("An ADM executive recorded the cartel meetings for the FBI. In October 1996, ADM pleaded guilty and paid a criminal fine of $100 million for the lysine and citric acid cartels. Three ADM executives were convicted in 1998. In 2000, the European Commission fined the five producers almost 110 million euros."),
  htmltools::p("In the damages litigation, the experts disagreed about the price without the cartel. The low price just before the cartel was the result of a price war, so one side treated it as too low for a benchmark and the other side treated it as the competitive price."))

lysine_source <- htmltools::tagList(
  src_link("Connor (2001), The lysine cartel of 1992-1995", "https://doi.org/10.1023/A:1026513927396"), " and ",
  src_link("US Department of Justice (1996)", "https://www.justice.gov/archive/atr/public/press_releases/1996/0988.htm"))

ba_presets <- c(
  "Clean case" = "clean",
  "Rising costs" = "rising",
  "Price war before the cartel" = "war",
  "Prices overshoot after the collapse" = "over",
  "Rising costs and overshooting" = "rising_over")

ba_notes <- list(
  clean = "Costs have no trend, and prices before and after the cartel are at their normal level.",
  rising = "Costs rise over the whole period, so competitive prices would have risen as well.",
  war = "In the last 12 months before the cartel, a price war holds the price 30 cents below its normal level.",
  over = "In the first 12 months after the collapse, the sellers have too much capacity and the price is 10 cents below its normal level.",
  rising_over = "Costs rise over the whole period, and in the first 12 months after the collapse the price is 10 cents below its normal level.")

cm_presets <- list(
  default = list(label = "Comparator moves in parallel", gap = 10, trend = 0.2, extra = 0, share = 0),
  drift = list(label = "Comparator drifts upward on its own", gap = 10, trend = 0, extra = 0.1, share = 0),
  affected = list(label = "The cartel also raised prices in the comparator", gap = 10, trend = 0.2, extra = 0, share = 50),
  cartelized = list(label = "The comparator was cartelized too", gap = 10, trend = 0.2, extra = 0, share = 100))
