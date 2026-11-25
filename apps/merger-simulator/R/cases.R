# Case texts and presets for the merger simulator.
# Case facts are real. All shares, prices, margins and costs are illustrative.

src_link <- function(text, href) htmltools::a(text, href = href, target = "_blank", rel = "noopener")

staples_story <- htmltools::tagList(
  htmltools::p("In 1996, Staples agreed to buy Office Depot. They were two of the three office-supply superstores in the United States, and many other shops also sold paper, pens and toner. The Federal Trade Commission asked a court to stop the merger. The example \"Office superstores\" is loaded."),
  htmltools::p(htmltools::strong("Decide before you change a setting. "), "Would the merger have raised prices, even though so many other shops sold the same goods?"))
staples_decided <- htmltools::tagList(
  htmltools::p("In 1997 the court granted the Federal Trade Commission a preliminary injunction, and the companies gave up the merger. Price data showed that Staples charged more where it faced no other superstore, and Office Depot charged more where it was the only superstore. For customers, the two superstores were each other's closest competitors. The example in this tool uses illustrative numbers in the same spirit."))
staples_source <- htmltools::HTML(paste0(src_link("FTC v. Staples, Inc., 970 F. Supp. 1066 (D.D.C. 1997)", "https://law.justia.com/cases/federal/district-courts/FSupp/970/1066/1639260/")))

haggen_note <- htmltools::tagList(
  htmltools::p("Albertsons and Safeway, two US grocery chains, merged in 2015. The Federal Trade Commission required them to sell 168 stores, and 146 of them went to Haggen, a much smaller chain. Haggen filed for bankruptcy in September 2015, a few months later. A divestiture works only if the buyer can compete as well as the seller did."),
  htmltools::p(htmltools::HTML(paste0("Source: ", src_link("FTC press release, January 2015", "https://www.ftc.gov/news-events/news/press-releases/2015/01/ftc-requires-albertsons-safeway-sell-168-stores-condition-merger"), "."))))

olympic_note <- htmltools::tagList(
  htmltools::p("The European Commission prohibited the merger of the Greek airlines Aegean and Olympic in 2011. It cleared a second attempt in 2013, because Olympic, in the middle of the Greek crisis, would have been forced to leave the market soon in any event. The Commission therefore compared the merger with a market without Olympic, which was the counterfactual. The example \"A large firm buys a small rival\" fits this case."),
  htmltools::p(htmltools::HTML(paste0("Source: ", src_link("European Commission press release IP/13/927 (case M.6796)", "https://ec.europa.eu/commission/presscorner/detail/en/IP_13_927"), "."))))

# Presets: sizes are market shares in percent of the listed firms
ms_presets <- list(
  stores = list(label = "Office superstores", names = c("Superstore 1", "Superstore 2", "Superstore 3", "Other shops"), size = c(35, 30, 15, 20), A = 1, B = 2,
                note = "Illustrative numbers in the spirit of Staples and Office Depot. The two largest superstores merge. The other shops are treated as one firm."),
  equal = list(label = "Four equal firms", names = paste("Firm", 1:5), size = c(25, 25, 25, 25), A = 1, B = 2,
               note = "Illustrative numbers. Four firms of the same size. Firms 1 and 2 merge."),
  large = list(label = "The two largest firms merge", names = paste("Firm", 1:5), size = c(37.5, 31.25, 18.75, 12.5), A = 1, B = 2,
               note = "Illustrative numbers. The two largest of four firms merge."),
  small = list(label = "A large firm buys a small rival", names = paste("Firm", 1:5), size = c(37.5, 31.25, 18.75, 12.5), A = 1, B = 4,
               note = "Illustrative numbers. The largest firm buys the smallest. The small rival earns a margin of about 31 percent, so if it fails, it fails because of fixed costs or debt."))
ms_preset_defaults <- list(s0 = 20, k = 1, mk = 40, p = 10)
