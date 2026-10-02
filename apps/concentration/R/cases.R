# Case presets for the concentration tool.
# Case facts are real; elasticities, margins and diversion ratios are illustrative and labeled.

src_link <- function(text, href) htmltools::a(text, href = href, target = "_blank")

heinz_source <- htmltools::tagList(
  src_link("FTC v. H.J. Heinz Co., 246 F.3d 708 (D.C. Cir. 2001)",
           "https://law.justia.com/cases/federal/appellate-courts/F3/246/708/469087/"))

heinz_story <- htmltools::tagList(
  htmltools::p("In 2000, Heinz agreed to buy Beech-Nut. In US baby food, Gerber sold about two of every three jars; Heinz (17.4%) and Beech-Nut (15.4%) were a distant second and third. The FTC went to court to stop the deal."),
  htmltools::p(htmltools::strong("Before you move a slider: "),
    "Heinz and Beech-Nut were rarely sold in the same store. Can a merger between them raise prices?"))

heinz_decided <- htmltools::tagList(
  htmltools::p("The district court refused to block the merger (October 2000). The Court of Appeals reversed in April 2001 and sent the case back for a preliminary injunction; Heinz called off the deal the same day."),
  htmltools::p("The court relied on the HHI: 4,775 before the merger and an increase of 510, far above the thresholds, which created a strong presumption of harm. Heinz and Beech-Nut were rarely on the same shelf (Heinz in about 40% of supermarkets, Beech-Nut in about 45%, Gerber in over 90%), but they competed fiercely for the second position on retailers' shelves. The court rejected the claimed efficiencies as not proven."),
  htmltools::p("With the rounded shares and the remaining 2.2% counted as one firm, the tool gets 4,770 and 536. The court's increase of 510 cannot be reproduced from the shares it quotes; it comes from the district court's findings. The court applied Section 7 of the Clayton Act and used the HHI levels of the 1992 Guidelines (1,800 and 100). The 2010 Guidelines raised them to 2,500 and 200; the 2023 Guidelines returned to 1,800 and 100."))

market_cases <- list(
  heinz = list(
    label = "Heinz/Beech-Nut (US baby food, 2001)",
    names = c("Gerber", "Heinz", "Beech-Nut", "Firm 4"),
    shares = c(65, 17.4, 15.4, 0), n_oth = 1, eta = 1.5, mergeA = 2, mergeB = 3,
    story = heinz_story, decided = heinz_decided, source = heinz_source),
  fragmented = list(
    label = "Fragmented market (illustrative)",
    names = c("Firm 1", "Firm 2", "Firm 3", "Firm 4"),
    shares = c(10, 10, 10, 10), n_oth = 6, eta = 1.5, mergeA = 1, mergeB = 2,
    story = htmltools::p("Ten firms, each with 10% of the market. Two of them want to merge. Illustrative numbers."),
    decided = NULL, source = NULL),
  custom = list(
    label = "Your own numbers",
    names = c("Firm 1", "Firm 2", "Firm 3", "Firm 4"),
    shares = c(30, 25, 20, 10), n_oth = 3, eta = 2, mergeA = 2, mergeB = 3,
    story = htmltools::p("Set the shares of up to four firms; the rest of the market is split equally among the other firms."),
    decided = NULL, source = NULL)
)

guppi_cases <- list(
  close = list(
    label = "Close substitutes, small shares (illustrative)",
    sA = 10, sB = 8, DAB = 40, DBA = 45, mA = 40, mB = 40,
    rivals = c(11, 11, 10, 10, 10, 10, 10, 10),
    note = "Two brands with small shares whose customers see them as each other's best alternative. Illustrative numbers."),
  distant = list(
    label = "Large shares, distant substitutes (illustrative)",
    sA = 25, sB = 15, DAB = 10, DBA = 12, mA = 30, mB = 25,
    rivals = c(15, 15, 15, 15),
    note = "Two large firms serving different customers: few of A's customers would switch to B, and vice versa. Illustrative numbers."),
  heinz = list(
    label = "Heinz/Beech-Nut: shoppers' diversion (illustrative)",
    sA = 17.4, sB = 15.4, DAB = 5, DBA = 5, mA = 40, mB = 40,
    rivals = c(65, 2.2),
    note = "Shares from the case. Because the two brands were rarely in the same store, few shoppers would switch from one to the other: the diversion ratios and margins here are illustrative. What does the GUPPI miss?",
    bench_note = "Shoppers' switching is not the whole story here: Heinz and Beech-Nut competed for retailers' second shelf slot, and that competition does not show up in shoppers' diversion."),
  custom = list(
    label = "Your own numbers",
    sA = 20, sB = 15, DAB = 25, DBA = 30, mA = 35, mB = 35,
    rivals = c(25, 20, 20),
    note = NULL)
)
