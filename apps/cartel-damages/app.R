# Antitrust Lab: Cartel damages
library(shiny)
library(bslib)
library(ggplot2)

# Shiny sources R/ automatically (theme.R, damages.R, cases.R)

app_version <- "0.5 (2026-10-12)"

verdict_box <- function(title, value, ok, note = NULL) {
  value_box(
    title = title, value = value,
    theme = if (isTRUE(ok)) value_box_theme(bg = lab_colors[["pass"]], fg = lab_colors[["pass_fg"]])
            else value_box_theme(bg = lab_colors[["fail"]], fg = lab_colors[["fail_fg"]]),
    if (!is.null(note)) p(note)
  )
}

number_box <- function(title, value, note = NULL, color = lab_colors[["critical"]]) {
  value_box(title = title, value = value,
            theme = value_box_theme(bg = lab_colors[["bg"]], fg = color),
            if (!is.null(note)) p(note, style = paste0("color:", lab_colors[["muted"]])))
}

muted <- function(...) p(..., style = paste0("color:", lab_colors[["muted"]], "; font-size:.9rem;"))
usd <- function(x, digits = 2) paste0(if (x < -1e-9) "−" else "", "$", num(abs(x), digits))
# one decimal, or two when the value is not a round tenth of a cent
cents <- function(x) {
  v <- 100 * x
  paste0(if (v < -1e-9) "−" else "", num(abs(v), if (abs(v * 10 - round(v * 10)) > 1e-6) 2 else 1), " cents")
}

# ---- Sidebar ----------------------------------------------------------------
sb <- sidebar(
  width = 330,
  conditionalPanel("input.tab == 't1'",
    selectInput("ba_preset", "What happened in the market", ba_presets),
    uiOutput("ba_note"),
    tags$hr(),
    p(strong("Your method")),
    radioButtons("ba_period", "Compare cartel prices with prices",
                 c("before the cartel" = "before", "after the cartel" = "after", "before and after" = "both")),
    checkboxInput("ba_ctrl", "Take cost changes into account (regression)", FALSE),
    checkboxInput("ba_excl", "Leave out the 12 months right before and right after the cartel", FALSE),
    tags$hr(),
    checkboxInput("ba_truth", "Show the true but-for price", FALSE),
    muted(em("Illustrative price series. One million pounds are sold per month."))
  ),
  conditionalPanel("input.tab == 't2'",
    selectInput("cm_preset", "Comparator market", setNames(names(cm_presets), sapply(cm_presets, `[[`, "label"))),
    sliderInput("cm_gap", "The comparator is cheaper by (cents per pound)", 0, 30, 10, 5, ticks = FALSE),
    sliderInput("cm_trend", "Common trend in both markets (cents per month)", 0, 0.4, 0.2, 0.1, ticks = FALSE),
    sliderInput("cm_extra", "Extra trend in the comparator alone (cents per month)", 0, 0.2, 0, 0.05, ticks = FALSE),
    sliderInput("cm_share", "Cartel effect on the comparator (share of the overcharge)", 0, 100, 0, 10, post = "%", ticks = FALSE),
    actionButton("cm_reset", "Reset the sliders to this case", class = "btn-outline-primary btn-sm mb-3"),
    checkboxInput("cm_truth", "Show the true but-for price", FALSE),
    muted(em("Illustrative price series."))
  ),
  conditionalPanel("input.tab == 't3'",
    sliderInput("st_n", "Number of sellers n", 2, 10, 4, 1, ticks = FALSE),
    sliderInput("st_c", "Cost of one more unit (marginal cost c)", 20, 80, 40, 5, ticks = FALSE),
    radioButtons("st_model", "Competition without the cartel",
                 c("Cournot (sellers choose quantities)" = "cournot",
                   "Bertrand, identical products (sellers choose prices)" = "bertrand",
                   "Bertrand, differentiated products (sellers choose prices)" = "diff")),
    conditionalPanel("input.st_model == 'diff'",
      sliderInput("st_g", "How close substitutes are the products? (\u03b3)", 0, 30, 3, 1, ticks = FALSE)),
    muted(em("Hypothetical market with demand p = 200 − 0.5 Q. The cartel lasts 5 years."))
  ),
  conditionalPanel("input.tab == 't4'",
    sliderInput("po_nd", "Number of direct purchasers", 1, 10, 3, 1, ticks = FALSE),
    sliderInput("po_dw", "Overcharge per unit of input", 1, 20, 12, 1, ticks = FALSE),
    muted(em("Illustrative numbers. The direct purchasers sell to their own customers with demand p = 100 − Q. They need one unit of the input for each unit they sell, and their unit cost without the cartel is 40."))
  ),
  conditionalPanel("input.tab == 't5'",
    muted("Predict first, then check with the tool, then reveal.")
  )
)

# ---- Tabs -------------------------------------------------------------------
tab1 <- nav_panel("1 Before and after", value = "t1",
  card(
    card_body(lysine_story),
    accordion(open = FALSE, accordion_panel("What was decided? (open after you have made up your mind)", lysine_decided)),
    card_footer(tagList("Source: ", lysine_source))),
  card(card_header("What this tab does"),
    card_body(p("The overcharge is the cartel price minus the but-for price, which is the price that buyers would have paid without the cartel. ",
                "Nobody observes the but-for price. A before-and-after comparison estimates it from the prices in the same market outside the cartel period. ",
                "On the left, you choose what happened in the market and how you estimate. The tool knows the true overcharge and tells you how close you are."),
              p("A competitive price follows cost. If you take cost changes into account, a regression estimates how the price moves with a cost index in the months you use. ",
                "It then counts as overcharge only the part of the higher cartel price that the cost index does not explain."))),
  layout_columns(fill = FALSE, col_widths = c(4, 4, 4),
    uiOutput("ba_box_est"), uiOutput("ba_box_dam"), uiOutput("ba_box_verdict")),
  card(card_header("Illustrative price per pound, with your estimate of the but-for price in the cartel period"),
       plotOutput("ba_plot", height = "400px"),
       plotOutput("ba_cost", height = "170px"),
       card_footer("The shaded months are the cartel period. The months you compare with are drawn in blue. The dashed line is your estimate of the but-for price. The lower panel shows the cost index, which drives the but-for price.")),
  accordion(open = FALSE, accordion_panel("Show the numbers", tableOutput("ba_table")))
)

tab2 <- nav_panel("2 Comparator market", value = "t2",
  card(card_header("What this tab does"),
    card_body(
      p("A second market without a cartel, the comparator, offers a second source of comparison. The tab shows three estimates side by side. They use different parts of the data."),
      tags$ol(
        tags$li(strong("Before and after. "), "Compare the cartel market with itself, during the cartel and before it. The comparator is not used. This is correct only if nothing else changed the price over time."),
        tags$li(strong("Comparator market (in US courts, the yardstick). "), "Compare the cartel market with the comparator in the same months, during the cartel. The months before the cartel are not used. This is correct only if both markets would have had the same price without the cartel."),
        tags$li(strong("Difference-in-differences. "), "Combine the two. Compare the price change in the cartel market with the price change in the comparator. This removes a constant price gap between the markets and a trend that both markets share. It is correct if prices in both markets would have moved in parallel without the cartel.")),
      p("The comparator must be unaffected by the cartel. With the last slider, you let the cartel raise prices in the comparator as well. ",
        "This happens when the comparator consists of sellers outside the cartel who raise their prices in its wake (umbrella pricing). Their customers are harmed as well."),
      p("The true overcharge is 30 cents per pound. A tile turns blue if its estimate is within 5% of this value."))),
  layout_columns(fill = FALSE, col_widths = c(4, 4, 4),
    uiOutput("cm_box_ba"), uiOutput("cm_box_yard"), uiOutput("cm_box_did")),
  card(card_header("Prices in the cartel market and in the comparator market"),
       plotOutput("cm_plot", height = "400px"),
       card_footer("The shaded months are the cartel period. The dashed line is the but-for price that difference-in-differences implies, which is the comparator's price shifted by the gap between the two markets before the cartel.")),
  card(card_header("Average prices (dollars per pound)"), tableOutput("cm_table"))
)

tab3 <- nav_panel("3 Which competition without the cartel?", value = "t3",
  card(card_header("What this tab does"),
    card_body(
      p("Without a comparison period or a comparator market, an expert can compute the but-for price from a model of competition. The result depends on the model. ",
        "With Cournot competition, each seller chooses a quantity and the price stays above cost. With Bertrand competition and identical products, sellers undercut each other until the price equals cost. ",
        "With Bertrand competition and differentiated products, each seller keeps the buyers who prefer its product, so the price stays above cost. The closer substitutes the products are, the closer the price comes to cost."),
      p("In this market, all sellers are in the cartel and set the price that maximizes their joint profit. ",
        "Cournot fits identical products from sellers bound by their capacity. Bertrand with identical products fits sellers who can each serve all buyers at its price. Bertrand with differentiated products fits brands that buyers do not see as identical."),
      p("In this tab, damages are the overcharge on the units that buyers bought. Tab 4 takes up the sales that are lost."))),
  layout_columns(fill = FALSE, col_widths = c(3, 3, 3, 3),
    uiOutput("st_box_cartel"), uiOutput("st_box_bf"), uiOutput("st_box_oc"), uiOutput("st_box_dam")),
  card(card_header("But-for price for each number of sellers, under the model you choose and its alternatives"),
       plotOutput("st_plot", height = "400px"),
       card_footer("The gap between the cartel price and the but-for price is the overcharge. The large dot marks your choice.")),
  uiOutput("st_note")
)

tab4 <- nav_panel("4 Who bears the harm?", value = "t4",
  card(card_header("What this tab does"),
    card_body(
      p("The direct purchasers, here the feed manufacturers, pay the overcharge. They raise their own prices and so pass part of it on to their customers, the indirect purchasers. ",
        "The higher price also costs them sales. Their loss has three parts, which are the overcharge they pay, minus the part they pass on, plus the profit they lose on the sales they no longer make."),
      p("In this tab, every direct purchaser pays the overcharge. Each direct purchaser chooses a quantity, as in the Cournot model of tab 3. Demand is linear, and unit cost is constant. ",
        "Under these conditions, more competition among the direct purchasers means that a larger part is passed on. Other demand curves give other rates, some above 100%. If only one firm paid the overcharge, it could pass on less the more competitors it has."))),
  layout_columns(fill = FALSE, col_widths = c(3, 3, 3, 3),
    uiOutput("po_box_rho"), uiOutput("po_box_paid"), uiOutput("po_box_net"), uiOutput("po_box_ind")),
  card(card_header("The loss of the direct purchasers as a group"),
       plotOutput("po_plot", height = "340px"),
       card_footer("The net loss equals the fall in the direct purchasers' profit.")),
  card(card_header("Who can claim what?"),
       plotOutput("po_claims", height = "340px"),
       card_footer(uiOutput("po_claims_note")))
)

experiment <- function(id, question, choices, answer) {
  card(
    card_header(question),
    radioButtons(paste0("ex_", id), "Your prediction", choices, selected = character(0), width = "100%"),
    actionButton(paste0("exb_", id), "Reveal", class = "btn-outline-primary btn-sm"),
    conditionalPanel(paste0("input.exb_", id, " > 0"), div(style = "margin-top:.6rem;", answer))
  )
}

tab5 <- nav_panel("Questions", value = "t5",
  layout_columns(col_widths = c(6, 6),
    experiment("1", "In tab 1, costs rise over the whole period. You compare cartel prices with prices before the cartel. Is your estimate too high or too low?",
      c("Too high", "Too low", "Correct"),
      p(strong("Too high. "), "Prices before the cartel were low partly because costs were lower then. The comparison gives 33 cents, and the true overcharge is 30 cents. If you take cost changes into account, the estimate is 30 cents.")),
    experiment("2", "In tab 1, choose \"Price war before the cartel\" and compare with prices before the cartel. Is your estimate too high or too low?",
      c("Too high", "Too low", "Correct"),
      p(strong("Too high. "), "Prices in the price-war months are unusually low and understate the but-for price, so the comparison gives 45 cents. Taking cost changes into account does not help, because costs did not cause the low price. Leaving out the price-war months gives 30 cents.")),
    experiment("3", "In tab 1, prices overshoot after the collapse, and you compare with prices after the cartel. Does the regression with cost repair the estimate?",
      c("Yes", "No"),
      p(strong("No. "), "The estimate is 35 cents with and without the cost index. Prices right after the collapse are unusually low and understate the but-for price, so you leave these months out or compare with the months before the cartel.")),
    experiment("4", "In tab 2, the cartel also raised prices in the comparator market by half as much. What happens to the difference-in-differences estimate?",
      c("It is too low", "It is too high", "Nothing"),
      p(strong("It is too low. "), "The comparator's price rise is taken for a common trend and subtracted. The estimate falls to 15 cents. With a comparator that was itself cartelized, it falls to zero.")),
    experiment("5", "In tab 2, there is no common trend, but the comparator's prices drift upward on their own. Which method is right?",
      c("Difference-in-differences", "Before and after", "Comparator market"),
      p(strong("Before and after. "), "It gives 30 cents. Difference-in-differences gives 27 cents because the trends are not parallel. A comparator helps only if it tells us what would have happened in the cartel market.")),
    experiment("6", "In tab 3, which model gives higher damages, and which fits an identical product made in plants with limited capacity?",
      c("Bertrand gives more, and Bertrand fits", "Bertrand gives more, and Cournot fits", "Cournot gives more, and Cournot fits"),
      p(strong("Bertrand gives more, and Cournot fits. "), "With four sellers and a cost of 40, damages are 64,000 under Bertrand and 38,400 under Cournot. The model should follow how the sellers compete. With limited capacity, a seller cannot serve the whole market by undercutting, which speaks for Cournot.")),
    experiment("7", "A report says that the overcharge was 40%. In tab 3 with four sellers, a cost of 40 and Cournot competition, which price is the base?",
      c("The cartel price", "The but-for price"),
      p(strong("The cartel price. "), "The overcharge is 48, which is 40% of the cartel price of 120. Measured against the but-for price of 72, the same overcharge is 66.7%. A percentage without its base cannot be compared with another one.")),
    experiment("8", "In tab 4, every direct purchaser pays the overcharge. With more competition among them, do they pass on more or less?",
      c("More", "Less", "The same"),
      p(strong("More. "), "A single direct purchaser passes on one half, and three pass on three quarters. With many, almost everything is passed on, because their margin is too thin to absorb a cost increase that hits all of them.")),
    experiment("9", "In tab 4 with 3 direct purchasers and an overcharge of 12, the direct purchasers pass on three quarters of the overcharge. In the EU, is their claim one quarter of the overcharge?",
      c("Yes", "No, it is larger", "No, it is smaller"),
      p(strong("No, it is larger. "), "They bear one quarter of the overcharge, which is 108, and they also lose profit of 135 on the sales they no longer make. Their claim is 243.")),
    experiment("10", "In tab 3 with four sellers and a cost of 40, choose Bertrand with differentiated products. What happens to the but-for price as the products become closer substitutes?",
      c("It falls toward cost", "It rises toward the cartel price", "It does not change"),
      p(strong("It falls toward cost. "), "With unrelated products (\u03b3 = 0), each seller is a monopolist on its own buyers, and the but-for price equals the cartel price of 120, so there is no overcharge. With \u03b3 = 3, the but-for price is 77.65. With \u03b3 = 30, it is 46.53, close to the cost of 40. Identical products, as in the lysine case, are the extreme case."))
  )
)

tab6 <- nav_panel("Math", value = "t6",
  withMathJax(),
  card(card_header("Notation"),
    p("\\(p^{\\text{cartel}}\\) is the cartel price and \\(p^{\\text{bf}}\\) the but-for price. The overcharge is \\(p^{\\text{cartel}} - p^{\\text{bf}}\\). \\(\\tau\\) is the true effect of the cartel on the price, and \\(\\widehat{\\tau}\\) is an estimate of it. \\(n\\) is the number of sellers, \\(c\\) the marginal cost, \\(Q\\) the market quantity, \\(n_{\\text{d}}\\) the number of direct purchasers, \\(\\Delta w\\) the overcharge on their input, and \\(\\rho\\) the pass-on rate."),
    p("An overcharge in percent needs a base. It is \\((p^{\\text{cartel}} - p^{\\text{bf}})/p^{\\text{bf}}\\) relative to the but-for price and \\((p^{\\text{cartel}} - p^{\\text{bf}})/p^{\\text{cartel}}\\) relative to the cartel price. Damages are the overcharge times the quantity bought during the cartel.")),
  card(card_header("Before and after (tab 1)"),
    p("The cost index is \\(100 + g\\,(t - 42.5) + 2 \\sin(2\\pi t/12)\\) in month \\(t\\), with \\(g = 0\\) or \\(g = 0.2\\). The but-for price is \\(p^{\\text{bf}}_t = 0.30 + 0.005 \\times \\text{cost index}_t\\) dollars per pound. In the 36 cartel months (months 25 to 60), the price is \\(p^{\\text{bf}}_t + \\tau\\) with \\(\\tau = 0.30\\). In the price-war case, the price in months 13 to 24 is 0.30 below this formula. In the overshooting cases, the price in months 61 to 72 is 0.10 below it."),
    p("The simple comparison is \\(\\widehat{\\tau} = \\bar p_{\\text{cartel months}} - \\bar p_{\\text{comparison months}}\\). The regression that takes cost changes into account is $$p_t = \\beta_0 + \\beta_1 \\, \\text{cost index}_t + \\tau \\, \\text{Cartel}_t + \\varepsilon_t,$$ estimated on the cartel months and the comparison months, where \\(\\text{Cartel}_t\\) is 1 in the cartel months and 0 otherwise. Damages are \\(\\widehat{\\tau}\\) times 36 million pounds.")),
  card(card_header("Comparator market (tab 2)"),
    p("With average prices \\(\\bar p\\) before and during the cartel, $$\\widehat{\\tau}_{\\text{before-and-after}} = \\bar p^{\\,\\text{cartel market}}_{\\text{during}} - \\bar p^{\\,\\text{cartel market}}_{\\text{before}}, \\qquad \\widehat{\\tau}_{\\text{comparator}} = \\bar p^{\\,\\text{cartel market}}_{\\text{during}} - \\bar p^{\\,\\text{comparator}}_{\\text{during}},$$ $$\\widehat{\\tau}_{\\text{DiD}} = \\left(\\bar p^{\\,\\text{cartel market}}_{\\text{during}} - \\bar p^{\\,\\text{cartel market}}_{\\text{before}}\\right) - \\left(\\bar p^{\\,\\text{comparator}}_{\\text{during}} - \\bar p^{\\,\\text{comparator}}_{\\text{before}}\\right).$$"),
    p("Difference-in-differences removes a constant gap between the markets and a trend that both markets share. It is biased if the comparator has a trend of its own or if the cartel affected the comparator.")),
  card(card_header("Competition without the cartel (tab 3)"),
    p("Demand is \\(p = 200 - 0.5\\,Q\\), and \\(n\\) sellers have the same constant marginal cost \\(c\\). The cartel sets the joint-profit-maximizing price \\(p^{\\text{cartel}} = (200 + c)/2\\) and sells \\(Q^{\\text{cartel}} = 200 - c\\). With Cournot competition, \\(p^{\\text{bf}} = (200 + n c)/(n + 1)\\). With Bertrand competition and identical products, \\(p^{\\text{bf}} = c\\). Damages are \\((p^{\\text{cartel}} - p^{\\text{bf}}) \\times Q^{\\text{cartel}} \\times 5\\)."),
    p("With Bertrand competition and differentiated products, seller \\(i\\) faces demand $$q_i = \\frac{1}{n}\\left[400 - 2(1 + \\gamma)\\,p_i + 2\\gamma\\,\\bar p\\right],$$ where \\(\\bar p\\) is the average price and \\(\\gamma \\ge 0\\) says how close substitutes the products are. At equal prices, total demand is again \\(p = 200 - 0.5\\,Q\\), so the cartel price and quantity do not change. With \\(k = 1 + \\gamma(n - 1)/n\\), the but-for price is \\(p^{\\text{bf}} = (200 + k c)/(1 + k)\\). With \\(\\gamma = 0\\), each seller is a monopolist on its own buyers and the but-for price equals the cartel price. As \\(\\gamma\\) grows, the but-for price approaches \\(c\\). The tool How many firms? uses the same demand.")),
  card(card_header("Who bears the harm? (tab 4)"),
    p("The \\(n_{\\text{d}}\\) direct purchasers compete in quantities with demand \\(p = 100 - Q\\) and unit cost \\(40 + \\Delta w\\). Their own price is \\(p = (100 + n_{\\text{d}} (40 + \\Delta w))/(n_{\\text{d}} + 1)\\), so the pass-on rate, which is the rise in their price per unit of overcharge, is \\(\\rho = n_{\\text{d}}/(n_{\\text{d}} + 1)\\)."),
    p("In this tab, \\(p\\) and \\(Q\\) are the direct purchasers' own price and quantity. With superscripts bf and cartel for the situations without and with the overcharge, the loss of the direct purchasers is $$\\underbrace{\\Delta w \\, Q^{\\text{cartel}}}_{\\text{overcharge paid}} - \\underbrace{(p^{\\text{cartel}} - p^{\\text{bf}}) \\, Q^{\\text{cartel}}}_{\\text{pass-on effect}} + \\underbrace{(Q^{\\text{bf}} - Q^{\\text{cartel}})(p^{\\text{bf}} - 40)}_{\\text{output effect}},$$ which equals the fall in their profit. The indirect purchasers pay \\((p^{\\text{cartel}} - p^{\\text{bf}})\\,Q^{\\text{cartel}}\\) more for the units they buy."),
    p("In the EU, anyone who suffered harm can claim full compensation, which covers the actual loss, lost profit and interest (Directive 2014/104/EU, Articles 3 and 12). The defendant can argue that the claimant passed the overcharge on, and bears the burden of proof (Article 13). An indirect purchaser is deemed to have proved pass-on once it shows the infringement, an overcharge for the direct purchaser, and its own purchase of the affected goods, unless the defendant shows otherwise (Article 14). Customers of sellers outside the cartel can claim for umbrella pricing (Kone). The European Commission's Practical Guide describes the methods of tabs 1 to 4. In the US, a direct purchaser recovers three times the overcharge under Section 4 of the Clayton Act (15 U.S.C. § 15), and the defendant cannot invoke pass-on (Hanover Shoe). Indirect purchasers cannot claim damages under federal law (Illinois Brick), although many states allow such claims under state law.")),
  card(card_header("Sources"),
    tags$ul(
      lab_ref("Connor, John M. 2001. \"'Our Customers Are Our Enemies': The Lysine Cartel of 1992–1995.\" ", em("Review of Industrial Organization"), " 18 (1): 5–21.", url = "https://doi.org/10.1023/A:1026513927396"),
      lab_ref("European Commission. 2013. \"Practical Guide: Quantifying Harm in Actions for Damages Based on Breaches of Article 101 or 102 of the Treaty on the Functioning of the European Union.\" Commission Staff Working Document SWD(2013) 205, June 11.", url = "https://competition-policy.ec.europa.eu/antitrust-and-cartels/actions-damages_en"),
      lab_ref("Clayton Act, Section 4, 15 U.S.C. § 15.", url = "https://www.law.cornell.edu/uscode/text/15/15"),
      lab_ref("European Parliament and Council of the European Union. 2014. \"Directive 2014/104/EU of 26 November 2014 on Certain Rules Governing Actions for Damages under National Law for Infringements of the Competition Law Provisions of the Member States and of the European Union.\" ", em("Official Journal of the European Union"), " L 349, December 5: 1–19.", url = "https://eur-lex.europa.eu/eli/dir/2014/104/oj/eng"),
      lab_ref(em("Hanover Shoe, Inc. v. United Shoe Machinery Corp."), ", 392 U.S. 481 (1968).", url = "https://supreme.justia.com/cases/federal/us/392/481/"),
      lab_ref(em("Illinois Brick Co. v. Illinois"), ", 431 U.S. 720 (1977).", url = "https://supreme.justia.com/cases/federal/us/431/720/"),
      lab_ref("Case C-557/12, ", em("Kone AG and Others v ÖBB-Infrastruktur AG"), ", ECLI:EU:C:2014:1317.", url = "https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:62012CJ0557"),
      lab_ref("US Department of Justice. 1996. \"Archer Daniels Midland Co. to Plead Guilty and Pay $100 Million for Role in Two International Price-Fixing Conspiracies.\" Press release, October 15.", url = "https://www.justice.gov/archive/atr/public/press_releases/1996/0988.htm")))
)

ui <- page_navbar(
  id = "tab",
  fillable = FALSE,
  title = lab_title("Cartel damages"),
  window_title = "Antitrust Lab",
  theme = lab_theme(),
  navbar_options = lab_navbar(),
  sidebar = sb,
  header = div(class = "container-fluid",
    lab_header("How much did the cartel overcharge, and who bears the harm?", "Topic: cartel damages", tool = "Cartel damages")),
  footer = div(class = "container-fluid",
    style = paste0("color:", lab_colors[["muted"]], "; font-size:.85rem; padding:.5rem 0;"),
    paste("Version", app_version, "· Case facts are real. All price series and numbers are illustrative.")),
  tab1, tab2, tab3, tab4, tab5, tab6
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {

  # Tab 1 ---------------------------------------------------------------------
  output$ba_note <- renderUI(muted(ba_notes[[input$ba_preset]]))
  ba_d <- reactive(ba_world(input$ba_preset))
  ba_e <- reactive(ba_estimate(ba_d(), input$ba_period, isTRUE(input$ba_ctrl), isTRUE(input$ba_excl)))

  output$ba_box_est <- renderUI({
    e <- ba_e()
    number_box("Your estimate of the overcharge", paste(cents(e$tau), "per pound"),
               paste0(pct(e$pct_bf), " of the but-for price, ", pct(e$pct_cartel), " of the cartel price"), lab_colors[["actual"]])
  })
  output$ba_box_dam <- renderUI(number_box("Damages", paste0("$", num(ba_e()$damages, 2), " million"),
    "Your estimate times 36 million pounds bought during the cartel"))
  output$ba_box_verdict <- renderUI({
    e <- ba_e(); err <- e$tau / ba_tau - 1; ok <- abs(err) <= 0.05 + 1e-9
    note <- if (abs(err) < 5e-4) "Your estimate equals the true overcharge of 30.0 cents."
            else paste0("Your estimate is ", pct(abs(err)), if (err > 0) " too high." else " too low.",
                        " The true overcharge is 30.0 cents.")
    verdict_box("Is the estimate within 5% of the true overcharge?", if (ok) "Yes" else "No", ok, note)
  })

  output$ba_plot <- renderPlot({
    d <- ba_d(); e <- ba_e()
    d$role <- ifelse(d$cartel, "cartel", ifelse(e$comp, "comp", "unused"))
    seg <- d[d$cartel, ]; seg$bf_hat <- e$bf_hat[d$cartel]
    below <- seg$bf_hat[18] <= seg$bf[18] + 1e-9  # the estimate line lies below the true line
    g <- ggplot(d, aes(t, p)) +
      annotate("rect", xmin = 24.5, xmax = 60.5, ymin = -Inf, ymax = Inf, fill = lab_colors[["fail"]], alpha = 0.55) +
      annotate("text", x = 42.5, y = 1.32, label = "Cartel period", color = lab_colors[["ink"]], size = 4.8) +
      geom_line(color = lab_colors[["muted"]], linewidth = 0.6) +
      geom_point(aes(color = role), size = 2.2) +
      scale_color_manual(values = c(cartel = lab_colors[["ink"]], comp = lab_colors[["pass"]], unused = "#B9AFA5"), guide = "none") +
      geom_line(data = seg, aes(t, bf_hat), color = lab_colors[["actual"]], linewidth = 1.3, linetype = "dashed") +
      annotate("text", x = 42.5, y = seg$bf_hat[18], label = "your estimate of the but-for price", hjust = 0.5,
               vjust = if (below) 2.4 else -1.7, color = lab_colors[["actual"]], size = 4.6)
    if (isTRUE(input$ba_truth))
      g <- g + geom_line(data = seg, aes(t, bf), color = lab_colors[["muted"]], linewidth = 1, linetype = "dotted") +
        annotate("text", x = 42.5, y = seg$bf[18], label = "true but-for price", hjust = 0.5,
                 vjust = if (below) -1.7 else 2.4, color = lab_colors[["muted"]], size = 4.6)
    g + scale_x_continuous(breaks = seq(12, 84, 12), limits = c(1, 84)) +
      scale_y_continuous(labels = function(x) paste0("$", formatC(x, format = "f", digits = 2)), limits = c(0.45, 1.35)) +
      labs(x = "Month", y = "Price per pound") + lab_gg()
  })

  output$ba_cost <- renderPlot({
    d <- ba_d()
    ggplot(d, aes(t, cost)) +
      annotate("rect", xmin = 24.5, xmax = 60.5, ymin = -Inf, ymax = Inf, fill = lab_colors[["fail"]], alpha = 0.55) +
      geom_line(color = lab_colors[["ink"]], linewidth = 1) +
      annotate("text", x = 1, y = 113, label = "Cost index", hjust = 0, color = lab_colors[["ink"]], size = 4.6) +
      scale_x_continuous(breaks = seq(12, 84, 12), limits = c(1, 84)) +
      scale_y_continuous(limits = c(85, 115), breaks = c(90, 100, 110), labels = function(x) formatC(x, format = "f", digits = 1)) +  # same label width as the price panel, so the panels line up
      labs(x = NULL, y = "Index") + lab_gg()
  })

  output$ba_table <- renderTable({
    d <- ba_d(); e <- ba_e()
    data.frame(` ` = c("Average price in the cartel months", "Average price in the months you compare with",
                       "Average cost index in the cartel months", "Average cost index in the months you compare with",
                       "Months you compare with", "Your estimate of the overcharge", "True overcharge"),
               Value = c(usd(e$p_cartel, 3), usd(mean(d$p[e$comp]), 3), num(mean(d$cost[d$cartel]), 1), num(mean(d$cost[e$comp]), 1),
                         as.character(sum(e$comp)), usd(e$tau, 3), usd(ba_tau, 3)),
               check.names = FALSE)
  }, striped = TRUE, width = "100%")

  # Tab 2 ---------------------------------------------------------------------
  observeEvent(input$cm_preset, {
    cs <- cm_presets[[input$cm_preset]]
    updateSliderInput(session, "cm_gap", value = cs$gap); updateSliderInput(session, "cm_trend", value = cs$trend)
    updateSliderInput(session, "cm_extra", value = cs$extra); updateSliderInput(session, "cm_share", value = cs$share)
  }, ignoreInit = TRUE)
  observeEvent(input$cm_reset, {
    cs <- cm_presets[[input$cm_preset]]
    updateSliderInput(session, "cm_gap", value = cs$gap); updateSliderInput(session, "cm_trend", value = cs$trend)
    updateSliderInput(session, "cm_extra", value = cs$extra); updateSliderInput(session, "cm_share", value = cs$share)
  })

  cm_d <- reactive(cm_world(input$cm_gap / 100, input$cm_trend / 100, input$cm_extra / 100, input$cm_share / 100))
  cm_e <- reactive(cm_estimate(cm_d()))
  cm_box <- function(title, est) {
    err <- est / cm_tau - 1; ok <- abs(err) <= 0.05 + 1e-9
    verdict_box(title, paste(cents(est), "per pound"), ok,
                if (abs(err) < 5e-4) "Equal to the true overcharge of 30.0 cents"
                else if (est < 1e-9) "This method finds no overcharge. The true overcharge is 30.0 cents"
                else paste0(pct(abs(err)), if (err > 0) " too high" else " too low", " against the true 30.0 cents"))
  }
  output$cm_box_ba <- renderUI(cm_box("Before and after", cm_e()$ba))
  output$cm_box_yard <- renderUI(cm_box("Comparator market", cm_e()$yard))
  output$cm_box_did <- renderUI(cm_box("Difference-in-differences", cm_e()$did))

  output$cm_plot <- renderPlot({
    d <- cm_d(); e <- cm_e()
    gap0 <- e$means[["tb"]] - e$means[["cb"]]
    dd <- d[d$cartel, ]; dd$did_bf <- dd$comp + gap0; dd$true_bf <- dd$treat - cm_tau
    g <- ggplot(d, aes(t)) +
      annotate("rect", xmin = 24.5, xmax = 60, ymin = -Inf, ymax = Inf, fill = lab_colors[["fail"]], alpha = 0.55) +
      annotate("text", x = 42.5, y = 1.47, label = "Cartel period", color = lab_colors[["ink"]], size = 4.8) +
      geom_line(aes(y = treat), color = lab_colors[["ink"]], linewidth = 1.3) +
      geom_line(aes(y = comp), color = lab_colors[["pass"]], linewidth = 1.3) +
      geom_line(data = dd, aes(y = did_bf), color = lab_colors[["actual"]], linewidth = 1.2, linetype = "dashed") +
      annotate("text", x = 1, y = d$treat[1], label = "cartel market", hjust = 0, vjust = -1, color = lab_colors[["ink"]], size = 4.8) +
      annotate("text", x = 1, y = d$comp[1], label = "comparator", hjust = 0, vjust = 1.8, color = lab_colors[["pass"]], size = 4.8)
    if (isTRUE(input$cm_truth))
      g <- g + geom_line(data = dd, aes(y = true_bf), color = lab_colors[["muted"]], linewidth = 1, linetype = "dotted") +
        annotate("text", x = 60, y = dd$true_bf[nrow(dd)], label = "true but-for price", hjust = 1, vjust = 1.8,
                 color = lab_colors[["muted"]], size = 4.4)
    g + scale_x_continuous(breaks = seq(12, 60, 12), limits = c(1, 60)) +
      scale_y_continuous(labels = function(x) paste0("$", formatC(x, format = "f", digits = 2)), limits = c(0.4, 1.5)) +
      labs(x = "Month", y = "Price per pound") + lab_gg()
  })

  output$cm_table <- renderTable({
    m <- cm_e()$means
    data.frame(` ` = c("Cartel market", "Comparator market", "Difference"),
               `Before the cartel` = c(usd(m[["tb"]], 3), usd(m[["cb"]], 3), usd(m[["tb"]] - m[["cb"]], 3)),
               `During the cartel` = c(usd(m[["td"]], 3), usd(m[["cd"]], 3), usd(m[["td"]] - m[["cd"]], 3)),
               Change = c(usd(m[["td"]] - m[["tb"]], 3), usd(m[["cd"]] - m[["cb"]], 3), usd(cm_e()$did, 3)),
               check.names = FALSE)
  }, striped = TRUE, width = "100%")

  # Tab 3 ---------------------------------------------------------------------
  st <- reactive(st_damages(input$st_n, input$st_c, input$st_model, g = if (is.null(input$st_g)) 3 else input$st_g))
  output$st_box_cartel <- renderUI(number_box("Cartel price", num(st()$p_cartel, 2), paste0("Quantity ", num(st()$Q, 0), " per year")))
  output$st_box_bf <- renderUI(number_box("But-for price", num(st()$bf, 2),
    switch(input$st_model, cournot = "Cournot competition", bertrand = "Bertrand competition, identical products", diff = paste0("Bertrand competition, differentiated products (\u03b3 = ", input$st_g, ")")), lab_colors[["pass"]]))
  output$st_box_oc <- renderUI(number_box("Overcharge per unit", num(st()$overcharge, 2),
    paste0(pct(st()$pct_bf), " of the but-for price, ", pct(st()$pct_cartel), " of the cartel price"), lab_colors[["actual"]]))
  output$st_box_dam <- renderUI(number_box("Damages over 5 years", num(st()$damages, 0), "Overcharge times quantity times 5", lab_colors[["actual"]]))

  output$st_plot <- renderPlot({
    c0 <- input$st_c; nn <- 2:10; k <- st_cartel(c0)
    g0 <- if (is.null(input$st_g)) 3 else input$st_g
    d <- rbind(data.frame(n = nn, p = st_cournot(nn, c0), model = "cournot"),
               data.frame(n = nn, p = st_bertrand(nn, c0), model = "bertrand"))
    if (input$st_model == "diff") d <- rbind(d, data.frame(n = nn, p = st_bertrand_diff(nn, c0, g0), model = "diff"))
    sel <- d[d$n == input$st_n & d$model == input$st_model, ]
    lx <- if (input$st_n >= 9) 6 else 10  # keep the label clear of the overcharge segment
    # Label placement for the differentiated-products curve: at the right end when it lies above Cournot there,
    # otherwise at the left end; the Cournot label moves below its line when the two curves are close.
    gap <- function(x) abs(st_bertrand_diff(x, c0, g0) - st_cournot(x, c0))
    cand <- c(2, 6, 10)
    free <- cand[!(cand == 2 & input$st_n <= 3) & !(cand == 6 & abs(input$st_n - 6) <= 1) & !(cand == 10 & input$st_n >= 8)]
    dx <- free[which.max(sapply(free, gap))]
    # the Cournot label goes to the side away from the dashed curve when the two are close or share a label position
    away <- if (st_bertrand_diff(lx, c0, g0) < st_cournot(lx, c0)) -0.9 else 1.7
    cv <- if (input$st_model == "diff" && (gap(lx) < 12 || dx == lx)) away else -0.9
    cx <- if (input$st_n >= 4 && input$st_n <= 8) 2 else 6  # cartel-price label, away from the segment
    g <- ggplot(d, aes(n, p, color = model, linetype = model)) +
      geom_hline(yintercept = k$p, color = lab_colors[["ink"]], linewidth = 1.2) +
      annotate("text", x = cx, y = k$p, label = paste("Cartel price", num(k$p, 2)), hjust = if (cx == 2) 0 else 0.5, vjust = -0.6, color = lab_colors[["ink"]], size = 4.8) +
      geom_line(linewidth = 1.2) + geom_point(size = 2.5)
    if (input$st_model == "diff")
      g <- g + annotate("text", x = dx, y = st_bertrand_diff(dx, c0, g0), label = "Bertrand, differentiated", hjust = if (dx == 2) 0 else if (dx == 6) 0.5 else 1, vjust = if (dx == lx && st_bertrand_diff(lx, c0, g0) < st_cournot(lx, c0)) 1.7 else -0.9,
                        color = lab_colors[["ink"]], size = 4.8)
    g +
      geom_segment(data = sel, aes(x = n, xend = n, y = p, yend = k$p), color = lab_colors[["actual"]], linewidth = 1.4, inherit.aes = FALSE) +
      geom_point(data = sel, size = 5.5) +
      annotate("text", x = input$st_n + 0.15, y = (sel$p + k$p) / 2, label = paste("Overcharge", num(k$p - sel$p, 2)),
               hjust = if (input$st_n >= 9) 1.15 else 0, color = lab_colors[["actual"]], size = 4.8) +
      annotate("text", x = lx, y = st_cournot(lx, c0), label = "Cournot", hjust = 1, vjust = cv, color = lab_colors[["pass"]], size = 4.8) +
      annotate("text", x = 10, y = c0, label = "Bertrand, identical products (price equals cost)", hjust = 1, vjust = 1.7, color = lab_colors[["muted"]], size = 4.8) +
      scale_color_manual(values = c(cournot = lab_colors[["pass"]], bertrand = lab_colors[["muted"]], diff = lab_colors[["ink"]]), guide = "none") +
      scale_linetype_manual(values = c(cournot = "solid", bertrand = "solid", diff = "dashed"), guide = "none") +
      scale_x_continuous(breaks = 2:10) +
      scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, 25)) +
      labs(x = "Number of sellers n", y = "Price") + lab_gg()
  })
  output$st_note <- renderUI({
    r <- st()
    card(card_body(p(paste0("Because of the higher price, buyers gave up ", num(r$lost_units, 1),
      " units per year that they would have bought at the but-for price. These units are not part of the damages above."))))
  })

  # Tab 4 ---------------------------------------------------------------------
  po <- reactive(po_harm(input$po_nd, input$po_dw))
  output$po_box_rho <- renderUI(number_box("Pass-on rate", pct(po()$rho),
    paste0("Their price rises by ", num(po()$p1 - po()$p0, 2), " when their cost rises by ", num(input$po_dw, 0),
           " (from ", num(po()$p0, 2), " to ", num(po()$p1, 2), ")"), lab_colors[["pass"]]))
  output$po_box_paid <- renderUI(number_box("Overcharge paid", num(po()$paid, 2),
    paste0(num(input$po_dw, 0), " per unit on ", num(po()$Q1, 2), " units")))
  output$po_box_net <- renderUI(number_box("Net loss of the direct purchasers", num(po()$net, 2),
    paste0("Their profit falls from ", num(po()$profit0, 2), " to ", num(po()$profit1, 2)), lab_colors[["actual"]]))
  output$po_box_ind <- renderUI(number_box("Passed on to indirect purchasers", num(po()$passed, 2),
    paste0("They also give up ", num(po()$Q0 - po()$Q1, 2), " units")))

  output$po_plot <- renderPlot({
    r <- po()
    lv <- c("Overcharge\npaid", "Passed on\n(pass-on effect)", "Profit lost on lost sales\n(output effect)", "Net loss")
    d <- data.frame(what = factor(lv, levels = lv), v = c(r$paid, r$passed, r$lost, r$net),
                    sign = c("loss", "gain", "loss", "net"),
                    lab = c(num(r$paid, 2), paste0("− ", num(r$passed, 2)), paste0("+ ", num(r$lost, 2)), paste0("= ", num(r$net, 2))))
    ggplot(d, aes(what, v, fill = sign)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = lab), vjust = -0.5, size = 5.2, color = lab_colors[["ink"]]) +
      scale_fill_manual(values = c(loss = lab_colors[["ink"]], gain = lab_colors[["fail"]], net = lab_colors[["actual"]]), guide = "none") +
      scale_y_continuous(limits = c(0, 800), expand = expansion(mult = c(0, 0.02))) +
      labs(x = NULL, y = NULL) + lab_gg()
  })

  output$po_claims <- renderPlot({
    r <- po()
    eu <- "European Union"; us <- "United States (federal law)"
    who <- c("Direct\npurchasers", "Indirect\npurchasers")
    d <- data.frame(panel = factor(c(eu, eu, us, us), levels = c(eu, us)),
                    who = factor(rep(who, 2), levels = who),
                    v = c(r$net, r$passed, 3 * r$paid, 0))
    d$lab <- c(num(r$net, 2), num(r$passed, 2), paste0("3 × ", num(r$paid, 2), " = ", num(3 * r$paid, 2)), "no claim for damages")
    ggplot(d, aes(who, v)) +
      geom_col(width = 0.55, fill = lab_colors[["pass"]]) +
      geom_text(aes(label = lab), vjust = -0.5, size = 5, color = lab_colors[["ink"]]) +
      facet_wrap(~panel, nrow = 1) +
      scale_y_continuous(limits = c(0, 2400), expand = expansion(mult = c(0, 0.02))) +
      labs(x = NULL, y = NULL) +
      lab_gg() + theme(strip.text = element_text(size = 15, face = "bold", hjust = 0), panel.spacing = unit(1.6, "lines"))
  })
  output$po_claims_note <- renderUI({
    r <- po()
    tagList(
      paste0("In the EU, the direct purchasers claim the overcharge they did not pass on (", num(r$paid - r$passed, 2),
             ") plus their lost profit (", num(r$lost, 2), "). The indirect purchasers claim the overcharge passed on to them. ",
             "The two claims add up to more than the overcharge paid because lost profit comes on top. Interest is left out. The loss of the customers who no longer buy (the deadweight loss) is rarely claimed. "),
      "In the US, the direct purchasers recover three times the full overcharge, and the defendant cannot argue that they passed it on. ",
      "Indirect purchasers cannot claim damages under federal law, although many states allow it under state law.")
  })
}

shinyApp(ui, server)
