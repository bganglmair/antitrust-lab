# Antitrust Lab: Merger simulator
library(shiny)
library(bslib)
library(ggplot2)

# Shiny sources R/ automatically (theme.R, merger.R, cases.R)

app_version <- "0.3 (2026-11-25)"
nmax <- 5
sig <- 0.05  # a price rise of 5 percent is treated as significant

verdict_box <- function(title, value, concern, note = NULL) {
  value_box(title = title, value = value,
    theme = if (isTRUE(concern)) value_box_theme(bg = lab_colors[["pass"]], fg = lab_colors[["pass_fg"]])
            else value_box_theme(bg = lab_colors[["fail"]], fg = lab_colors[["fail_fg"]]),
    if (!is.null(note)) p(note))
}
number_box <- function(title, value, note = NULL, color = lab_colors[["ink"]]) {
  value_box(title = title, value = value, theme = value_box_theme(bg = lab_colors[["bg"]], fg = color),
            if (!is.null(note)) p(note, style = paste0("color:", lab_colors[["muted"]])))
}
muted <- function(...) p(..., style = paste0("color:", lab_colors[["muted"]], "; font-size:.9rem;"))
pctx <- function(x, d = 1) paste0(num(100 * x, d), " percent")
signed <- function(x, d = 1) { v <- as.numeric(num(x, d)); paste0(ifelse(v > 0, "+", ifelse(v < 0, "−", "")), num(abs(v), d)) }
eur <- function(x) { v <- round(x / 1000) * 1000; paste0(ifelse(v < 0, "−", ifelse(v > 0, "+", "")), "€", formatC(abs(v), format = "d", big.mark = ",")) }
two <- function(a, b, f = function(x) num(x, 1)) if (f(a) == f(b)) paste0(f(a), "% each") else paste0(f(a), " and ", f(b), "%")
size_row <- function(i) conditionalPanel(paste0("input.ms_n >= ", i), div(class = "d-flex gap-2 align-items-center",
  div(style = "width:58%;", uiOutput(paste0("ms_lab", i))),
  div(style = "width:42%;", numericInput(paste0("ms_size", i), NULL, 20, min = 1, step = 0.25))))

# ---- Sidebar ----------------------------------------------------------------
model_tabs <- "input.tab == 't1' || input.tab == 't2' || input.tab == 't3' || input.tab == 't4'"
sb <- sidebar(
  width = 340,
  conditionalPanel(model_tabs,
    selectInput("ms_preset", "Example", setNames(names(ms_presets), sapply(ms_presets, `[[`, "label"))),
    uiOutput("ms_note")),
  conditionalPanel("input.tab == 't2' || input.tab == 't3'",
    sliderInput("ms_E", "Cost saving of the merging firms (percent of marginal cost)", 0, 60, 0, 1, post = "%", ticks = FALSE),
    muted("Both merging firms' marginal cost falls by this percentage.")),
  conditionalPanel("input.tab == 't4'",
    uiOutput("rm_sold_ui"),
    sliderInput("rm_x", "How much higher are the new owner's costs?", 0, 30, 0, 1, post = "%", ticks = FALSE),
    muted("At 0 the new owner is as good as the seller. This tab leaves out cost savings.")),
  conditionalPanel(model_tabs,
    accordion(open = FALSE, id = "ms_acc",
      accordion_panel("Change the market",
        selectInput("ms_n", "Number of firms", 3:5, 4),
        muted("Market share of each listed firm, in percent. If the shares do not add up to 100, the tool scales them."),
        size_row(1), size_row(2), size_row(3), size_row(4), size_row(5),
        uiOutput("ms_ab_ui"),
        sliderInput("ms_s0", "Customers who buy from none of the listed firms (outside option)", 5, 50, 20, 1, post = "%", ticks = FALSE),
        muted("The more customers buy elsewhere or not at all, the more sales the listed firms lose when all their prices rise."),
        uiOutput("ms_k_ui"),
        uiOutput("ms_mk_ui"),
        checkboxInput("ms_prices_on", "Firms charge different prices", FALSE),
        conditionalPanel("input.ms_prices_on", uiOutput("ms_price_ui")))),
    muted(em("Prices are in euros, the market has 1 million customers, and all numbers are illustrative."))),
  conditionalPanel("input.tab == 't5' || input.tab == 't6'", muted("This tab needs no settings."))
)

# ---- Tabs -------------------------------------------------------------------
warn_ui <- uiOutput("ms_warn")
tab1 <- nav_panel("1 Prices", value = "t1", warn_ui,
  card(card_header("What this tab does"),
    card_body(p("After the merger, the merged firm keeps the customers who switch from one of its products to the other, so a price rise becomes profitable and it raises both prices. Rivals then raise their prices too, by less. The diamonds show each merging firm's GUPPI, a quick screen that authorities compute before any simulation."))),
  layout_columns(fill = FALSE, col_widths = c(4, 4, 4), uiOutput("t1_box_ab"), uiOutput("t1_box_riv"), uiOutput("t1_box_g")),
  card(card_header("Price change after the merger"), plotOutput("t1_plot", height = "420px"),
       card_footer("Blue bars show price rises above 5 percent, sand bars smaller ones. The dashed line marks 5 percent."))
)

tab2 <- nav_panel("2 Cost savings", value = "t2", warn_ui,
  card(card_header("What this tab does"),
    card_body(p("A lower marginal cost, the cost of serving one more customer, pushes prices down and works against the price rise. Set a cost saving in the sidebar and see whether prices still rise."))),
  layout_columns(fill = FALSE, col_widths = c(6, 6), uiOutput("t2_box_need"), uiOutput("t2_box_price")),
  card(card_header("Price change of the merging firms for each cost saving"), plotOutput("t2_plot", height = "420px"),
       card_footer("Above the zero line customers pay more than before the merger. The dots mark the saving at which each price is back at its old level. The orange line is your saving."))
)

tab3 <- nav_panel("3 Who gains, who loses", value = "t3", warn_ui,
  card(card_header("What this tab does"),
    card_body(p("Customers lose from higher prices. The merging firms and their rivals gain. With a cost saving (sidebar), total welfare can rise while customers still lose, so the welfare standard can decide the case."))),
  layout_columns(fill = FALSE, col_widths = c(6, 6), uiOutput("t3_box_cs"), uiOutput("t3_box_tw")),
  card(card_header("Change in consumer surplus and profits, in euros"), plotOutput("t3_plot", height = "380px"),
       card_footer("A market of 1 million customers. Blue bars are losses, sand bars gains. Consumer surplus is what customers would be willing to pay minus what they pay."))
)

tab4 <- nav_panel("4 Divestiture", value = "t4", warn_ui,
  card(card_header("What this tab does"),
    card_body(p("A competition authority can clear the merger on condition that one product is sold to a new owner. If the new owner competes as well as the seller did, prices return to their old level. A weaker new owner restores less."))),
  layout_columns(fill = FALSE, col_widths = c(6, 6), uiOutput("t4_box_2"), uiOutput("t4_box_3")),
  card(card_header("Price change compared with before the merger"), plotOutput("t4_plot", height = "380px"),
       card_footer("The left panel shows the merger without a remedy, the right panel with the divestiture. Blue bars show price rises above 5 percent."))
)

experiment <- function(id, question, choices, answer) {
  card(card_header(question),
    radioButtons(paste0("ex_", id), "Your prediction", choices, selected = character(0), width = "100%"),
    actionButton(paste0("exb_", id), "Reveal", class = "btn-outline-primary btn-sm"),
    conditionalPanel(paste0("input.exb_", id, " > 0"), div(style = "margin-top:.6rem;", answer)))
}

tab5 <- nav_panel("Questions", value = "t5",
  card(card_header("What this tab does"),
    card_body(p("Predict first, then check with the tool, then reveal. All questions use the examples as loaded, in which 20 percent of customers buy from none of the listed firms, every price is €10 and Firm 1 has a margin of 40 percent."))),
  layout_columns(col_widths = c(6, 6),
    experiment("1", "In tab 1, with \"Four equal firms\", firms 1 and 2 merge. Do the rivals' prices change?",
      c("Yes, they rise a little", "No, they stay the same", "Yes, they fall"),
      p(strong("Yes, they rise a little. "), "The merging firms raise their prices by 8.6 percent. Some of their customers move to the rivals, and the rivals answer by raising their own prices by 0.9 percent.")),
    experiment("2", "In tab 1, compare \"Four equal firms\" with \"The two largest firms merge\". In which example do the merging firms raise their prices more?",
      c("The two largest firms", "Four equal firms", "The same"),
      p(strong("The two largest firms. "), "Firms 1 and 2 raise their prices by 10.8 and 13.4 percent, against 8.6 percent with four equal firms. Larger partners take more of each other's lost customers.")),
    experiment("3", "In tab 1, a large firm buys a small rival. The acquirer's GUPPI is 4.4 percent, below 5 percent. Is the merger harmless for prices?",
      c("No, the small rival's price rises a lot", "Yes, prices barely change", "No, but only the acquirer's price rises"),
      p(strong("No, the small rival's price rises a lot. "), "The screen must be run for both firms. The small rival's GUPPI is 13.3 percent, and its price rises by 12.4 percent. The acquirer's price rises by 3.6 percent, close to its GUPPI.")),
    experiment("4", "In tab 2, with \"The two largest firms merge\", the merger cuts the merging firms' marginal cost by 10 percent. Do their prices still rise?",
      c("Yes, but by less", "No, they fall", "Yes, by as much as before"),
      p(strong("Yes, but by less. "), "The prices rise by 7.5 and 9.9 percent instead of 10.8 and 13.4. Firm 1's price is back at its old level only with a saving of 35.6 percent, and Firm 2's with 41.3 percent.")),
    experiment("5", "In tab 3, with \"Four equal firms\" and no cost saving, who gains from the merger?",
      c("The merging firms and their rivals", "Only the merging firms", "Nobody"),
      p(strong("The merging firms and their rivals. "), "Rivals do not take part in the merger, yet they gain, because they can raise their prices too. Customers lose more than the firms gain.")),
    experiment("6", "In tab 3, with \"Four equal firms\" and a 10 percent cost saving, do the two welfare standards agree?",
      c("No, customers lose but total welfare rises", "Yes, both count the merger as harmful", "Yes, both count the merger as beneficial"),
      p(strong("No, customers lose but total welfare rises. "), "Consumer surplus falls by about €198,000, and total welfare rises by about €163,000, because the firms' gains include the cost saving. Which standard applies decides the case.")),
    experiment("7", "In tab 4, with \"The two largest firms merge\", Firm 2's product is sold to a new owner whose costs are 10 percent higher. Is competition fully restored?",
      c("No, prices stay higher than before", "Yes, prices return to their old level", "No, prices are higher than without the remedy"),
      p(strong("No, prices stay higher than before. "), "The price of the product sold rises by 4.9 percent. Consumer surplus falls by about €135,000, against €625,000 without a remedy. A remedy is only as good as the new owner.")),

  )
)

tab6 <- nav_panel("Math", value = "t6",
  withMathJax(),
  card(card_header("Notation"),
    p("Firm \\(j\\) sells one product at price \\(p_j\\) with marginal cost \\(c_j\\) and margin \\(m_j = (p_j - c_j)/p_j\\). \\(s_j\\) is its share of all customers and \\(s_0\\) the share of customers who buy from none of the listed firms (the outside option). The shares entered in the tool are shares of the listed firms, so \\(s_j = (1 - s_0) \\times\\) the entered share. \\(\\theta\\) is the customers' price sensitivity. \\(A\\) and \\(B\\) are the merging firms, \\(D_{AB}\\) the diversion ratio from \\(A\\) to \\(B\\), and \\(E\\) the cost saving as a share of marginal cost.")),
  card(card_header("Demand and calibration"),
    p("With logit demand (Werden and Froeb 1994), $$s_j = \\frac{e^{\\delta_j - \\theta p_j}}{1 + \\sum_l e^{\\delta_l - \\theta p_l}}, \\qquad s_0 = \\frac{1}{1 + \\sum_l e^{\\delta_l - \\theta p_l}},$$ where \\(\\delta_j\\) is the quality of product \\(j\\). Before the merger each firm sets its price on its own, so $$p_j - c_j = \\frac{1}{\\theta\\,(1 - s_j)}.$$ One observed margin \\(m_k\\) gives \\(\\theta = 1/(m_k\\,p_k\\,(1 - s_k))\\). The other margins and costs follow, and \\(\\delta_j = \\ln(s_j/s_0) + \\theta p_j\\). If all prices rise by 1 percent, sales of the listed firms fall by about \\(\\theta\\,\\bar p\\,s_0\\) percent, where \\(\\bar p\\) is the share-weighted average price. The tool keeps the observed margin low enough that every implied cost is above zero.")),
  card(card_header("After the merger (tab 1)"),
    p("The merged firm sets the same markup on both products, $$p_A - c_A = p_B - c_B = \\frac{1}{\\theta\\,(1 - s_A - s_B)}.$$ Every other firm keeps its own condition. The tool solves all conditions together, step by step until prices no longer change. Diversion follows shares, so \\(D_{AB} = s_B/(1 - s_A)\\), and \\(\\text{GUPPI}_A = D_{AB}\\,(p_B - c_B)/p_A\\) (Farrell and Shapiro 2010). ")),
  card(card_header("Cost savings (tab 2)"),
    p("Prices stay at their old level if the merged firm's condition holds at the old prices and shares. If each firm gets its own saving, the needed cost is \\(c_j' = p_j - 1/(\\theta(1 - s_A - s_B))\\) for \\(j = A, B\\), and the needed saving is \\(1 - c_j'/c_j\\) (Werden 1996). The tool applies the same saving \\(E\\) to both firms. It then finds, for each firm, the \\(E\\) at which that firm's price is back at its old level. Pass-through is the fall in price caused by the saving divided by the fall in marginal cost.")),
  card(card_header("Welfare and divestiture (tabs 3 and 4)"),
    p("Consumer surplus per customer is \\(\\ln\\bigl(1 + \\sum_j e^{\\delta_j - \\theta p_j}\\bigr)/\\theta\\), and its change is multiplied by 1 million customers. Profits are \\((p_j - c_j)\\,s_j\\) times 1 million. In a divestiture the product sold gets an owner of its own, with marginal cost higher by the chosen share.")),
  card(card_header("Limits"),
    p("Logit lets diversion follow shares. Real customers often see some products as closer than their shares suggest, and then the price rise is larger. The size of the price rise also depends on the shape of demand, and logit, linear and other demand systems can give different answers for the same shares and margins. Logit gives every product a group of loyal buyers, so exit and new products have large effects on consumer surplus. Each firm sells one product, and entry, capacity limits and coordinated effects are not modeled.")),
  card(card_header("Sources"),
    tags$ul(
      lab_ref("Deneckere, Raymond, and Carl Davidson. 1985. \"Incentives to Form Coalitions with Bertrand Competition.\" ", em("RAND Journal of Economics"), " 16 (4): 473–486.", url = "https://doi.org/10.2307/2555507"),
      lab_ref("European Commission. 2004. \"Guidelines on the Assessment of Horizontal Mergers under the Council Regulation on the Control of Concentrations between Undertakings.\" ", em("Official Journal of the European Union"), " C 31: 5–18.", url = "https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:52004XC0205(02)"),
      lab_ref("Farrell, Joseph, and Carl Shapiro. 2010. \"Antitrust Evaluation of Horizontal Mergers: An Economic Alternative to Market Definition.\" ", em("B.E. Journal of Theoretical Economics"), " 10 (1): Article 9.", url = "https://doi.org/10.2202/1935-1704.1563"),
      lab_ref("FTC v. Staples, Inc., 970 F. Supp. 1066 (D.D.C. 1997).", url = "https://law.justia.com/cases/federal/district-courts/FSupp/970/1066/1639260/"),
      lab_ref("U.S. Department of Justice and Federal Trade Commission. 2023. \"Merger Guidelines.\" December 18.", url = "https://www.justice.gov/d9/2023-12/2023%20Merger%20Guidelines.pdf"),
      lab_ref("Werden, Gregory J. 1996. \"A Robust Test for Consumer Welfare Enhancing Mergers among Sellers of Differentiated Products.\" ", em("Journal of Industrial Economics"), " 44 (4): 409–413.", url = "https://doi.org/10.2307/2950522"),
      lab_ref("Werden, Gregory J., and Luke M. Froeb. 1994. \"The Effects of Mergers in Differentiated Products Industries: Logit Demand and Merger Policy.\" ", em("Journal of Law, Economics, and Organization"), " 10 (2): 407–426.", url = "https://doi.org/10.1093/oxfordjournals.jleo.a036857")))
)

ui <- page_navbar(
  id = "tab", fillable = FALSE,
  title = lab_title("Merger simulator"), window_title = "Antitrust Lab",
  theme = lab_theme(), navbar_options = lab_navbar(), sidebar = sb,
  header = div(class = "container-fluid",
    lab_header("Will this merger raise prices?", "Topic: horizontal mergers and merger control", tool = "Merger simulator")),
  footer = div(class = "container-fluid", style = paste0("color:", lab_colors[["muted"]], "; font-size:.85rem; padding:.5rem 0;"),
    paste("Version", app_version, "· All shares, prices, margins and costs are illustrative.")),
  tab1, tab2, tab3, tab4, tab5, tab6
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {
  M <- 1e6  # customers
  same <- function(a, b) length(a) == length(b) && all(!is.na(a)) && isTRUE(all.equal(as.numeric(a), as.numeric(b)))
  names_now <- reactive({ s <- ms_presets[[input$ms_preset]]; n <- as.integer(input$ms_n)
    if (length(s$names) >= n && (n == length(s$size) || all(grepl("^Firm ", s$names)))) s$names[seq_len(n)] else paste("Firm", seq_len(n)) })

  # Presets
  observeEvent(input$ms_preset, {
    s <- ms_presets[[input$ms_preset]]; d <- ms_preset_defaults; n <- length(s$size)
    nm <- s$names[seq_len(n)]
    updateSelectInput(session, "ms_n", selected = n)
    for (i in seq_len(nmax)) updateNumericInput(session, paste0("ms_size", i), value = if (i <= n) s$size[i] else 10)
    for (i in seq_len(nmax)) updateNumericInput(session, paste0("ms_p", i), value = d$p)
    updateSliderInput(session, "ms_s0", value = d$s0)
    updateCheckboxInput(session, "ms_prices_on", value = FALSE)
    updateSelectInput(session, "ms_A", choices = setNames(seq_len(n), nm), selected = s$A)
    updateSelectInput(session, "ms_B", choices = setNames(seq_len(n), nm)[-s$A], selected = s$B)
    updateSelectInput(session, "ms_k", choices = setNames(seq_len(n), nm), selected = d$k)
    updateSliderInput(session, "ms_mk", value = d$mk)
  })
  for (i in seq_len(nmax)) local({ j <- i; output[[paste0("ms_lab", j)]] <- renderUI(tags$label(names_now()[j], style = "margin:0;")) })
  lastp <- reactiveVal("")
  output$ms_ab_ui <- renderUI({ nm <- names_now(); pr <- input$ms_preset; s <- ms_presets[[pr]]
    ch <- setNames(seq_along(nm), nm)
    if (pr != isolate(lastp())) { A <- s$A; B <- s$B; lastp(pr) } else {
      A <- isolate(if (is.null(input$ms_A)) s$A else as.integer(input$ms_A)); B <- isolate(if (is.null(input$ms_B)) s$B else as.integer(input$ms_B)) }
    if (A > length(nm)) A <- 1
    if (B > length(nm) || B == A) B <- setdiff(seq_along(nm), A)[1]
    div(class = "d-flex gap-2",
      div(style = "width:50%;", selectInput("ms_A", "Acquirer (A)", ch, A)),
      div(style = "width:50%;", selectInput("ms_B", "Target (B)", ch[-A], B))) })
  observeEvent(input$ms_A, { nm <- names_now(); A <- as.integer(input$ms_A); ch <- setNames(seq_along(nm), nm)[-A]
    B <- if (!is.null(input$ms_B) && as.integer(input$ms_B) != A && as.integer(input$ms_B) <= length(nm)) input$ms_B else ch[1]
    updateSelectInput(session, "ms_B", choices = ch, selected = B) }, ignoreInit = TRUE)
  output$ms_k_ui <- renderUI({ nm <- names_now(); k <- isolate(if (is.null(input$ms_k)) 1 else min(as.integer(input$ms_k), length(nm)))
    selectInput("ms_k", "Whose margin is observed?", setNames(seq_along(nm), nm), k) })
  output$ms_mk_ui <- renderUI({ v <- isolate(if (is.null(input$ms_mk)) 40 else input$ms_mk)
    sliderInput("ms_mk", "Observed margin (price minus marginal cost, as a share of the price)", 5, 80, v, 1, post = "%", ticks = FALSE) })
  output$ms_price_ui <- renderUI({ nm <- names_now()
    lapply(seq_along(nm), function(i) numericInput(paste0("ms_p", i), paste0("Price of ", nm[i], " (€)"), isolate(if (is.null(input[[paste0("ms_p", i)]])) 10 else input[[paste0("ms_p", i)]]), min = 1, step = 0.5)) })

  # These controls sit in the collapsed panel "Change the market". Render them even while it is closed.
  for (o in c("ms_ab_ui", "ms_k_ui", "ms_mk_ui", "ms_price_ui", paste0("ms_lab", seq_len(nmax)))) outputOptions(output, o, suspendWhenHidden = FALSE)
  inputs <- reactive({
    n <- as.integer(input$ms_n); req(input$ms_A, input$ms_B, input$ms_k, input$ms_mk)
    size <- sapply(seq_len(n), function(i) { v <- input[[paste0("ms_size", i)]]; if (is.null(v)) NA else v })
    p <- if (isTRUE(input$ms_prices_on)) sapply(seq_len(n), function(i) { v <- input[[paste0("ms_p", i)]]; if (is.null(v)) 10 else v }) else rep(10, n)
    validate(need(all(is.finite(size) & size > 0), "Enter a market share above zero for each firm."),
             need(all(is.finite(p) & p > 0), "Enter a price above zero for each firm."))
    A <- as.integer(input$ms_A); B <- as.integer(input$ms_B); k <- as.integer(input$ms_k)
    validate(need(A <= n && B <= n && A != B && k <= n, "Choose two different merging firms."))
    s0 <- input$ms_s0 / 100
    mmax <- ms_max_margin(size, s0, p, k)
    list(n = n, size = size, p = p, A = A, B = B, k = k, s0 = s0, mk = min(input$ms_mk / 100, 0.98 * mmax), mmax = mmax)
  })
  observe({ x <- inputs(); top <- max(6, min(80, floor(100 * 0.98 * x$mmax))); updateSliderInput(session, "ms_mk", max = top) })
  base <- reactive({ x <- inputs(); ms_calib(x$size, x$s0, x$p, x$k, x$mk) })
  merged <- reactive({ x <- inputs(); ms_merge(base(), x$A, x$B, 0) })
  merged_E <- reactive({ x <- inputs(); ms_merge(base(), x$A, x$B, input$ms_E / 100) })
  cs0 <- reactive({ k <- base(); ms_cs(k$p, k$theta, k$delta) })
  chg <- function(p1, p0) p1 / p0 - 1
  nm2 <- function(x) names_now()[c(x$A, x$B)]

  output$ms_note <- renderUI({ s <- ms_presets[[input$ms_preset]]; x <- inputs()
    ok <- x$n == length(s$size) && same(x$size, s$size) && x$A == s$A && x$B == s$B && x$k == 1 && same(input$ms_mk, 40) && same(input$ms_s0, 20) && !isTRUE(input$ms_prices_on)
    muted(if (ok) s$note else "You changed the numbers of this example.") })
  output$ms_summary <- renderUI({ x <- inputs(); nm <- names_now()
    muted(paste0(x$n, " firms with shares ", paste(num(100 * x$size / sum(x$size), 1), collapse = ", "), " percent. ",
                 nm[x$A], " buys ", nm[x$B], ". Observed margin of ", nm[x$k], " ", num(100 * x$mk, 0), " percent. ",
                 num(100 * x$s0, 0), " percent of customers buy from none of the listed firms.")) })
  output$ms_warn <- renderUI({ if (!merged()$converged || !merged_E()$converged)
    div(class = "alert alert-warning", "The calculation did not settle for these numbers, so treat the results with care.") })

  # Tab 1 ---------------------------------------------------------------------
  output$t1_box_ab <- renderUI({ x <- inputs(); k <- base(); r <- merged(); v <- chg(r$p[c(x$A, x$B)], k$p[c(x$A, x$B)])
    verdict_box("Price rise of the merging firms", two(100 * v[1], 100 * v[2]), any(v > sig), paste(nm2(x), collapse = " and ")) })
  output$t1_box_riv <- renderUI({ x <- inputs(); k <- base(); r <- merged(); riv <- setdiff(seq_len(x$n), c(x$A, x$B))
    v <- 100 * chg(r$p[riv], k$p[riv])
    verdict_box("Price rise of the rivals", if (num(min(v), 1) == num(max(v), 1)) paste0(num(v[1], 1), "%") else paste0(num(min(v), 1), " to ", num(max(v), 1), "%"),
      any(v > 100 * sig), "Rivals raise their prices too, by less") })
  output$t1_box_g <- renderUI({ x <- inputs(); g <- ms_guppi(base(), x$A, x$B)
    verdict_box("GUPPI of the merging firms", two(100 * g[["A"]], 100 * g[["B"]]), any(g >= sig),
      "A quick screen. Below about 5 percent, a price rise is often thought unlikely") })

  output$t1_plot <- renderPlot({
    x <- inputs(); k <- base(); r <- merged(); nm <- names_now()
    ord <- c(x$A, x$B, setdiff(seq_len(x$n), c(x$A, x$B)))
    d <- data.frame(firm = factor(nm[ord], levels = nm[ord]), v = 100 * chg(r$p[ord], k$p[ord]),
                    grp = factor(ifelse(ord %in% c(x$A, x$B), "Merging firms", "Rivals"), levels = c("Merging firms", "Rivals")))
    g <- ms_guppi(k, x$A, x$B); gd <- data.frame(firm = factor(nm[c(x$A, x$B)], levels = nm[ord]), v = 100 * g, grp = factor("Merging firms", levels = levels(d$grp)))
    top <- max(25, ceiling(max(c(d$v, gd$v)) / 5) * 5 + 5)
    ggplot(d, aes(firm, v)) +
      geom_hline(yintercept = 100 * sig, linetype = "dashed", color = lab_colors[["ink"]]) +
      geom_col(aes(fill = v > 100 * sig), width = 0.6, color = lab_colors[["ink"]], linewidth = 0.3) +
      geom_text(aes(label = paste0(signed(v), "%")), vjust = -0.6, size = 5, color = lab_colors[["ink"]]) +
      geom_point(data = gd, aes(firm, v), shape = 23, size = 4.5, fill = lab_colors[["actual"]], color = lab_colors[["ink"]], position = position_nudge(x = 0.38)) +
      scale_fill_manual(values = c(`TRUE` = lab_colors[["pass"]], `FALSE` = lab_colors[["fail"]]), guide = "none") +
      scale_y_continuous(limits = c(0, top), expand = expansion(mult = c(0, 0.02))) +
      facet_grid(~ grp, scales = "free_x", space = "free_x") +
      labs(x = NULL, y = "Price change after the merger (percent)") + lab_gg() +
      theme(axis.text.x = element_text(size = 13), strip.text = element_text(size = 13, face = "bold"))
  })

  output$t1_screens <- renderUI({
    x <- inputs(); k <- base(); r <- merged(); nm <- names_now()
    h <- ms_hhi(k$inside, x$A, x$B); g <- ms_guppi(k, x$A, x$B)
    shA <- 100 * k$inside[x$A]; shB <- 100 * k$inside[x$B]
    us <- h[["delta"]] > 100 && (h[["post"]] > 1800 || shA + shB > 30)
    eu_ok <- (shA + shB < 25) || h[["post"]] < 1000 ||
      (max(shA, shB) < 50 && ((h[["post"]] <= 2000 && h[["delta"]] < 250) || (h[["post"]] > 2000 && h[["delta"]] < 150)))
    chip <- function(txt, concern) tags$span(txt, style = paste0("padding:.1rem .45rem; border-radius:.3rem; background:", if (concern) lab_colors[["pass"]] else lab_colors[["fail"]],
                                                                  "; color:", if (concern) lab_colors[["pass_fg"]] else lab_colors[["fail_fg"]], ";"))
    row <- function(a, b) tags$tr(tags$td(a), tags$td(style = "text-align:right;", b))
    tagList(
      tags$table(class = "table table-sm", style = "margin-bottom:.5rem;",
        row("HHI before and after", paste0(num(h[["pre"]], 0), " → ", num(h[["post"]], 0))),
        row("Change in HHI", num(h[["delta"]], 0)),
        row("US 2023 Merger Guidelines", chip(if (us) "Presumed to lessen competition" else "No presumption", us)),
        row("EU 2004 Horizontal Merger Guidelines", chip(if (eu_ok) "Concerns unlikely" else "Concerns possible", !eu_ok)),
        row(paste0("Diversion ratio from ", nm[x$A], " to ", nm[x$B]), pctx(ms_diversion(k$s, x$A, x$B))),
        row(paste0("Diversion ratio from ", nm[x$B], " to ", nm[x$A]), pctx(ms_diversion(k$s, x$B, x$A))),
        row(paste0("GUPPI of ", nm[x$A]), chip(pctx(g[["A"]]), g[["A"]] >= sig)),
        row(paste0("Simulated price rise of ", nm[x$A]), pctx(chg(r$p[x$A], k$p[x$A]))),
        row(paste0("GUPPI of ", nm[x$B]), chip(pctx(g[["B"]]), g[["B"]] >= sig)),
        row(paste0("Simulated price rise of ", nm[x$B]), pctx(chg(r$p[x$B], k$p[x$B])))),
      muted(paste0("The HHI treats the listed firms as the whole market, so with five or fewer firms it is always at least 2,000. If all prices rise by 1 percent, the listed firms lose about ",
                   num(ms_market_elast(k), 2), " percent of their sales.")))
  })

  sens_plot <- function(d, xlab, xnow, ynow) {
    ggplot(d, aes(xv, v, linetype = who)) +
      geom_hline(yintercept = 100 * sig, linetype = "dashed", color = lab_colors[["muted"]]) +
      geom_line(linewidth = 1.2, color = lab_colors[["ink"]]) +
      geom_point(data = data.frame(xv = xnow, v = ynow, who = names(ynow)), color = lab_colors[["actual"]], size = 4) +
      scale_linetype_manual(values = c("solid", "longdash"), name = NULL) +
      scale_y_continuous(limits = c(0, max(25, max(d$v) * 1.1))) +
      labs(x = xlab, y = "Price rise (percent)") + lab_gg() + theme(legend.position = "top", legend.key.width = grid::unit(2.5, "lines"))
  }
  output$t1_sens_m <- renderPlot({
    x <- inputs(); nm <- names_now(); top <- min(0.8, 0.98 * x$mmax); grid <- seq(0.1, top, length.out = 15)
    d <- do.call(rbind, lapply(grid, function(m) { k <- ms_calib(x$size, x$s0, x$p, x$k, m); r <- ms_merge(k, x$A, x$B)
      data.frame(xv = 100 * m, v = 100 * chg(r$p[c(x$A, x$B)], k$p[c(x$A, x$B)]), who = nm2(x)) }))
    k <- base(); r <- merged(); y <- setNames(100 * chg(r$p[c(x$A, x$B)], k$p[c(x$A, x$B)]), nm2(x))
    sens_plot(d, paste0("Observed margin of ", nm[x$k], " (percent)"), 100 * x$mk, y)
  })
  output$t1_sens_s0 <- renderPlot({
    x <- inputs()
    d <- do.call(rbind, lapply(seq(0.05, 0.5, 0.05), function(z) {
      m <- min(x$mk, 0.98 * ms_max_margin(x$size, z, x$p, x$k)); k <- ms_calib(x$size, z, x$p, x$k, m); r <- ms_merge(k, x$A, x$B)
      data.frame(xv = 100 * z, v = 100 * chg(r$p[c(x$A, x$B)], k$p[c(x$A, x$B)]), who = nm2(x)) }))
    k <- base(); r <- merged(); y <- setNames(100 * chg(r$p[c(x$A, x$B)], k$p[c(x$A, x$B)]), nm2(x))
    sens_plot(d, "Customers who buy from none of the listed firms (percent)", 100 * x$s0, y)
  })

  # Tab 2 ---------------------------------------------------------------------
  cross <- reactive({ x <- inputs(); ms_cross(base(), x$A, x$B) })
  output$t2_box_need <- renderUI({ x <- inputs(); cr <- cross(); nm <- nm2(x)
    f <- function(v) if (is.na(v)) "above 95" else num(100 * v, 1)
    number_box("Cost saving at which each price is back at its old level",
      if (f(cr[1]) == f(cr[2])) paste0(f(cr[1]), "% each") else paste0(f(cr[1]), " and ", f(cr[2]), "%"),
      paste0("Percent of marginal cost, for ", nm[1], " and ", nm[2], ". Neither price rises from ", f(max(cr)), " percent.")) })
  output$t2_box_price <- renderUI({ x <- inputs(); k <- base(); r <- merged_E(); v <- 100 * chg(r$p[c(x$A, x$B)], k$p[c(x$A, x$B)])
    E <- input$ms_E
    verdict_box(if (E == 0) "Price change without a cost saving" else paste0("Price change with a saving of ", E, " percent"),
      if (signed(v[1]) == signed(v[2])) paste0(signed(v[1]), "% each") else paste0(signed(v[1]), " and ", signed(v[2]), "%"),
      any(v > 0.05), if (all(v <= 0.05)) "Prices do not rise, so customers are no worse off" else "Prices still rise") })
  output$t2_box_pass <- renderUI({ x <- inputs(); k <- base(); r0 <- merged(); r <- merged_E(); E <- input$ms_E / 100
    if (E == 0) return(number_box("Pass-through of the saving", "—", "Set a cost saving in the sidebar"))
    pass <- (r0$p[c(x$A, x$B)] - r$p[c(x$A, x$B)]) / (k$c[c(x$A, x$B)] * E)
    number_box("Pass-through of the saving", two(100 * pass[1], 100 * pass[2], function(z) num(z, 0)),
      "Fall in price divided by the fall in marginal cost") })
  output$t2_plot <- renderPlot({
    x <- inputs(); k <- base(); nm <- nm2(x); E <- input$ms_E; cr <- cross()
    d <- do.call(rbind, lapply(seq(0, 60, 2), function(e) { r <- ms_merge(k, x$A, x$B, e / 100)
      data.frame(E = e, v = 100 * chg(r$p[c(x$A, x$B)], k$p[c(x$A, x$B)]), who = nm) }))
    ylo <- min(-30, min(d$v)); yhi <- max(25, max(d$v) + 4)
    lab <- d[d$E == 60, ]
    if (abs(lab$v[1] - lab$v[2]) < 2.5) lab <- data.frame(E = 60, v = mean(lab$v), who = paste(nm, collapse = " and "))
    lab$vj <- if (nrow(lab) == 2) ifelse(lab$v == max(lab$v), -0.8, 1.6) else -0.8
    dots <- data.frame(E = 100 * cr, v = 0, who = nm); dots <- dots[!is.na(dots$E) & dots$E <= 60, ]
    if (nrow(dots) == 2 && abs(dots$E[1] - dots$E[2]) < 0.05) dots <- dots[1, ]
    ggplot(d, aes(E, v)) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf, fill = lab_colors[["pass"]], alpha = 0.08) +
      annotate("text", x = 1, y = yhi * 0.95, label = "customers pay more than before", hjust = 0, color = lab_colors[["pass"]], size = 4.6) +
      annotate("text", x = 1, y = ylo * 0.92, label = "customers pay less than before", hjust = 0, color = lab_colors[["muted"]], size = 4.6) +
      geom_hline(yintercept = 0, color = lab_colors[["ink"]]) +
      geom_vline(xintercept = E, color = lab_colors[["actual"]], linewidth = 1.1) +
      annotate("text", x = E, y = yhi * 0.8, label = paste0("your saving: ", E, "%"), hjust = if (E > 45) 1.05 else -0.05, color = lab_colors[["actual"]], size = 4.6) +
      geom_line(aes(linetype = who), linewidth = 1.3, color = lab_colors[["ink"]]) +
      geom_point(data = dots, size = 3.5, color = lab_colors[["ink"]]) +
      geom_text(data = dots, aes(label = paste0(num(E, 1), "%")), vjust = 1.8, size = 4.4, color = lab_colors[["ink"]]) +
      scale_linetype_manual(values = setNames(c("solid", "longdash"), nm), name = NULL) +
      scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 10)) +
      scale_y_continuous(limits = c(ylo, yhi)) +
      labs(x = "Cost saving of the merging firms (percent of marginal cost)", y = "Price change after the merger (percent)") + lab_gg() +
      theme(legend.position = "top", legend.key.width = grid::unit(2.5, "lines"))
  })

  # Tab 3 ---------------------------------------------------------------------
  welfare <- function(r) { x <- inputs(); k <- base(); pre <- (k$p - k$c) * k$s; post <- r$profit
    par <- c(x$A, x$B); riv <- setdiff(seq_len(x$n), par)
    v <- c(Customers = (r$cs - cs0()), `Merging firms` = sum(post[par]) - sum(pre[par]), Rivals = sum(post[riv]) - sum(pre[riv]))
    c(v, Total = sum(v)) * M }
  wlim <- reactive({ x <- inputs(); k <- base()
    max(abs(c(welfare(merged()), welfare(ms_merge(k, x$A, x$B, 0.6))))) * 1.35 / 1000 })
  output$t3_box_cs <- renderUI({ w <- welfare(merged_E())
    verdict_box("Change in consumer surplus", eur(w[["Customers"]]), w[["Customers"]] < 0, if (w[["Customers"]] < 0) "Customers lose" else "Customers gain") })
  output$t3_box_tw <- renderUI({ w <- welfare(merged_E())
    verdict_box("Change in total welfare", eur(w[["Total"]]), w[["Total"]] < 0, "Customers, merging firms and rivals together") })
  output$t3_box_verdict <- renderUI({ w <- welfare(merged_E()); cl <- w[["Customers"]] < 0; tl <- w[["Total"]] < 0
    verdict_box("Consumer standard and total standard",
      paste0(if (cl) "Harmful" else "Beneficial", " / ", if (tl) "harmful" else "beneficial"), cl,
      if (cl == tl) "The two standards agree" else "The two standards disagree, so the choice of standard decides") })
  output$t3_plot <- renderPlot({
    E <- input$ms_E
    ws <- list(welfare(merged())); lbl <- "Without a cost saving"
    if (E > 0) { ws[[2]] <- welfare(merged_E()); lbl <- c(lbl, paste0("With a cost saving of ", E, " percent")) }
    d <- do.call(rbind, lapply(seq_along(ws), function(i) data.frame(grp = factor(names(ws[[i]]), levels = rev(names(ws[[i]]))), v = ws[[i]] / 1000, sc = lbl[i])))
    d$sc <- factor(d$sc, levels = lbl); lim <- wlim()
    ggplot(d, aes(v, grp)) + geom_vline(xintercept = 0, color = lab_colors[["muted"]]) +
      geom_col(aes(fill = v < 0), width = 0.6, color = lab_colors[["ink"]], linewidth = 0.3) +
      geom_text(aes(label = sapply(v * 1000, eur), hjust = ifelse(v < 0, 1.1, -0.1)), size = 4.6, color = lab_colors[["ink"]]) +
      scale_fill_manual(values = c(`TRUE` = lab_colors[["pass"]], `FALSE` = lab_colors[["fail"]]), guide = "none") +
      scale_x_continuous(limits = c(-lim, lim)) + facet_wrap(~ sc, ncol = 1) +
      labs(x = "Change in thousand euros", y = NULL) + lab_gg() + theme(strip.text = element_text(size = 13, face = "bold"), axis.text.y = element_text(size = 13))
  })

  # Tab 4 ---------------------------------------------------------------------
  output$rm_sold_ui <- renderUI({ x <- inputs(); nm <- names_now()
    cur <- isolate(input$rm_sold); sel <- if (!is.null(cur) && as.integer(cur) %in% c(x$A, x$B)) cur else x$B
    selectInput("rm_sold", "Which product is sold?", setNames(c(x$A, x$B), nm[c(x$A, x$B)]), sel) })
  alt <- reactive({ x <- inputs(); k <- base()
    if (TRUE) { sold <- if (is.null(input$rm_sold) || !(as.integer(input$rm_sold) %in% c(x$A, x$B))) x$B else as.integer(input$rm_sold)
      ms_divest(k, x$A, x$B, sold, input$rm_x / 100) } else ms_exit(k, x$B) })
  output$t4_box_1 <- renderUI({ v <- (merged()$cs - cs0()) * M
    verdict_box("Consumer surplus, merger alone", eur(v), v < -500, "Compared with before the merger") })
  output$t4_box_2 <- renderUI({ v <- (alt()$cs - cs0()) * M
    verdict_box(if (TRUE) "Consumer surplus, with the divestiture" else "Consumer surplus, if the target leaves", eur(v), v < -500,
      "Compared with before the merger") })
  output$t4_box_3 <- renderUI({ m <- (merged()$cs - cs0()) * M; a <- (alt()$cs - cs0()) * M
    if (TRUE) {
      undone <- if (m < 0) (a - m) / (-m) else NA
      number_box("Share of the customers' loss the remedy undoes", if (is.na(undone)) "—" else paste0(num(100 * undone, 0), "%"), "100 percent restores the market as it was before the merger")
    } else verdict_box("Is the merger better for customers than exit?", if (m >= a) "Yes" else "No", m < a,
      if (m >= a) "Customers lose less with the merger than if the target leaves" else "Customers would be better off if the target left") })
  output$t4_plot <- renderPlot({
    x <- inputs(); k <- base(); nm <- names_now(); r <- merged(); a <- alt()
    altlab <- if (TRUE) "With the divestiture" else "Target leaves the market"
    d <- rbind(data.frame(firm = nm, v = 100 * chg(r$p, k$p), sc = "Merger alone"),
               data.frame(firm = nm, v = 100 * chg(a$p, k$p), sc = altlab))
    ord <- nm[c(x$A, x$B, setdiff(seq_len(x$n), c(x$A, x$B)))]
    d$firm <- factor(d$firm, levels = ord); d$sc <- factor(d$sc, levels = c("Merger alone", altlab))
    d$lab <- ifelse(is.na(d$v), "leaves", paste0(signed(d$v), "%")); d$gone <- is.na(d$v); d$v[is.na(d$v)] <- 0
    top <- max(25, ceiling(max(d$v) / 5) * 5 + 5)
    ggplot(d, aes(firm, v)) +
      geom_hline(yintercept = 100 * sig, linetype = "dashed", color = lab_colors[["ink"]]) +
      geom_col(aes(fill = v > 100 * sig), width = 0.6, color = lab_colors[["ink"]], linewidth = 0.3) +
      geom_text(aes(label = lab), vjust = -0.6, size = 4.6, color = lab_colors[["ink"]]) +
      scale_fill_manual(values = c(`TRUE` = lab_colors[["pass"]], `FALSE` = lab_colors[["fail"]]), guide = "none") +
      scale_y_continuous(limits = c(min(0, min(d$v) - 2), top), expand = expansion(mult = c(0, 0.02))) +
      facet_wrap(~ sc, nrow = 1) +
      labs(x = NULL, y = "Price change (percent)") + lab_gg() + theme(axis.text.x = element_text(size = 12), strip.text = element_text(size = 13, face = "bold"))
  })
}

shinyApp(ui, server)
