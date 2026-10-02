# Antitrust Lab: Market definition
library(shiny)
library(bslib)
library(ggplot2)

# Shiny sources R/ automatically (theme.R, mdef.R, cases.R)

app_version <- "1.4 (2026-10-02)"

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

case_card <- function(story_id, decided_id, source_id) {
  card(
    card_body(uiOutput(story_id)),
    accordion(open = FALSE,
      accordion_panel("What was decided? (open after you have made up your mind)", uiOutput(decided_id))),
    card_footer(uiOutput(source_id))
  )
}

pct_slider <- function(id, label, value, min = 0, max = 60, step = 1) {
  sliderInput(id, label, min = min, max = max, value = value, step = step, post = "%", ticks = FALSE)
}

scope_note <- p(em("This tool covers demand substitution between products. Supply substitution, geographic markets and price-correlation evidence are assessed separately."),
                style = paste0("color:", lab_colors[["muted"]], "; font-size:.9rem;"))

illustrative <- p(em("Illustrative numbers. The case facts are real, but the percentages are not taken from the decisions."),
                  style = paste0("color:", lab_colors[["muted"]], "; font-size:.9rem;"))

# ---- UI ---------------------------------------------------------------------
step_label <- function(k, to) paste0("Step ", k, ". ", toupper(substr(to, 1, 1)), substring(to, 2))

tab_steps <- nav_panel(
  "2 SSNIP step by step",
  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      width = 340,
      selectInput("st_case", "Case", setNames(names(steps_cases), sapply(steps_cases, `[[`, "label"))),
      radioButtons("st_dp", "Price increase (SSNIP)", c("5%" = 0.05, "10%" = 0.10), selected = 0.10, inline = TRUE),
      pct_slider("st_m", "Margin m = (price - cost) / price", 50, min = 5, max = 80, step = 5),
      tags$hr(),
      uiOutput("st_div_head"),
      pct_slider("st_d1", step_label(1, steps_cases$perrier$to[1]), steps_cases$perrier$d[1]),
      pct_slider("st_d2", step_label(2, steps_cases$perrier$to[2]), steps_cases$perrier$d[2]),
      pct_slider("st_d3", step_label(3, steps_cases$perrier$to[3]), steps_cases$perrier$d[3]),
      pct_slider("st_d4", step_label(4, steps_cases$perrier$to[4]), steps_cases$perrier$d[4]),
      uiOutput("st_div_rest"),
      illustrative,
      scope_note,
      actionButton("st_reset", "Reset this case", class = "btn-outline-secondary btn-sm")
    ),
    case_card("st_story", "st_decided", "st_source"),
    card(card_header("What this tab does"),
      card_body(
        p("The test starts with the narrowest group of products. Suppose one firm owned the whole group and raised all its prices. ",
          "The price increase is not profitable if too many customers switch to products outside the group. ",
          "In that case, the closest substitute is added to the group and the test is repeated. The first group that passes the test is the relevant market."),
        p("When all prices in the group rise together, customers have no reason to switch inside the group. The tool therefore predicts the loss in two moves. ",
          "First, suppose the price of one product alone rises. Its price was profit-maximizing, so it loses a share of its sales equal to the price increase divided by the margin. ",
          "The aggregate diversion ratio of the group, also called the recapture rate, is the part of these lost sales that would go to other products inside the group. ",
          "Second, all prices in the group rise. The customers who would have switched inside the group now stay with their product, so only the sales that leave the group are lost."),
        p("The predicted loss is (1 - aggregate diversion ratio) × price increase / margin. ",
          "Every step adds the diversion ratio to the new products, so the aggregate diversion ratio grows and the predicted loss falls from step to step. ",
          "The order of the steps follows the candidate markets discussed in the case, and a later group can attract more sales because it is larger. ",
          "The tool assumes that all products in a group are alike in margin and in diversion."))),
    layout_columns(
      fill = FALSE, col_widths = c(4, 8),
      uiOutput("st_box_cl"), uiOutput("st_box_market")
    ),
    card(card_header("Share of sales lost against the critical loss at each step"),
         plotOutput("st_plot", height = "420px")),
    accordion(open = FALSE, accordion_panel("Show the numbers", tableOutput("st_table")))
  )
)

tab_cl <- nav_panel(
  "1 Is a price increase profitable?",
  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      width = 340,
      selectInput("cl_case", "Case", setNames(names(cl_cases), sapply(cl_cases, `[[`, "label"))),
      radioButtons("cl_dp", "Price increase (SSNIP)", c("5%" = 0.05, "10%" = 0.10), selected = 0.10, inline = TRUE),
      pct_slider("cl_m", "Margin m = (price - cost) / price", 50, min = 5, max = 90),
      radioButtons("cl_method", "How do we predict the loss of sales?", c(
        "From switching inside the group (aggregate diversion ratio)" = "div",
        "From the price sensitivity of the group's demand (own-price elasticity)" = "eps")),
      conditionalPanel("input.cl_method == 'div'",
        pct_slider("cl_A", "Aggregate diversion ratio (share of the sales one product loses, when its price alone rises, that go to other products in the group)", 40, max = 95)),
      conditionalPanel("input.cl_method == 'eps'",
        sliderInput("cl_eps", "Own-price elasticity of the group (absolute value)", 0.1, 8, 1.3, 0.1, ticks = FALSE)),
      illustrative,
      scope_note
    ),
    uiOutput("cl_case_note"),
    layout_columns(
      fill = FALSE, col_widths = c(4, 4, 4),
      uiOutput("cl_box_cl"), uiOutput("cl_box_loss"), uiOutput("cl_box_verdict")
    ),
    card(card_header("Critical loss and predicted loss across margins"),
         plotOutput("cl_plot", height = "400px"),
         card_footer("The shaded area shows losses small enough for the price increase to be profitable. The dots mark your case.")),
    uiOutput("cl_note")
  )
)

tab_cello <- nav_panel(
  "3 Cellophane fallacy",
  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      width = 340,
      p("One firm sells the product and already charges the monopoly price. ",
        "Run the SSNIP test twice, once from a competitive benchmark price and once from the observed price."),
      radioButtons("ce_dp", "Price increase (SSNIP)", c("5%" = 0.05, "10%" = 0.10), selected = 0.10, inline = TRUE),
      sliderInput("ce_c", "Unit cost", 0.5, 4, 2, 0.1, pre = "€", ticks = FALSE),
      sliderInput("ce_pmax", "Price at which nobody buys", 6, 15, 10, 0.5, pre = "€", ticks = FALSE),
      uiOutput("ce_p0_ui"),
      p(em("Stylized numbers."), style = paste0("color:", lab_colors[["muted"]], "; font-size:.9rem;"))
    ),
    card(card_body(cello_case)),
    layout_columns(
      fill = FALSE, col_widths = c(6, 6),
      uiOutput("ce_box_bench"), uiOutput("ce_box_obs")
    ),
    card(card_header("Profit of the cellophane seller at each price"),
         plotOutput("ce_plot", height = "400px")),
    accordion(open = FALSE, accordion_panel("Show the numbers", tableOutput("ce_table")))
  )
)

experiment <- function(id, question, choices, answer) {
  card(
    card_header(question),
    radioButtons(paste0("ex_", id), "Your prediction", choices, selected = character(0), width = "100%"),
    actionButton(paste0("exb_", id), "Reveal", class = "btn-outline-primary btn-sm"),
    conditionalPanel(paste0("input.exb_", id, " > 0"),
      div(style = "margin-top:.6rem;", answer))
  )
}

tab_exp <- nav_panel(
  "Questions",
  p("Predict first, then check with the tool, then reveal."),
  layout_columns(
    col_widths = c(6, 6),
    experiment("1", "In Nestlé/Perrier, should soft drinks be in the same market as bottled water?",
      c("Yes, people drink both when thirsty", "No", "Only sparkling water competes with soft drinks"),
      p(strong("No, according to the Commission. "), "The Commission pointed to large and persistent price differences (soft drinks cost two to three times as much) and to water and soft-drink prices that moved independently of each other (low or negative correlation). It also found that consumers bought water as a natural, healthy product. ",
        "Serving the same need (thirst) does not put two products in the same market. What matters is whether enough buyers would switch after a 5 to 10% price increase.")),
    experiment("2", "Most shoppers who leave Whole Foods go to conventional supermarkets. Does that prove that premium stores are not a market?",
      c("Yes, most switchers leave the group", "No", "Only if margins are high"),
      p(strong("No. "), "In tab 1 (Whole Foods case, illustrative numbers), the critical loss is 10%. With profit-maximizing prices, the price increase breaks even at an aggregate diversion ratio of 10% and is profitable above that. ",
        "Most can leave. The district court's reasoning would hold only with low margins, as the low-margin grocer shows.")),
    experiment("3", "In tab 1, use the aggregate diversion ratio with a 10% price increase and set it to 20%. Then raise the margin from 20% to 60%. Does the market get narrower or wider?",
      c("Narrower", "Wider", "No change"),
      p(strong("Narrower. "), "A higher margin lowers the critical loss, but it lowers the predicted loss by more, because a high margin signals customers who react little to price. ",
        "At a 20% margin, the critical loss is 33% and the predicted loss is 40%, so the group does not pass the test. At a 60% margin, the critical loss is 14% and the predicted loss is 13%, so the group passes. This is the argument made in Whole Foods, where margins were said to be high.")),
    experiment("4", "In tab 1, use the elasticity method, set the elasticity to 2, and raise the margin. What happens?",
      c("The market gets narrower", "The market gets wider", "No change"),
      p(strong("Wider, but watch out. "), "With the group's elasticity held fixed, a higher margin raises the cost of each lost sale, so the price increase is unprofitable more often. ",
        "But if firms price optimally, the group's elasticity cannot exceed 1/m. With an elasticity of 2, margins above 50% are inconsistent, and margins below 50% imply an aggregate diversion ratio of 1 - 2m. The tool checks this for you. With a 10% price increase the loss is 20%, and the test flips from pass to fail at a margin of 40%.")),
    experiment("5", "In tab 3, can a monopolist pass the SSNIP test at its own profit-maximizing price?",
      c("Yes, if demand is inelastic", "No, never", "Depends on the cost"),
      p(strong("No, never. "), "At the monopoly price any further increase lowers profit, otherwise the firm would already charge more. ",
        "The test then always says that substitutes must be added, and the market comes out too wide. That is the criticism of the du Pont judgment."))
  )
)

tab_model <- nav_panel(
  "Math",
  withMathJax(),
  card(card_header("Critical loss"),
    p("A hypothetical monopolist raises price by a share \\(\\Delta p\\). With margin \\(m = (p - c)/p\\), the price increase is profitable as long as it loses less than the critical loss"),
    p("$$\\text{CL}^* = \\frac{\\Delta p}{m + \\Delta p}$$"),
    p("The proportional change in profit is \\(\\dfrac{(m+\\Delta p)(1-L) - m}{m}\\), where \\(L\\) is the share of sales lost.")),
  card(card_header("Predicting the loss"),
    p("With an elasticity \\(\\eta\\) of the group, the predicted loss is \\(L = |\\eta| \\, \\Delta p\\)."),
    p("The diversion-ratio route starts from the margin. If each product's price is already profit-maximizing, each product's own elasticity is \\(|\\eta_i| = 1/m\\). Let \\(A\\) be the aggregate diversion ratio of the group, which is the share of the sales lost by one product, when its price alone rises, that go to other products in the group. It is also called the recapture rate. When all prices in the group rise together, sales no longer move between the products in the group, and each product loses only the share \\(1 - A\\) of what it would lose if its price rose alone. Then the group's elasticity is \\(|\\eta| = (1 - A)/m\\), so that \\(L = \\Delta p (1 - A)/m\\), and the test is passed exactly when \\(A > \\text{CL}^*\\). At exactly break-even the test is not passed."),
    p("In tab 2, \\(D_{0k}\\) is the diversion ratio from the starting product to the products added at step \\(k\\). The aggregate diversion ratio of the group at step \\(k\\) is \\(A_k = D_{01} + \\dots + D_{0k}\\), so it cannot fall when the group grows. The tool assumes that every product in the group loses the same share \\(A_k\\) of its departing customers to the rest of the group."),
    p("The diversion formula assumes symmetric products, single-product firms that price optimally before the price increase, demand that is linear over the range of the price increase, and the same price increase on every product in the group. With multi-brand owners (as in Nestlé/Perrier), each product is more elastic than \\(1/m\\) suggests, so the formula understates the loss."),
    p("The elasticity formula is exact for linear demand and overstates the loss for convex demand (for example, constant elasticity), and the gap grows with \\(|\\eta| \\, \\Delta p\\)."),
    p("With linear demand, the price increase is profitable if \\(|\\eta| < 1/(m + \\Delta p)\\), the critical elasticity."),
    p("This is the break-even test, which asks whether the hypothetical monopolist could raise price without losing profit. The stricter profit-maximizing test asks whether the hypothetical monopolist would raise price by at least \\(\\Delta p\\). With linear demand, it requires \\(A \\ge 2\\Delta p/(m + 2\\Delta p)\\) (Farrell and Shapiro 2008).")),
  card(card_header("Cellophane tab"),
    p("Linear demand \\(Q = 100\\,(1 - p/\\bar p)\\), constant unit cost \\(c\\). Monopoly price \\(p^{\\text{m}} = (\\bar p + c)/2\\), where \\(\\bar p\\) is the price at which nobody buys. The elasticity at price \\(p\\) is \\(|\\eta| = p/(\\bar p - p)\\). At \\(p^{\\text{m}}\\), \\(m = 1/|\\eta|\\), so the predicted loss \\(\\Delta p/m\\) always exceeds \\(\\text{CL}^*\\). From a starting price \\(p_0\\) the test is passed if \\(p_0 < 2p^{\\text{m}}/(2 + \\Delta p)\\). Prices close to the monopoly price therefore fail the test as well. The chart shows the profit \\(\\pi(p) = (p - c)\\,Q(p)\\), which peaks at \\(p^{\\text{m}}\\).")),
  card(card_header("Sources"),
    tags$ul(
      lab_ref("European Commission. 1992. \"Commission Decision of 22 July 1992 Relating to a Proceeding under Council Regulation (EEC) No 4064/89 (Case No IV/M.190, Nestl\u00e9/Perrier).\" Decision 92/553/EEC. ", em("Official Journal of the European Communities"), " L 356, December 5: 1\u201331.", url = "https://eur-lex.europa.eu/eli/dec/1992/553/oj/eng"),
      lab_ref("European Commission. 2024. \"Commission Notice on the Definition of the Relevant Market for the Purposes of Union Competition Law.\" C/2024/1645. ", em("Official Journal of the European Union"), " C, February 22.", url = "https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=OJ:C_202401645"),
      lab_ref("Farrell, Joseph, and Carl Shapiro. 2008. \"Improving Critical Loss Analysis.\" ", em("The Antitrust Source"), ", February: 1\u201317.", url = "https://faculty.haas.berkeley.edu/shapiro/critical2008.pdf"),
      lab_ref("Federal Trade Commission. 2009. \"FTC Consent Order Settles Charges That Whole Foods Acquisition of Rival Wild Oats Was Anticompetitive.\" Press release, March 6.", url = "https://www.ftc.gov/news-events/news/press-releases/2009/03/ftc-consent-order-settles-charges-whole-foods-acquisition-rival-wild-oats-was-anticompetitive"),
      lab_ref(em("Federal Trade Commission v. Whole Foods Market, Inc."), ", 548 F.3d 1028 (D.C. Cir. 2008).", url = "https://www.courtlistener.com/opinion/1287444/federal-trade-commission-v-whole-foods-market-inc/"),
      lab_ref("Harris, Barry C., and Joseph J. Simons. 1989. \"Focusing Market Definition: How Much Substitution Is Necessary?\" ", em("Research in Law and Economics"), " 12: 207\u201326."),
      lab_ref("Katz, Michael L., and Carl Shapiro. 2003. \"Critical Loss: Let's Tell the Whole Story.\" ", em("Antitrust"), " 17 (2): 49\u201356."),
      lab_ref(em("United States v. E. I. du Pont de Nemours & Co."), ", 351 U.S. 377 (1956).", url = "https://supreme.justia.com/cases/federal/us/351/377/")))
)

ui <- page_navbar(
  fillable = FALSE,
  title = lab_title("Market definition"),
  window_title = "Antitrust Lab",
  theme = lab_theme(),
  navbar_options = lab_navbar(),
  header = div(class = "container-fluid",
    lab_header("Is this product a market of its own?", "Topic: market definition", tool = "Market definition")),
  footer = div(class = "container-fluid",
    style = paste0("color:", lab_colors[["muted"]], "; font-size:.85rem; padding:.5rem 0;"),
    paste("Version", app_version, "· Case facts are real, and numbers are illustrative unless stated otherwise.")),
  tab_cl, tab_steps, tab_cello, tab_exp, tab_model
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {

  # Tab 2 ---------------------------------------------------------------------
  st_case <- reactive(steps_cases[[input$st_case]])

  load_steps_case <- function(cs) {
    updateRadioButtons(session, "st_dp", selected = cs$dp)
    updateSliderInput(session, "st_m", value = cs$m)
    for (k in 1:4) updateSliderInput(session, paste0("st_d", k), label = step_label(k, cs$to[k]), value = cs$d[k])
  }
  observeEvent(input$st_case, load_steps_case(st_case()), ignoreInit = TRUE)
  observeEvent(input$st_reset, load_steps_case(st_case()))

  output$st_story   <- renderUI(st_case()$story)
  output$st_decided <- renderUI(if (is.null(st_case()$decided)) p("No decision.") else st_case()$decided)
  output$st_source  <- renderUI(if (is.null(st_case()$source)) NULL else tagList("Source: ", st_case()$source))

  output$st_div_head <- renderUI(p(strong(paste0("Suppose the price of ", st_case()$focal,
    " alone rises. Of the sales it loses, what share goes to each of these products? Each share is a diversion ratio."))))

  st_d <- reactive(c(input$st_d1, input$st_d2, input$st_d3, input$st_d4) / 100)

  output$st_div_rest <- renderUI({
    s <- sum(st_d())
    if (s > 1 + 1e-9) p(strong("The four diversion ratios add up to more than 100%. The tool caps the aggregate diversion ratio at 100%."),
                        style = paste0("color:", lab_colors[["fail_fg"]], "; font-size:.9rem;"))
    else p(em(paste0("The other ", pct(1 - s, 0), " of the lost sales go to products outside the four groups or disappear.")),
           style = paste0("color:", lab_colors[["muted"]], "; font-size:.9rem;"))
  })

  steps <- reactive({
    dp <- as.numeric(input$st_dp)
    cl <- cl_star(dp, input$st_m / 100)
    d  <- st_d()
    A  <- pmin(cumsum(d), 1)   # aggregate diversion ratio of each candidate market
    L  <- loss_from_diversion(dp, input$st_m / 100, A)
    ok <- L < cl - 1e-9
    first <- if (any(ok)) which(ok)[1] else NA
    data.frame(step = 1:4, set = st_case()$market_name, d = d, A = A, loss = L, cl = cl, ok = ok,
               market = !is.na(first) & seq_along(L) == first,
               shown = if (is.na(first)) rep(TRUE, 4) else seq_along(L) <= first)
  })

  output$st_box_cl <- renderUI(number_box("Critical loss", pct(steps()$cl[1]),
    "Largest loss of sales the monopolist can afford"))
  output$st_box_market <- renderUI({
    d <- steps()
    if (any(d$market)) {
      k <- which(d$market)
      verdict_box("Relevant market", d$set[k],
                  TRUE, paste0("Found at step ", k, ", the first group for which the price increase is profitable"))
    } else verdict_box("Relevant market", "Wider than these four groups", FALSE,
                       "Even the largest group fails the test")
  })

  output$st_plot <- renderPlot({
    d <- steps()
    d$x <- paste0("Step ", d$step, "\n", stringr_wrap(d$set, 22), "\n(aggregate diversion\nratio ", pct(d$A, 0), ")")
    d$x <- factor(d$x, levels = d$x)
    d$status <- ifelse(d$ok, "yes", "no")
    d$alpha <- ifelse(d$shown, 1, 0.18)
    d$lab <- ifelse(d$shown, paste0("loses ", pct(d$loss), "\n", ssnip_word(d$loss, d$cl)), paste0("loses ", pct(d$loss)))
    ggplot(d, aes(x, loss)) +
      geom_col(aes(fill = status, alpha = alpha), width = 0.6) +
      geom_hline(yintercept = d$cl[1], color = lab_colors[["critical"]], linewidth = 1.2, linetype = "dashed") +
      annotate("text", x = 4.45, y = d$cl[1], label = paste("Critical loss", pct(d$cl[1])),
               hjust = 1, vjust = -0.6, color = lab_colors[["critical"]], size = 5) +
      geom_label(aes(label = lab), vjust = -0.15, size = 4.8, lineheight = 0.95, color = lab_colors[["ink"]],
                 fill = lab_colors[["bg"]], label.size = 0, label.padding = unit(0.15, "lines")) +
      scale_fill_manual(values = c("yes" = lab_colors[["pass"]], "no" = lab_colors[["fail"]]), name = NULL) +
      scale_alpha_identity() +
      scale_y_continuous(labels = function(x) pct(x, 0), limits = c(0, max(0.35, max(d$loss), d$cl[1]) + 0.08),
                         expand = expansion(mult = c(0, 0.02))) +
      labs(x = NULL, y = "Share of sales lost") +
      lab_gg() + theme(legend.position = "none")
  })

  output$st_table <- renderTable({
    d <- steps()
    data.frame(Step = d$step, `Candidate market` = d$set,
               `Diversion ratio to the added products` = pct(d$d, 0),
               `Aggregate diversion ratio of the group` = pct(d$A, 0),
               `Sales lost` = pct(d$loss), `Critical loss` = pct(d$cl),
               `Price increase` = ssnip_word(d$loss, d$cl),
               Conclusion = ifelse(d$market, "This is the relevant market",
                                   ifelse(d$ok, "", ifelse(d$shown, "Add the next product", ""))),
               check.names = FALSE)
  }, striped = TRUE, hover = TRUE, width = "100%")

  # Tab 1 ---------------------------------------------------------------------
  observeEvent(input$cl_case, {
    cs <- cl_cases[[input$cl_case]]
    updateRadioButtons(session, "cl_dp", selected = cs$dp)
    updateSliderInput(session, "cl_m", value = cs$m)
    updateRadioButtons(session, "cl_method", selected = cs$method)
    updateSliderInput(session, "cl_A", value = cs$A)
    updateSliderInput(session, "cl_eps", value = cs$eps)
  })

  output$cl_case_note <- renderUI({
    cs <- cl_cases[[input$cl_case]]
    if (is.null(cs$note)) NULL else card(card_body(p(cs$note)))
  })

  cl_res <- reactive({
    dp <- as.numeric(input$cl_dp); m <- input$cl_m / 100; A <- input$cl_A / 100
    cl <- cl_star(dp, m)
    L <- if (input$cl_method == "div") loss_from_diversion(dp, m, A)
         else loss_from_elasticity(input$cl_eps, dp)
    list(dp = dp, m = m, A = A, cl = cl, L = L, ok = L < cl - 1e-9, pc = profit_change(dp, m, L))
  })

  output$cl_box_cl <- renderUI(number_box("Critical loss", pct(cl_res()$cl), "Break-even share of sales lost"))
  output$cl_box_loss <- renderUI(number_box("Predicted loss", pct(cl_res()$L), if (cl_res()$L >= 1 - 1e-9) "All sales are lost. With a margin this low, customers are so price-sensitive that the price increase empties the market" else "Share of sales the group would lose",
                                            lab_colors[["actual"]]))
  output$cl_box_verdict <- renderUI({
    r <- cl_res()
    w <- ssnip_word(r$L, r$cl)
    verdict_box("SSNIP test", paste("Price increase", w), r$ok,
      paste0("Profit changes by ", pct(r$pc), ". ",
             if (r$ok) "The relevant market is this group or a narrower one." else if (w == "breaks even") "Exactly at break-even, the test is not passed. Widen the candidate market." else "Widen the candidate market."))
  })

  output$cl_plot <- renderPlot({
    r <- cl_res()
    mm <- seq(0.05, 0.9, by = 0.005)
    curves <- data.frame(m = mm, value = cl_star(r$dp, mm), what = "Critical loss")
    if (input$cl_method == "div") {
      curves <- rbind(curves, data.frame(m = mm, value = pmin(loss_from_diversion(r$dp, mm, r$A), 1.5),
                                         what = "Predicted loss"))
    } else {
      curves <- rbind(curves, data.frame(m = mm, value = loss_from_elasticity(input$cl_eps, r$dp),
                                         what = "Predicted loss"))
    }
    ymax <- min(1, max(0.35, 1.25 * max(r$cl, r$L)))
    ggplot(curves, aes(m, value, color = what)) +
      geom_ribbon(data = data.frame(m = mm, lo = 0, hi = cl_star(r$dp, mm)),
                  aes(m, ymin = lo, ymax = hi), inherit.aes = FALSE, fill = lab_colors[["pass"]], alpha = 0.10) +
      geom_line(linewidth = 1.3) +
      geom_vline(xintercept = r$m, color = lab_colors[["muted"]], linetype = "dotted") +
      geom_point(data = data.frame(m = r$m, value = c(r$cl, r$L), what = c("Critical loss", "Predicted loss")),
                 size = 4) +
      geom_text(data = data.frame(m = r$m, value = c(r$cl, r$L), what = c("Critical loss", "Predicted loss")),
                aes(label = pct(value)), hjust = -0.25, vjust = if (r$cl >= r$L) c(-0.6, 1.6) else c(1.6, -0.6),
                size = 4.5, show.legend = FALSE) +
      geom_text(data = data.frame(m = 0.905, value = c(cl_star(r$dp, 0.9),
                  if (input$cl_method == "div") loss_from_diversion(r$dp, 0.9, r$A) else loss_from_elasticity(input$cl_eps, r$dp)),
                  what = c("Critical loss", "Predicted loss")),
                aes(label = tolower(what)), hjust = 0, size = 4.5, show.legend = FALSE) +
      scale_color_manual(values = c("Critical loss" = lab_colors[["critical"]], "Predicted loss" = lab_colors[["actual"]]),
                         name = NULL) +
      scale_x_continuous(labels = function(x) pct(x, 0), breaks = c(0.25, 0.5, 0.75)) +
      scale_y_continuous(labels = function(x) pct(x, 0)) +
      coord_cartesian(ylim = c(0, ymax), xlim = c(0.05, 1.08)) +
      labs(x = "Margin m", y = "Share of sales") +
      lab_gg() + theme(legend.position = "none")
  })

  output$cl_note <- renderUI({
    r <- cl_res()
    if (input$cl_method != "eps") {
      return(card(card_body(p(paste0("With profit-maximizing prices, the test is passed exactly when the aggregate diversion ratio of the group ",
        "(", pct(r$A, 0), ") exceeds the critical loss (", pct(r$cl), "). ",
        "High margins lower the critical loss, so the group can be a market even when most switchers leave it.")))))
    }
    me <- r$m * input$cl_eps
    if (me > 1 + 1e-9) {
      card(class = "border-warning", card_body(
        p(strong("These numbers are inconsistent. "), paste0("If each firm prices optimally, the group's demand cannot be more elastic than 1/m = ",
          formatC(1 / r$m, format = "f", digits = 2), ". Either the margin or the elasticity estimate is too high. ",
          "Using both overstates the loss and makes the market look too wide (Katz and Shapiro 2003)."))))
    } else card(card_body(p(paste0("These numbers are consistent. With optimal pricing, this elasticity implies an aggregate diversion ratio of ", pct(1 - me, 0), "."))))
  })

  # Tab 3 ---------------------------------------------------------------------
  output$ce_p0_ui <- renderUI({
    c0 <- input$ce_c; pm <- lin_pm(input$ce_pmax, c0)
    dp <- as.numeric(input$ce_dp)
    hi <- floor(10 * pm - 0.5) / 10   # up to just below the monopoly price, so that the verdict can flip
    sliderInput("ce_p0", "Competitive benchmark price",
                min = round(c0 + 0.1, 1), max = max(round(c0 + 0.2, 1), hi), pre = "€", ticks = FALSE,
                value = round(c0 + 0.1 * (pm - c0), 1), step = 0.1)
  })

  ce <- reactive({
    req(input$ce_p0)
    dp <- as.numeric(input$ce_dp); c0 <- input$ce_c; pmax <- input$ce_pmax
    pm <- lin_pm(pmax, c0)
    p0 <- min(max(input$ce_p0, c0 + 0.05), pm - 0.05)
    rbind(cellophane_row("Competitive benchmark price", p0, c0, pmax, dp),
          cellophane_row("Observed (monopoly) price", pm, c0, pmax, dp))
  })

  cello_box <- function(r, title) {
    verdict_box(title, if (r$verdict == "profitable") "Cellophane is a market" else "Widen the market",
      r$verdict == "profitable", paste0("Loses ", pct(r$predicted_loss), " against a critical loss of ", pct(r$critical_loss)))
  }
  output$ce_box_bench <- renderUI(cello_box(ce()[1, ], "Starting from the competitive price"))
  output$ce_box_obs   <- renderUI(cello_box(ce()[2, ], "Starting from the observed price"))

  output$ce_plot <- renderPlot({
    d <- ce(); pmax <- input$ce_pmax; c0 <- input$ce_c; dp <- as.numeric(input$ce_dp)
    pm <- lin_pm(pmax, c0); thr <- 2 * pm / (2 + dp)
    prof <- function(p) (p - c0) * lin_q(p, pmax)
    hill <- data.frame(p = seq(c0, pmax, length.out = 200))
    hill$profit <- prof(hill$p)
    top <- prof(pm)
    mv <- data.frame(p0 = d$price, p1 = d$price * (1 + dp))
    mv$y0 <- prof(mv$p0); mv$y1 <- prof(mv$p1)
    mv$up <- mv$y1 > mv$y0
    mv$lab <- paste0(c("Competitive benchmark price", "Observed (monopoly) price"), " €", sprintf("%.2f", mv$p0),
                     "\nprofit ", ifelse(abs(mv$y1 / mv$y0 - 1) < 1e-9, "does not change", paste0(ifelse(mv$up, "rises", "falls"), " by ", pct(abs(mv$y1 / mv$y0 - 1)))))
    ggplot(hill, aes(p, profit)) +
      annotate("rect", xmin = c0, xmax = thr, ymin = 0, ymax = Inf, fill = lab_colors[["pass"]], alpha = 0.10) +
      geom_line(linewidth = 1.2, color = lab_colors[["ink"]]) +
      geom_vline(xintercept = thr, color = lab_colors[["critical"]], linewidth = 1.1, linetype = "dashed") +
      annotate("text", x = thr, y = 1.3 * top, hjust = 1.03, vjust = 1, size = 4.6, lineheight = 0.95,
               color = lab_colors[["critical"]],
               label = paste0("Starting prices in the shaded area\npass the test (below €", sprintf("%.2f", thr), ")")) +
      geom_segment(data = mv, aes(x = p0, xend = p1, y = y0, yend = y1), inherit.aes = FALSE,
                   color = ifelse(mv$up, lab_colors[["pass"]], lab_colors[["actual"]]), linewidth = 1.6,
                   arrow = arrow(length = unit(0.3, "cm"))) +
      geom_point(data = mv, aes(p0, y0), inherit.aes = FALSE, size = 4.5, color = lab_colors[["ink"]]) +
      geom_text(data = mv, aes(p0, y0, label = lab), inherit.aes = FALSE, hjust = c(-0.06, -0.04), vjust = c(1.3, -0.5),
                size = 4.6, lineheight = 0.95, color = lab_colors[["ink"]]) +
      scale_x_continuous(labels = function(x) paste0("€", x)) +
      coord_cartesian(ylim = c(0, 1.3 * top)) +
      labs(x = "Price of cellophane", y = "Profit",
           caption = paste0("Each arrow shows a ", pct(dp, 0), " price increase. It is profitable if it moves the firm up the profit hill. At the top of the hill, every price increase lowers profit.")) +
      lab_gg()
  })

  output$ce_table <- renderTable({
    d <- ce()
    data.frame(`Starting price` = d$start, `Price (€)` = sprintf("%.2f", d$price),
               Margin = pct(d$margin), `Elasticity (absolute value)` = sprintf("%.2f", d$elasticity),
               `Critical loss` = pct(d$critical_loss), `Predicted loss` = pct(d$predicted_loss),
               `Price increase` = sub("^is ", "", ssnip_word(d$predicted_loss, d$critical_loss)), check.names = FALSE)
  }, striped = TRUE, width = "100%")
}

shinyApp(ui, server)
