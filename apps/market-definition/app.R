# Antitrust Lab: Market definition
library(shiny)
library(bslib)
library(ggplot2)

# Shiny sources R/ automatically (theme.R, mdef.R, cases.R)

app_version <- "0.6 (2026-10-02)"

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

illustrative <- p(em("Illustrative numbers. The case facts are real; the percentages are not taken from the decisions."),
                  style = paste0("color:", lab_colors[["muted"]], "; font-size:.9rem;"))

# ---- UI ---------------------------------------------------------------------
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
      p(strong("Share recaptured: of the customers who leave a product after the price increase, the share who switch to another product in the group")),
      pct_slider("st_l1", steps_cases$perrier$steps[1], steps_cases$perrier$A[1], max = 95),
      pct_slider("st_l2", steps_cases$perrier$steps[2], steps_cases$perrier$A[2], max = 95),
      pct_slider("st_l3", steps_cases$perrier$steps[3], steps_cases$perrier$A[3], max = 95),
      pct_slider("st_l4", steps_cases$perrier$steps[4], steps_cases$perrier$A[4], max = 95),
      p(em("The share of sales lost follows from the margin, the price increase and these shares (see Model)."),
        style = paste0("color:", lab_colors[["muted"]], "; font-size:.9rem;")),
      illustrative,
      scope_note,
      actionButton("st_reset", "Reset this case", class = "btn-outline-secondary btn-sm")
    ),
    case_card("st_story", "st_decided", "st_source"),
    layout_columns(
      fill = FALSE, col_widths = c(4, 8),
      uiOutput("st_box_cl"), uiOutput("st_box_market")
    ),
    card(card_header("Each step: share of sales lost against the critical loss"),
         plotOutput("st_plot", height = "380px")),
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
        "From the share recaptured by the group (switching)" = "div",
        "From an estimated elasticity of the group" = "eps")),
      conditionalPanel("input.cl_method == 'div'",
        pct_slider("cl_A", "Share recaptured: lost customers who switch to other products in the group", 40, max = 95)),
      conditionalPanel("input.cl_method == 'eps'",
        sliderInput("cl_eps", "Own-price elasticity of the group (absolute value)", 0.1, 6, 2, 0.1, ticks = FALSE)),
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
         card_footer("Shaded area: losses small enough for the price increase to be profitable. Dots: your case.")),
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
        "Run the SSNIP test twice: from a competitive benchmark price and from the observed price."),
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
    card(card_header("From the monopoly price, any further price increase loses money"),
         plotOutput("ce_plot", height = "400px")),
    accordion(open = FALSE, accordion_panel("Show the numbers", tableOutput("ce_table")))
  )
)

experiment <- function(id, question, choices, answer) {
  card(
    card_header(question),
    radioButtons(paste0("ex_", id), "Your prediction", choices, selected = character(0)),
    actionButton(paste0("exb_", id), "Reveal", class = "btn-outline-primary btn-sm"),
    conditionalPanel(paste0("input.exb_", id, " > 0"),
      div(style = "margin-top:.6rem;", answer))
  )
}

tab_exp <- nav_panel(
  "Experiments",
  p("Predict first, then check with the tool, then reveal."),
  layout_columns(
    col_widths = c(6, 6),
    experiment("1", "Nestlé/Perrier: should soft drinks be in the same market as bottled water?",
      c("Yes, people drink both when thirsty", "No", "Only sparkling water competes with soft drinks"),
      p(strong("No, according to the Commission. "), "Its evidence: large and persistent price differences (soft drinks cost two to three times as much), water and soft drink prices that moved independently (low or negative correlation), and consumers buying water as a natural, healthy product. ",
        "Serving the same need (thirst) does not put two products in the same market; what matters is whether enough buyers would switch after a 5 to 10% price increase.")),
    experiment("2", "Whole Foods: most shoppers who leave Whole Foods go to conventional supermarkets. Does that prove premium stores are not a market?",
      c("Yes, most switchers leave the group", "No", "Only if margins are high"),
      p(strong("No. "), "In tab 1 (Whole Foods case, illustrative numbers), the critical loss is 10%. With profit-maximizing prices, the price increase breaks even once 10% of switchers stay inside the group and is profitable above that. ",
        "Most can leave. The district court's reasoning would hold only with low margins: try the low-margin grocer.")),
    experiment("3", "Tab 1, 'Your own numbers', recapture method, 10% price increase, 20% recaptured: raise the margin from 20% to 60%. Does the market get narrower or wider?",
      c("Narrower", "Wider", "No change"),
      p(strong("Narrower. "), "A higher margin lowers the critical loss, so fewer customers need to stay inside the group for the price increase to be profitable. ",
        "At a 20% margin the critical loss is 33% and the group does not pass the test; at 60% it is 14% and the group passes. This is the argument made in Whole Foods, where margins were said to be high.")),
    experiment("4", "Tab 1, elasticity method: keep the elasticity at 2 and raise the margin. What happens?",
      c("The market gets narrower", "The market gets wider", "No change"),
      p(strong("Wider, but watch out. "), "With the group's elasticity held fixed, a higher margin raises the cost of each lost sale, so the price increase is unprofitable more often. ",
        "But if firms price optimally, the group's elasticity cannot exceed 1/m. With an elasticity of 2, margins above 50% are inconsistent; below 50% they imply that a share 1 - 2m of switchers stays in the group. The tool checks this for you. With a 10% price increase the loss is 20%, and the test flips from pass to fail at a margin of 40%.")),
    experiment("5", "Tab 3: can a monopolist pass the SSNIP test at its own profit-maximizing price?",
      c("Yes, if demand is inelastic", "No, never", "Depends on the cost"),
      p(strong("No, never. "), "At the monopoly price any further increase lowers profit, otherwise the firm would already charge more. ",
        "The test then always says 'add substitutes', and the market comes out too wide. That is the criticism of the du Pont judgment."))
  )
)

tab_model <- nav_panel(
  "Model",
  withMathJax(),
  card(card_header("Critical loss"),
    p("A hypothetical monopolist raises price by a share \\(\\Delta p\\). With margin \\(m = (p - c)/p\\), the price increase is profitable as long as it loses less than the critical loss:"),
    p("$$\\text{CL}^* = \\frac{\\Delta p}{m + \\Delta p}$$"),
    p("Profit change: \\(\\dfrac{(m+\\Delta p)(1-L) - m}{m}\\), where \\(L\\) is the share of sales lost.")),
  card(card_header("Predicting the loss"),
    p("From an elasticity \\(\\eta\\) of the group: \\(L = |\\eta| \\, \\Delta p\\)."),
    p("From recapture (diversion): if each product's price is already profit-maximizing, \\(|\\eta| = 1/m\\) for each product. If a share \\(A\\) of lost customers stays inside the group, then \\(L = \\Delta p (1 - A)/m\\), and the test is passed exactly when \\(A > \\text{CL}^*\\). At exactly break-even the test is not passed."),
    p("Assumptions for the diversion formula: symmetric products, single-product firms that price optimally before the price increase, demand linear for small price changes, and the same price increase on every product in the group. With multi-brand owners (as in Nestlé/Perrier), each product is more elastic than \\(1/m\\) suggests, so the formula understates the loss."),
    p("The elasticity formula is exact for linear demand and overstates the loss for convex demand (for example, constant elasticity); the gap grows with \\(|\\eta| \\, \\Delta p\\)."),
    p("Critical elasticity: the price increase is profitable if the group's elasticity is below \\(1/(m + \\Delta p)\\)."),
    p("This is the break-even test: could the hypothetical monopolist raise price without losing profit? The stricter profit-maximizing test asks whether the hypothetical monopolist would raise price by at least \\(\\Delta p\\); with linear demand it requires \\(A > 2\\Delta p/(m + 2\\Delta p)\\) (Farrell and Shapiro 2008).")),
  card(card_header("Cellophane tab"),
    p("Linear demand \\(Q = 100\\,(1 - p/\\bar p)\\), constant unit cost \\(c\\). Monopoly price \\(p^{\\text{m}} = (\\bar p + c)/2\\), where \\(\\bar p\\) is the price at which nobody buys. Elasticity at price \\(p\\): \\(|\\eta| = p/(\\bar p - p)\\). At \\(p^{\\text{m}}\\), \\(m = 1/|\\eta|\\), so the predicted loss \\(\\Delta p/m\\) always exceeds \\(\\text{CL}^*\\). From a starting price \\(p_0\\) the test is passed if \\(p_0 < 2p^{\\text{m}}/(2 + \\Delta p)\\): prices close to the monopoly price fail as well.")),
  card(card_header("Sources"),
    tags$ul(
      tags$li(src_link("Commission Decision 92/553/EEC (Nestlé/Perrier), 1992", "https://eur-lex.europa.eu/eli/dec/1992/553/oj/eng")),
      tags$li(src_link("FTC v. Whole Foods Market, 548 F.3d 1028 (D.C. Cir. 2008)", "https://www.courtlistener.com/opinion/1287444/federal-trade-commission-v-whole-foods-market-inc/")),
      tags$li(src_link("United States v. E. I. du Pont de Nemours & Co., 351 U.S. 377 (1956)", "https://supreme.justia.com/cases/federal/us/351/377/")),
      tags$li("Harris, B. C., and J. J. Simons (1989). Focusing market definition: How much substitution is necessary? Research in Law and Economics 12: 207-226."),
      tags$li("Katz, M. L., and C. Shapiro (2003). Critical loss: Let's tell the whole story. Antitrust 17(2)."),
      tags$li(src_link("Farrell, J., and C. Shapiro (2008). Improving critical loss analysis. The Antitrust Source.", "http://faculty.haas.berkeley.edu/Shapiro/critical2008.pdf")),
      tags$li(src_link("European Commission (2024). Notice on the definition of the relevant market, para. 30.", "https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=OJ:C_202401645"))))
)

ui <- page_navbar(
  fillable = FALSE,
  title = "Antitrust Lab · Market definition",
  theme = lab_theme(),
  navbar_options = lab_navbar(),
  header = div(class = "container-fluid",
    lab_header("Is this product a market of its own?", "Topic: market definition", tool = "Market definition")),
  footer = div(class = "container-fluid",
    style = paste0("color:", lab_colors[["muted"]], "; font-size:.85rem; padding:.5rem 0;"),
    paste("Version", app_version, "· Case facts are real; numbers are illustrative unless stated otherwise.")),
  tab_cl, tab_steps, tab_cello, tab_exp, tab_model
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {

  # Tab 1 ---------------------------------------------------------------------
  st_case <- reactive(steps_cases[[input$st_case]])

  load_steps_case <- function(cs) {
    updateRadioButtons(session, "st_dp", selected = cs$dp)
    updateSliderInput(session, "st_m", value = cs$m)
    for (k in 1:4) updateSliderInput(session, paste0("st_l", k), label = cs$steps[k], value = cs$A[k])
  }
  observeEvent(input$st_case, load_steps_case(st_case()), ignoreInit = TRUE)
  observeEvent(input$st_reset, load_steps_case(st_case()))

  output$st_story   <- renderUI(st_case()$story)
  output$st_decided <- renderUI(if (is.null(st_case()$decided)) p("No decision: your own numbers.") else st_case()$decided)
  output$st_source  <- renderUI(if (is.null(st_case()$source)) NULL else tagList("Source: ", st_case()$source))

  steps <- reactive({
    dp <- as.numeric(input$st_dp)
    cl <- cl_star(dp, input$st_m / 100)
    A  <- c(input$st_l1, input$st_l2, input$st_l3, input$st_l4) / 100
    L  <- loss_from_diversion(dp, input$st_m / 100, A)
    ok <- L < cl - 1e-9
    first <- if (any(ok)) which(ok)[1] else NA
    data.frame(step = 1:4, set = st_case()$steps, A = A, loss = L, cl = cl, ok = ok,
               market = !is.na(first) & seq_along(L) == first,
               shown = if (is.na(first)) rep(TRUE, 4) else seq_along(L) <= first)
  })

  output$st_box_cl <- renderUI(number_box("Critical loss", pct(steps()$cl[1]),
    "Largest loss of sales the monopolist can afford"))
  output$st_box_market <- renderUI({
    d <- steps()
    if (any(d$market)) {
      k <- which(d$market)
      verdict_box("Relevant market", st_case()$market_name[k],
                  TRUE, paste0("Found at step ", k, ": the first group where the price increase is profitable"))
    } else verdict_box("Relevant market", "Wider than these four groups", FALSE,
                       "Even the largest group fails the test")
  })

  output$st_plot <- renderPlot({
    d <- steps()
    d$set <- factor(d$set, levels = d$set)
    d$status <- ifelse(d$ok, "Price increase profitable: market found", "Price increase not profitable: widen the group")
    d$alpha <- ifelse(d$shown, 1, 0.18)
    d$lab <- ifelse(d$shown, paste0(pct(d$loss, 0), " \u00b7 ", ssnip_word(d$loss, d$cl)), pct(d$loss, 0))
    ggplot(d, aes(set, loss)) +
      geom_col(aes(fill = status, alpha = alpha), width = 0.6) +
      geom_hline(yintercept = d$cl[1], color = lab_colors[["critical"]], linewidth = 1.2, linetype = "dashed") +
      annotate("text", x = 4.45, y = d$cl[1], label = paste("Critical loss", pct(d$cl[1])),
               hjust = 1, vjust = -0.6, color = lab_colors[["critical"]], size = 5) +
      geom_text(aes(label = lab), vjust = -0.4, size = 5, color = lab_colors[["ink"]]) +
      scale_fill_manual(values = c("Price increase profitable: market found" = lab_colors[["pass"]],
                                   "Price increase not profitable: widen the group" = lab_colors[["fail"]]), name = NULL) +
      scale_alpha_identity() +
      scale_x_discrete(labels = function(x) gsub("^\\+ ", "+ ", stringr_wrap(x))) +
      scale_y_continuous(labels = function(x) pct(x, 0), limits = c(0, max(0.35, max(d$loss), d$cl[1]) + 0.05),
                         expand = expansion(mult = c(0, 0.02))) +
      labs(x = "Candidate market (each step adds the next closest substitute)", y = "Share of sales lost") +
      lab_gg() + theme(legend.position = "none")
  })

  output$st_table <- renderTable({
    d <- steps()
    data.frame(Step = d$step, `Candidate market` = d$set, `Share recaptured` = pct(d$A, 0),
               `Sales lost` = pct(d$loss), `Critical loss` = pct(d$cl),
               `Price increase` = ssnip_word(d$loss, d$cl),
               Conclusion = ifelse(d$market, "This is the relevant market",
                                   ifelse(d$ok, "", ifelse(d$shown, "Add the next product", ""))),
               check.names = FALSE)
  }, striped = TRUE, hover = TRUE, width = "100%")

  # Tab 2 ---------------------------------------------------------------------
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
  output$cl_box_loss <- renderUI(number_box("Predicted loss", pct(cl_res()$L), if (cl_res()$L >= 1 - 1e-9) "All sales lost: with a margin this low, customers are so price-sensitive that the price increase empties the market" else "Share of sales the group would lose",
                                            lab_colors[["actual"]]))
  output$cl_box_verdict <- renderUI({
    r <- cl_res()
    w <- ssnip_word(r$L, r$cl)
    verdict_box("SSNIP test", paste("Price increase", w), r$ok,
      paste0("Profit changes by ", pct(r$pc), ". ",
             if (r$ok) "The relevant market is this group or a narrower one." else if (w == "breaks even") "Exactly at break-even the test is not passed: widen the candidate market." else "Widen the candidate market."))
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
      return(card(card_body(p(paste0("With profit-maximizing prices, the test is passed exactly when the share recaptured by the group ",
        "(", pct(r$A, 0), ") exceeds the critical loss (", pct(r$cl), "). ",
        "High margins lower the bar: the group can be a market even when most switchers leave it.")))))
    }
    me <- r$m * input$cl_eps
    if (me > 1 + 1e-9) {
      card(class = "border-warning", card_body(
        p(strong("Check: "), paste0("Inconsistent. If each firm prices optimally, the group's demand cannot be more elastic than 1/m = ",
          formatC(1 / r$m, format = "f", digits = 2), ". Either the margin or the elasticity estimate is too high. ",
          "Using both overstates the loss and makes the market look too wide (Katz and Shapiro 2003)."))))
    } else card(card_body(p(paste0("Consistent: with optimal pricing, this elasticity implies that ", pct(1 - me, 0),
      " of lost customers stay inside the group."))))
  })

  # Tab 3 ---------------------------------------------------------------------
  output$ce_p0_ui <- renderUI({
    c0 <- input$ce_c; pm <- lin_pm(input$ce_pmax, c0)
    dp <- as.numeric(input$ce_dp)
    hi <- floor(10 * pm - 1) / 10   # up to just below the monopoly price, so that the verdict can flip
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
    pp <- seq(0, pmax, length.out = 200)
    dem <- data.frame(p = pp, q = lin_q(pp, pmax))
    pts <- data.frame(p = d$price, q = lin_q(d$price, pmax),
                      lab = paste0(c("Competitive benchmark", "Observed (monopoly) price"), "\nloses ", pct(d$predicted_loss),
                                   " vs. critical loss ", pct(d$critical_loss)))
    up  <- data.frame(p0 = d$price, p1 = d$price * (1 + dp), q0 = lin_q(d$price, pmax), q1 = lin_q(d$price * (1 + dp), pmax))
    ggplot(dem, aes(q, p)) +
      geom_line(linewidth = 1.2, color = lab_colors[["ink"]]) +
      geom_hline(yintercept = c0, color = lab_colors[["muted"]], linetype = "dashed") +
      annotate("text", x = 98, y = c0, label = "Unit cost", hjust = 1, vjust = -0.5, color = lab_colors[["muted"]], size = 4.5) +
      geom_segment(data = up, aes(x = q0, xend = q1, y = p1, yend = p1), inherit.aes = FALSE,
                   color = lab_colors[["actual"]], linewidth = 1, arrow = arrow(length = unit(0.25, "cm"))) +
      geom_segment(data = up, aes(x = q0, xend = q0, y = p0, yend = p1), inherit.aes = FALSE,
                   color = lab_colors[["actual"]], linewidth = 1) +
      geom_point(data = pts, aes(q, p), size = 4.5, color = c(lab_colors[["pass"]], lab_colors[["ink"]])) +
      geom_text(data = pts, aes(q, p, label = lab), hjust = ifelse(pts$q > 60, 1.05, -0.08), vjust = ifelse(pts$q > 60, 1.4, -0.5), size = 4.6,
                lineheight = 0.95, color = lab_colors[["ink"]]) +
      scale_x_continuous(limits = c(0, 100)) +
      labs(x = "Quantity of cellophane (index, 100 = price of zero)", y = "Price (€)",
           caption = paste0("Orange arrows: a ", pct(dp, 0), " price increase and the quantity lost. With linear demand, customers also react more at higher prices; the fallacy holds for any demand.")) +
      lab_gg()
  })

  output$ce_table <- renderTable({
    d <- ce()
    data.frame(`Starting price` = d$start, `Price (€)` = sprintf("%.2f", d$price),
               Margin = pct(d$margin), `Elasticity (absolute value)` = sprintf("%.2f", d$elasticity),
               `Critical loss` = pct(d$critical_loss), `Predicted loss` = pct(d$predicted_loss),
               `Price increase` = ifelse(d$verdict == "profitable", "profitable", "not profitable"), check.names = FALSE)
  }, striped = TRUE, width = "100%")
}

shinyApp(ui, server)
