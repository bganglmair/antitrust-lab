# Antitrust Lab: Welfare sandbox
library(shiny)
library(bslib)
library(ggplot2)

# Shiny sources R/ automatically (theme.R, welfare.R, cases.R)

app_version <- "0.6 (2026-10-02)"

number_box <- function(title, value, note = NULL, color = lab_colors[["critical"]]) {
  value_box(title = title, value = value,
            theme = value_box_theme(bg = lab_colors[["bg"]], fg = color),
            if (!is.null(note)) p(note, style = paste0("color:", lab_colors[["muted"]])))
}
muted <- function(...) p(..., style = paste0("color:", lab_colors[["muted"]], "; font-size:.9rem;"))
num <- function(x, digits = 1) formatC(sign(x) * floor(abs(x) * 10^digits + 0.5 + 1e-9) / 10^digits + 0, format = "f", digits = digits, big.mark = ",")  # half-up; + 0 turns -0 into 0

col_cs <- lab_colors[["pass"]]      # consumer surplus
col_ps <- "#8C7A66"                 # producer surplus / transfer
col_dwl <- lab_colors[["actual"]]   # deadweight loss

wc <- welfare_cases$constant
sb <- sidebar(
  width = 330,
  conditionalPanel("input.tab == 't1' || input.tab == 't2'",
    selectInput("case", "Example", setNames(names(welfare_cases), sapply(welfare_cases, `[[`, "label"))),
    sliderInput("a", "Highest price any buyer would pay (a)", wc$amin, wc$amax, wc$a, wc$astep, ticks = FALSE),
    sliderInput("b", "Slope of demand (b)", 0.5, 3, wc$b, 0.25, ticks = FALSE),
    sliderInput("c", "Marginal cost of the first unit (c)", wc$cmin, wc$cmax, wc$c, wc$cstep, ticks = FALSE),
    conditionalPanel("input.case == 'rising'",
      sliderInput("d", "How fast marginal cost rises with output (d)", 0.25, 3, welfare_cases$rising$d, 0.25, ticks = FALSE))
  ),
  conditionalPanel("input.tab == 't1'",
    tags$hr(),
    radioButtons("regime", "Show the market under", c("Monopoly" = "m", "Competition" = "c", "Both" = "b"), inline = TRUE)
  ),
  conditionalPanel("input.tab == 't2'",
    tags$hr(),
    sliderInput("alpha", "Share of the rents spent on getting or defending the monopoly (\u03b1)", 0, 100, 0, 5, post = "%", ticks = FALSE),
    muted(em("\u03b1 is a thought experiment, not an estimate."))
  ),
  conditionalPanel("input.tab == 't1' || input.tab == 't2'", muted(em("Illustrative numbers."))),
  conditionalPanel("input.tab == 't3'", muted("Predict first, then check with the tool, then reveal."))
)

tab1 <- nav_panel("1 Monopoly and deadweight loss", value = "t1",
  uiOutput("note"),
  uiOutput("guard"),
  layout_columns(fill = FALSE, col_widths = c(4, 4, 4),
    uiOutput("b_price"), uiOutput("b_qty"), uiOutput("b_lerner")),
  layout_columns(fill = FALSE, col_widths = c(4, 4, 4),
    uiOutput("b_cs"), uiOutput("b_ps"), uiOutput("b_dwl")),
  card(card_header("Consumer surplus, producer surplus, and deadweight loss"),
       plotOutput("plot1", height = "420px"),
       card_footer("Under a total-welfare standard, what buyers pay extra is a transfer to the firm, not a loss. The deadweight loss is surplus that nobody gets, because units that are worth more to buyers than they cost to make are not produced. Consumers lose both."))
)

tab2 <- nav_panel("2 Rent seeking", value = "t2",
  card(card_body(
    p("A monopoly position is valuable, so firms spend resources, for example on lobbying or litigation, to obtain or defend it. Whatever part of the rents is wasted in this way is no longer a transfer but a cost to society. Spending that also produces something useful, such as research, does not count."),
    p("Harberger (1954) counted only the triangle. Tullock (1967) and Posner (1975) argued that the rents may be spent in the contest for them."))),
  uiOutput("guard2"),
  layout_columns(fill = FALSE, col_widths = c(4, 4, 4),
    uiOutput("r_dwl"), uiOutput("r_rents"), uiOutput("r_cost")),
  card(card_header("Social cost of monopoly = deadweight loss + \u03b1 \u00d7 rents"),
       plotOutput("plot2", height = "260px")),
  uiOutput("r_note")
)

experiment <- function(id, question, choices, answer) {
  card(
    card_header(question),
    radioButtons(paste0("ex_", id), "Your prediction", choices, selected = character(0), width = "100%"),
    actionButton(paste0("exb_", id), "Reveal", class = "btn-outline-primary btn-sm"),
    conditionalPanel(paste0("input.exb_", id, " > 0"), div(style = "margin-top:.6rem;", answer))
  )
}

tab3 <- nav_panel("Questions", value = "t3",
  layout_columns(col_widths = c(6, 6),
    experiment("1", "Choose constant marginal cost in tab 1. The monopolist earns a profit of 9. How much of it is a loss to society?",
      c("All 9", "None of it", "A third of it"),
      p(strong("None of it, under a total-welfare standard. "), "The 9 is a transfer, because buyers pay 9 more for the 3 units they still buy and the firm receives exactly that. The loss to society is the triangle, 4.5. There are two qualifications. First, under a consumer-welfare standard, the transfer counts as a loss to consumers. Second, if the firm spends resources to get or keep the monopoly, that part is a real cost (tab 2).")),
    experiment("2", "Choose constant marginal cost in tab 1. Buyers value the product more (raise a). What happens to the Lerner index and the deadweight loss?",
      c("Both rise", "Lerner index rises, loss unchanged", "Nothing, because cost has not changed"),
      p(strong("Both rise. "), "Demand becomes less elastic at the monopoly price, so the markup grows. The Lerner index equals 1/|\u03b7| (at the starting values, 0.75 = 1/1.33). A larger gap between price and cost and more units that are no longer sold mean a larger triangle. As a share of the surplus under competition, the loss stays at 25% when cost is constant.")),
    experiment("3", "Choose constant marginal cost in tab 2. All rents are spent on lobbying (\u03b1 = 100%). How large is the social cost compared with Harberger's triangle?",
      c("The same", "Twice as large", "Three times as large"),
      p(strong("Three times. "), "The triangle is 4.5 and the rents are 9, so the social cost is 13.5.")),
    experiment("4", "Switch to the rising-cost example. Under competition, do sellers earn any surplus?",
      c("No, price equals marginal cost", "Yes"),
      p(strong("Yes. "), "Price equals the cost of the last unit, but earlier units cost less, so sellers earn producer surplus even under competition. That is why the monopoly rents are the profit above the competitive level, not the whole profit."))
  )
)

tab4 <- nav_panel("Math", value = "t4",
  withMathJax(),
  card(card_header("Market"),
    p("Inverse demand is \\(p = a - bQ\\) and marginal cost is \\(\\text{MC}(Q) = c + dQ\\), with \\(a > c\\). Here \\(c\\) is the cost of the first unit, and marginal cost is constant when \\(d = 0\\). There is no fixed cost."),
    p("Under competition, price equals marginal cost, so \\(Q^{\\text{c}} = (a - c)/(b + d)\\). Under monopoly, marginal revenue \\(a - 2bQ\\) equals marginal cost, so \\(Q^{\\text{m}} = (a - c)/(2b + d)\\) and \\(p^{\\text{m}} = a - bQ^{\\text{m}}\\).")),
  card(card_header("Surplus"),
    p("At each outcome's quantity \\(Q\\) and price \\(p\\), consumer surplus is \\(\\text{CS} = bQ^2/2\\), producer surplus is \\(\\text{PS} = pQ - cQ - dQ^2/2\\), and welfare is \\(W = \\text{CS} + \\text{PS}\\)."),
    p("The deadweight loss is \\(\\text{DWL} = W^{\\text{c}} - W^{\\text{m}} = (p^{\\text{m}} - \\text{MC}(Q^{\\text{m}}))(Q^{\\text{c}} - Q^{\\text{m}})/2\\)."),
    p("The Lerner index is \\(L = (p^{\\text{m}} - \\text{MC}(Q^{\\text{m}}))/p^{\\text{m}} = 1/|\\eta|\\), where \\(\\eta\\) is the elasticity of demand at the monopoly price.")),
  card(card_header("Rent seeking"),
    p("Rents are the profit above the competitive level, \\(\\text{PS}^{\\text{m}} - \\text{PS}^{\\text{c}}\\). With constant cost this is the monopoly profit and equals the transfer from buyers, \\((p^{\\text{m}} - p^{\\text{c}})Q^{\\text{m}}\\). With rising cost, the rents are smaller than both, namely \\((p^{\\text{m}} - p^{\\text{c}})Q^{\\text{m}} - d\\,(Q^{\\text{c}} - Q^{\\text{m}})^2/2\\). The social cost of monopoly is \\(\\text{DWL} + \\alpha \\times \\text{rents}\\). The benchmarks are \\(\\alpha = 0\\) (Harberger) and \\(\\alpha = 1\\) (Posner, all rents wasted), and the tool lets \\(\\alpha\\) range between them.")),
  card(card_header("Sources"),
    tags$ul(
      lab_ref("Harberger, Arnold C. 1954. \"Monopoly and Resource Allocation.\" ", em("American Economic Review"), " 44 (2): 77\u201387."),
      lab_ref("Posner, Richard A. 1975. \"The Social Costs of Monopoly and Regulation.\" ", em("Journal of Political Economy"), " 83 (4): 807\u201327.", url = "https://doi.org/10.1086/260357"),
      lab_ref("Tullock, Gordon. 1967. \"The Welfare Costs of Tariffs, Monopolies, and Theft.\" ", em("Western Economic Journal"), " 5 (3): 224\u201332.", url = "https://doi.org/10.1111/j.1465-7295.1967.tb01923.x")))
)

ui <- page_navbar(
  id = "tab",
  fillable = FALSE,
  title = lab_title("Welfare"),
  window_title = "Antitrust Lab",
  theme = lab_theme(),
  navbar_options = lab_navbar(),
  sidebar = sb,
  header = div(class = "container-fluid",
    lab_header("What does market power cost society?", "Topic: market power and welfare", tool = "Monopoly and welfare")),
  footer = div(class = "container-fluid",
    style = paste0("color:", lab_colors[["muted"]], "; font-size:.85rem; padding:.5rem 0;"),
    paste("Version", app_version, "\u00b7 All numbers are illustrative.")),
  tab1, tab2, tab3, tab4
)

server <- function(input, output, session) {

  observeEvent(input$case, {
    cs <- welfare_cases[[input$case]]
    updateSliderInput(session, "a", min = cs$amin, max = cs$amax, step = cs$astep, value = cs$a)
    updateSliderInput(session, "c", min = cs$cmin, max = cs$cmax, step = cs$cstep, value = cs$c)
    updateSliderInput(session, "b", value = cs$b)
    if (input$case == "rising") updateSliderInput(session, "d", value = cs$d)
  }, ignoreInit = TRUE)

  # Constant marginal cost: the slope is zero and its slider is hidden
  d_eff <- reactive(if (identical(input$case, "rising")) input$d else 0)
  out <- reactive(welfare_outcomes(input$a, input$b, input$c, d_eff()))
  ok <- reactive(!is.null(out()))

  output$note <- renderUI({
    cs <- welfare_cases[[input$case]]
    same <- isTRUE(all.equal(c(input$a, input$b, input$c, d_eff()), c(cs$a, cs$b, cs$c, cs$d)))
    if (is.null(cs$note) || !same) NULL else card(card_body(p(cs$note)))
  })
  output$guard2 <- renderUI({
    if (ok()) NULL else card(class = "border-warning", card_body(p(strong("No trade. "),
      "No buyer is willing to pay the cost of the first unit. Raise a or lower c.")))
  })
  output$guard <- renderUI({
    if (ok()) NULL else card(class = "border-warning", card_body(p(strong("No trade. "),
      "No buyer is willing to pay the cost of the first unit. Raise a or lower c.")))
  })

  mono <- reactive(input$regime != "c")   # monopoly values in "Monopoly" and "Both"
  both <- reactive(input$regime == "b")

  output$b_price <- renderUI({ req(ok()); o <- out()
    number_box("Price", num(if (mono()) o$pm else o$pc, 2),
               if (mono()) paste0("Competitive price: ", num(o$pc, 2)) else paste0("Monopoly price: ", num(o$pm, 2))) })
  output$b_qty <- renderUI({ req(ok()); o <- out()
    number_box("Quantity", num(if (mono()) o$Qm else o$Qc, 2),
               if (mono()) paste0("Competitive quantity: ", num(o$Qc, 2)) else paste0("Monopoly quantity: ", num(o$Qm, 2))) })
  output$b_lerner <- renderUI({ req(ok()); o <- out()
    if (mono()) number_box("Lerner index (p \u2212 MC)/p", pct(o$lerner),
                           paste0("= 1/|\u03b7|, with |\u03b7| = ", num(o$eta_abs, 2), " at this price"))
    else number_box("Lerner index (p \u2212 MC)/p", pct(0), "Price equals marginal cost") })
  output$b_cs <- renderUI({ req(ok()); o <- out()
    number_box("Consumer surplus", num(if (mono()) o$cs_m else o$cs_c), if (both()) paste0("Under competition: ", num(o$cs_c)) else NULL, col_cs) })
  output$b_ps <- renderUI({ req(ok()); o <- out()
    number_box("Producer surplus", num(if (mono()) o$ps_m else o$ps_c),
               if (both()) paste0("Under competition: ", num(o$ps_c), ". Transferred from buyers: ", num(o$transfer)) else if (mono()) paste0("of which transferred from buyers: ", num(o$transfer)) else NULL, col_ps) })
  output$b_dwl <- renderUI({ req(ok()); o <- out()
    number_box("Deadweight loss", num(if (mono()) o$dwl else 0),
               if (mono()) paste0(pct(o$dwl / o$w_c), " of the surplus under competition") else "None, because every worthwhile unit is produced", col_dwl) })

  output$plot1 <- renderPlot({
    validate(need(ok(), "Raise a or lower c."))
    o <- out(); a <- input$a; b <- input$b; c0 <- input$c; d <- d_eff()
    Q <- if (mono()) o$Qm else o$Qc; p <- if (mono()) o$pm else o$pc
    qmax <- o$Qc * 1.35
    cs_poly <- data.frame(x = c(0, 0, Q), y = c(a, p, p))
    ps_poly <- data.frame(x = c(0, Q, Q, 0), y = c(p, p, mc_at(Q, c0, d), c0))
    g <- ggplot() +
      geom_polygon(data = cs_poly, aes(x, y), fill = col_cs, alpha = 0.25) +
      geom_polygon(data = ps_poly, aes(x, y), fill = col_ps, alpha = 0.35)
    if (mono()) {
      dwl_poly <- data.frame(x = c(o$Qm, o$Qc, o$Qm), y = c(o$pm, o$pc, o$mcm))
      g <- g + geom_polygon(data = dwl_poly, aes(x, y), fill = col_dwl, alpha = 0.55) +
        annotate("segment", x = 0, xend = min(a / (2 * b), qmax), y = a, yend = a - 2 * b * min(a / (2 * b), qmax),
                 color = lab_colors[["muted"]], linewidth = 0.9, linetype = "dashed") +
        annotate("text", x = min(a / (2 * b), qmax) * 0.8, y = a - 2 * b * min(a / (2 * b), qmax) * 0.8,
                 label = "Marginal revenue", hjust = 1.08, color = lab_colors[["muted"]], size = 4.3) +
        annotate("text", x = o$Qm + (o$Qc - o$Qm) * 0.3, y = (o$pm + o$pc + o$mcm) / 3, label = "Deadweight\nloss", lineheight = 0.9, color = lab_colors[["ink"]], size = 4.6)
    }
    if (both()) {
      g <- g +
        annotate("segment", x = 0, xend = o$Qc, y = o$pc, yend = o$pc, linetype = "dotted", color = lab_colors[["ink"]]) +
        annotate("segment", x = o$Qc, xend = o$Qc, y = 0, yend = o$pc, linetype = "dotted", color = lab_colors[["ink"]]) +
        annotate("point", x = c(o$Qm, o$Qc), y = c(o$pm, o$pc), size = 4, color = lab_colors[["ink"]]) +
        annotate("label", x = o$Qm, y = o$pm, label = "Monopoly", hjust = -0.15, vjust = -0.4, size = 4.6, fill = lab_colors[["bg"]], label.size = 0, color = lab_colors[["ink"]]) +
        annotate("label", x = o$Qc, y = o$pc, label = "Competition", hjust = -0.12, vjust = -0.4, size = 4.6, fill = lab_colors[["bg"]], label.size = 0, color = lab_colors[["ink"]])
    }
    g +
      annotate("segment", x = 0, xend = qmax, y = a, yend = a - b * qmax, color = lab_colors[["ink"]], linewidth = 1.2) +
      annotate("segment", x = 0, xend = qmax, y = c0, yend = c0 + d * qmax, color = lab_colors[["ink"]], linewidth = 1.2) +
      annotate("text", x = 0.12 * o$Qc, y = a - b * 0.12 * o$Qc, label = "Demand", hjust = -0.15, vjust = -0.4, color = lab_colors[["ink"]], size = 4.6) +
      annotate("text", x = qmax, y = c0 + d * qmax, label = "Marginal cost", hjust = 1, vjust = -0.7, color = lab_colors[["ink"]], size = 4.6) +
      annotate("segment", x = 0, xend = Q, y = p, yend = p, linetype = "dotted", color = lab_colors[["ink"]]) +
      annotate("segment", x = Q, xend = Q, y = 0, yend = p, linetype = "dotted", color = lab_colors[["ink"]]) +
      annotate("text", x = Q / 5, y = p + (a - p) / 4, label = "Consumer\nsurplus", color = col_cs, size = 4.6, lineheight = 0.9) +
      annotate("text", x = Q / 2.2, y = (p + mc_at(Q / 2.2, c0, d)) / 2,
               label = if ((if (mono()) o$ps_m else o$ps_c) > 1e-9) "Producer\nsurplus" else "", color = lab_colors[["ink"]], size = 4.6, lineheight = 0.9) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.02)),
                         breaks = if (mono()) c(o$Qm, o$Qc) else o$Qc, labels = function(x) num(x, 1)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.02)),
                         breaks = sort(unique(round(c(0, c0, if (mono()) c(o$pm, o$pc) else o$pc, a), 2))), labels = function(x) num(x, 1)) +
      coord_cartesian(xlim = c(0, qmax), ylim = c(0, a * 1.02)) +
      labs(x = "Quantity", y = "Price") + lab_gg()
  })

  # Tab 2 ---------------------------------------------------------------------
  cost <- reactive({ req(ok()); o <- out(); al <- input$alpha / 100
    list(dwl = o$dwl, rents = o$rents, spent = al * o$rents, kept = (1 - al) * o$rents,
         total = social_cost(o$dwl, o$rents, al), w_c = o$w_c) })

  output$r_dwl <- renderUI(number_box("Deadweight loss (triangle)", num(cost()$dwl), "For Harberger, this is the whole cost", col_dwl))
  output$r_rents <- renderUI(number_box("Rents (profit above the competitive level)", num(cost()$rents),
                                        paste0("spent on rent seeking: ", num(cost()$spent)), col_ps))
  output$r_cost <- renderUI(number_box("Social cost of monopoly", num(cost()$total),
                                       paste0(pct(cost()$total / cost()$w_c), " of the surplus under competition")))

  output$plot2 <- renderPlot({
    k <- cost()
    d <- data.frame(part = factor(c("Deadweight loss", "Rents spent on rent seeking", "Rents kept by the firm"),
                                  levels = c("Rents kept by the firm", "Rents spent on rent seeking", "Deadweight loss")),
                    v = c(k$dwl, k$spent, k$kept))
    d$mid <- c(k$dwl / 2, k$dwl + k$spent / 2, k$dwl + k$spent + k$kept / 2)
    d$lab <- ifelse(d$v > 0.04 * sum(d$v), paste0(d$part, "\n", num(d$v)), "")
    ggplot(d, aes(x = 1, y = v, fill = part)) +
      geom_col(width = 0.55) +
      geom_text(aes(y = mid, label = lab), color = c(lab_colors[["ink"]], "white", lab_colors[["ink"]]), size = 4.6, lineheight = 0.95) +
      annotate("segment", x = 1.36, xend = 1.36, y = 0, yend = k$total, color = lab_colors[["ink"]], linewidth = 1) +
      annotate("text", x = 1.44, y = k$total / 2, label = paste0("Social cost: ", num(k$total)), color = lab_colors[["ink"]], size = 4.8) +
      scale_fill_manual(values = c("Deadweight loss" = col_dwl, "Rents spent on rent seeking" = lab_colors[["ink"]],
                                   "Rents kept by the firm" = lab_colors[["fail"]]), guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.03)), labels = function(x) num(x, 0)) +
      scale_x_continuous(limits = c(0.6, 1.6)) +
      coord_flip() + labs(x = NULL, y = NULL) +
      lab_gg() + theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank(),
                       panel.grid.major.x = element_line(color = lab_colors[["grid"]]))
  })

  output$r_note <- renderUI({
    if (isTRUE(d_eff() > 0) && ok()) card(card_body(muted(paste0("With rising marginal cost, sellers earn producer surplus even under competition, so the rents are smaller than the monopolist's whole profit. They are also smaller than what buyers pay extra (", num(out()$transfer), "), because the firm gives up surplus on the units it no longer sells.")))) else NULL
  })
}

shinyApp(ui, server)
