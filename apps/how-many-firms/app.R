# Antitrust Lab: How many firms?
library(shiny)
library(bslib)
library(ggplot2)

# Shiny sources R/ automatically (theme.R, firms.R)

app_version <- "0.3 (2026-10-02)"

number_box <- function(title, value, note = NULL, color = lab_colors[["critical"]]) {
  value_box(title = title, value = value,
            theme = value_box_theme(bg = lab_colors[["bg"]], fg = color),
            if (!is.null(note)) p(note, style = paste0("color:", lab_colors[["muted"]])))
}
muted <- function(...) p(..., style = paste0("color:", lab_colors[["muted"]], "; font-size:.9rem;"))
num <- function(x, digits = 1) formatC(round(x, digits) + 0, format = "f", digits = digits, big.mark = ",")  # + 0 turns -0 into 0

col_cs <- lab_colors[["pass"]]
col_ps <- "#8C7A66"
col_w <- lab_colors[["ink"]]
nshow <- 20

sb <- sidebar(
  width = 330,
  conditionalPanel("input.tab == 't1' || input.tab == 't2' || input.tab == 'tb'",
    sliderInput("a", "Highest price any buyer would pay (a)", 60, 160, 100, 10, ticks = FALSE),
    sliderInput("c", "Marginal cost (c)", 0, 60, 40, 5, ticks = FALSE),
    sliderInput("F", "Fixed cost per firm (F)", 0, 1500, 100, 10, ticks = FALSE)
  ),
  conditionalPanel("input.tab == 't1'",
    sliderInput("n", "Number of firms (n)", 1, 12, 2, 1, ticks = FALSE)
  ),
  conditionalPanel("input.tab == 'tb'",
    sliderInput("g", "How close substitutes are the products? (\u03b3)", 0, 30, 3, 1, ticks = FALSE),
    muted("0: no price competition, the firms simply share the buyers. 30: nearly identical products."),
    sliderInput("nb", "Number of firms (n)", 1, 12, 2, 1, ticks = FALSE)
  ),
  conditionalPanel("input.tab == 't1' || input.tab == 't2' || input.tab == 'tb'", muted(em("Illustrative numbers."))),
  conditionalPanel("input.tab == 't3'", muted("Predict first, then check with the tool, then reveal."))
)

intro <- card(card_body(
  p("Every firm must pay a fixed cost before it sells anything: a plant, a network, research facilities. More firms compete the price down, but each of them pays the fixed cost again."),
  p(strong("Before you move a slider: "), "is the market better off with more firms?")))

tab1 <- nav_panel("1 The trade-off", value = "t1",
  intro,
  uiOutput("guard"),
  layout_columns(fill = FALSE, col_widths = c(4, 4, 4),
    uiOutput("v_w"), uiOutput("v_e"), uiOutput("v_cs")),
  layout_columns(fill = FALSE, col_widths = c(4, 4, 4, 4, 4, 4),
    uiOutput("b_p"), uiOutput("b_pi"), uiOutput("b_l"), uiOutput("b_cs"), uiOutput("b_w"), uiOutput("b_ps")),
  card(card_header("Consumer surplus, producer surplus and welfare as firms are added"),
       plotOutput("plot1", height = "420px"),
       card_footer("Free entry: firms enter as long as profit is not negative. An entrant counts its own profit, not the sales it takes from the firms already there, so entry usually goes further than welfare would call for. It also does not count the gain to consumers, which is why entry can fall one firm short when the fixed cost is very high.")),
  muted("A simple model with identical products: firms choose quantities (Cournot). Tab 3 lets firms sell different varieties and set prices.")
)

tab2 <- nav_panel("2 Fixed costs and market structure", value = "t2",
  card(card_body(
    p("How many firms are best for welfare depends on the fixed cost. When it is large, the price cut from a second firm no longer covers the duplicated fixed cost, and one firm is best. Industries with very large fixed costs are often run as a single regulated firm. The smaller the fixed cost, the more firms welfare calls for; only with no fixed cost are more firms always better."),
    p("This tab keeps the model of tab 1. Tab 3 changes how the firms compete."))),
  layout_columns(fill = FALSE, col_widths = c(6, 6), uiOutput("f_w"), uiOutput("f_e")),
  card(card_header("Number of firms against the fixed cost"),
       plotOutput("plot2", height = "400px"),
       card_footer("Dotted line: the fixed cost you chose (the chart starts at 5)."))
)

tabb <- nav_panel("3 Price competition", value = "tb",
  card(card_body(
    p("In tab 1 all firms sell the same product and choose quantities. Here each firm sells its own variety and sets its price. How close the varieties are decides how hard the firms compete: when the varieties are far apart, each firm prices like a monopolist on its share of the buyers; with nearly identical products two firms already push the price close to cost."),
    p(strong("Before you move a slider: "), "does tougher price competition attract more entry or less?"))),
  uiOutput("guard_b"),
  layout_columns(fill = FALSE, col_widths = c(4, 4, 4),
    uiOutput("bv_w"), uiOutput("bv_e"), uiOutput("bv_c")),
  layout_columns(fill = FALSE, col_widths = c(4, 4, 4, 4, 4, 4),
    uiOutput("bb_p"), uiOutput("bb_pi"), uiOutput("bb_l"), uiOutput("bb_cs"), uiOutput("bb_w"), uiOutput("bb_ps")),
  card(card_header("Consumer surplus, producer surplus and welfare as firms are added"),
       plotOutput("plotb", height = "420px"),
       card_footer("Entry depends on the profit a firm expects after it has entered. Soft competition leaves high margins and attracts many firms; tough competition leaves thin margins, so few firms enter: often just the number welfare calls for, and sometimes one fewer, because an entrant does not count the gain to consumers.")),
  muted("At equal prices buyers purchase the same total quantity however many varieties are on offer. The model therefore leaves out the value of variety itself; where buyers value variety, more firms are worth more than shown here. This matters most when the products are far apart (low \u03b3): there the verdict of too many firms can shrink or reverse.")
)

experiment <- function(id, question, choices, answer) {
  card(
    card_header(question),
    radioButtons(paste0("ex_", id), "Your prediction", choices, selected = character(0)),
    actionButton(paste0("exb_", id), "Reveal", class = "btn-outline-primary btn-sm"),
    conditionalPanel(paste0("input.exb_", id, " > 0"), div(style = "margin-top:.6rem;", answer))
  )
}

tab3 <- nav_panel("Experiments", value = "t3",
  layout_columns(col_widths = c(6, 6),
    experiment("1", "Default numbers: a third firm enters and the price falls from 60 to 55. Does welfare rise?",
      c("Yes: a lower price is always better", "No"),
      p(strong("No. "), "Welfare falls from 1,400 to 1,387.5. Consumers gain 212.5, but the three firms together earn 225 less than the two did, because the newcomer pays the fixed cost of 100 and mostly takes sales from the others.")),
    experiment("2", "Default numbers: would entry stop at two firms on its own?",
      c("Yes, the market finds the best number", "No, more firms enter"),
      p(strong("No. "), "A third and a fourth firm still earn a profit, and a fifth just breaks even, so they enter. Free entry stops at five firms. Welfare is then 1,250; with these numbers that happens to be the same as under monopoly.")),
    experiment("3", "An authority that looks only at consumer surplus: how many firms does it want?",
      c("Two", "Five", "As many as possible"),
      p(strong("As many as possible. "), "Consumer surplus rises with every additional firm. But firms enter only while profit is not negative, so in practice the consumer standard ends at the free-entry number, five. The consumer standard and the total-welfare standard disagree about the best market structure.")),
    experiment("4", "The fixed cost falls from 100 to 10. What happens to the number of firms with the highest welfare?",
      c("It stays at 2", "It rises", "It falls"),
      p(strong("It rises, from 2 to 6. "), "Duplicating a small fixed cost is cheap, so the gain from lower prices dominates for longer. Free entry now gives 17 firms.")),
    experiment("5", "Tab 1: there is no fixed cost (F = 0). Is there still a trade-off?",
      c("Yes", "No"),
      p(strong("No. "), "Without fixed costs every additional firm lowers the price at no cost to society. Welfare keeps rising with the number of firms.")),
    experiment("6", "Tab 3, default numbers: firms sell different varieties and set prices. Compared with tab 1 (free entry: five firms), free entry gives ...",
      c("Fewer firms", "The same number", "More firms"),
      p(strong("More firms: six. "), "Different varieties shield each firm from its rivals. From the fourth firm on, the price falls more slowly than in tab 1 (54.1 against 52 with four firms), so entry stays profitable for one firm longer. Welfare is still highest with two firms.")),
    experiment("7", "Tab 3: the products become nearly identical (\u03b3 = 30). How many firms enter?",
      c("One", "Two", "More than six"),
      p(strong("One. "), "With two firms the price would fall from 70 to about 43.5, and each firm would earn 99.7 before the fixed cost of 100, a loss of 0.3. The second firm stays out and the monopoly price of 70 stands, although welfare would be higher with two firms (about 1,594 against 1,250). With a fixed cost of 200 the loss is larger and the result is the same. Tough competition after entry can mean one firm too few."))
  )
)

tab4 <- nav_panel("Model", value = "t4",
  withMathJax(),
  card(card_header("Cournot competition with a fixed cost"),
    p("\\(n\\) equal firms choose quantities. Demand \\(p = a - Q\\), marginal cost \\(c\\), fixed cost \\(F\\) per firm."),
    p("$$q = \\frac{a - c}{n + 1}, \\qquad p = \\frac{a + nc}{n + 1}, \\qquad \\pi = q^2 - F.$$"),
    p("$$\\text{CS} = \\frac{n^2 (a - c)^2}{2 (n + 1)^2}, \\qquad \\text{PS} = n\\pi, \\qquad W = \\text{CS} + \\text{PS}.$$"),
    p("Lerner index \\(L = (p - c)/p\\).")),
  card(card_header("Price competition with differentiated products (tab 3)"),
    p("Each of \\(n\\) firms sells one variety and sets its price. Demand for variety \\(i\\) (Shubik and Levitan 1980), with \\(\\bar{p}\\) the average price of all \\(n\\) varieties:"),
    p("$$q_i = \\frac{1}{n}\\big[a - p_i - \\gamma\\,(p_i - \\bar{p})\\big], \\qquad \\gamma \\ge 0.$$"),
    p("Equilibrium price, with \\(k = 1 + \\gamma (n - 1)/n\\):"),
    p("$$p = \\frac{a + k c}{1 + k}, \\qquad Q = a - p, \\qquad \\pi = \\frac{(p - c)\\,Q}{n} - F, \\qquad \\text{CS} = \\frac{Q^2}{2}.$$"),
    p("With one firm, or with \\(\\gamma = 0\\), this is the monopoly price \\((a + c)/2\\). As \\(\\gamma\\) grows, the price with two or more firms approaches \\(c\\). The number of firms with the highest welfare and the free-entry number are found by comparing whole numbers.")),
  card(card_header("How many firms? (Cournot model, tabs 1 and 2)"),
    p("For \\(F > 0\\), welfare is highest at \\(n^{*} = \\big((a - c)^2/F\\big)^{1/3} - 1\\); the tool reports the best whole number (zero if no firm can add to welfare; the smaller number if two are tied)."),
    p("Free entry: the largest \\(n\\) with \\(\\pi \\ge 0\\), \\(n^{e} = (a - c)/\\sqrt{F} - 1\\), rounded down (zero if negative)."),
    p("Free entry usually gives too many firms, and never too few by more than one (Mankiw and Whinston 1986). In both models of this tool entry is never short by more than one firm. Where buyers value variety itself (not modeled here), entry can fall short by more.")),
  card(card_header("Sources"),
    tags$ul(
      tags$li("Mankiw, N. G., and M. D. Whinston (1986). Free entry and social inefficiency. RAND Journal of Economics 17(1): 48-58."),
      tags$li("Shubik, M., and R. Levitan (1980). Market Structure and Behavior. Harvard University Press.")))
)

welfare_plot <- function(d, nsel, nw, ne) {
  long <- rbind(data.frame(n = d$n, v = d$cs, what = "Consumer surplus"),
                data.frame(n = d$n, v = d$ps, what = "Producer surplus"),
                data.frame(n = d$n, v = d$w, what = "Welfare"))
  cols <- c("Consumer surplus" = col_cs, "Producer surplus" = col_ps, "Welfare" = col_w)
  ymax <- max(long$v)
  ylo <- min(0, max(min(long$v), -0.35 * ymax))
  vis <- long[long$v >= ylo, ]
  ends <- do.call(rbind, lapply(split(vis, vis$what), function(z) z[which.max(z$n), ]))
  ends <- ends[order(ends$v), ]; ends$ylab <- ends$v
  gap <- 0.07 * (ymax * 1.05 - ylo)
  for (k in seq_len(nrow(ends))[-1]) if (ends$ylab[k] - ends$ylab[k - 1] < gap) ends$ylab[k] <- ends$ylab[k - 1] + gap
  g <- ggplot(long, aes(n, v, color = what)) +
    geom_hline(yintercept = 0, color = lab_colors[["muted"]]) +
    geom_line(aes(linewidth = what)) + geom_point(size = 2) +
    geom_point(data = long[long$n == nsel, ], size = 5) +
    geom_text(data = ends, aes(y = ylab, label = what), hjust = 0, nudge_x = 0.3, size = 4.6, show.legend = FALSE) +
    scale_linewidth_manual(values = c("Consumer surplus" = 1, "Producer surplus" = 1, "Welfare" = 1.7), guide = "none") +
    scale_color_manual(values = cols, guide = "none") +
    scale_x_continuous(breaks = seq(1, nshow, 1), limits = c(1, nshow + 4.5)) +
    scale_y_continuous(labels = function(x) num(x, 0)) +
    coord_cartesian(ylim = c(ylo, ymax * 1.05)) +
    labs(x = "Number of firms", y = NULL) + lab_gg()
  if (is.finite(nw) && nw >= 1 && nw <= nshow)
    g <- g + annotate("segment", x = nw, xend = nw, y = ylo, yend = ymax, linetype = "dashed", color = col_w) +
      annotate("label", fill = lab_colors[["bg"]], label.size = 0, x = nw, y = ymax, label = if (isTRUE(nw == ne)) "Highest welfare = free entry" else "Highest welfare", hjust = -0.05, vjust = 1.2, color = col_w, size = 4.4)
  if (is.finite(ne) && ne >= 1 && ne <= nshow && ne != nw)
    g <- g + annotate("segment", x = ne, xend = ne, y = ylo, yend = ymax, linetype = "dashed", color = lab_colors[["actual"]]) +
      annotate("label", fill = lab_colors[["bg"]], label.size = 0, x = ne, y = ymax * 0.88, label = "Free entry", hjust = -0.05, vjust = 1.2, color = lab_colors[["actual"]], size = 4.4)
  g
}

ui <- page_navbar(
  id = "tab",
  fillable = FALSE,
  title = "Antitrust Lab · How many firms?",
  theme = lab_theme(),
  navbar_options = lab_navbar(),
  sidebar = sb,
  header = div(class = "container-fluid",
    lab_header("Are more competitors always better?", "Topic: market structure and welfare", tool = "How many firms?")),
  footer = div(class = "container-fluid",
    style = paste0("color:", lab_colors[["muted"]], "; font-size:.85rem; padding:.5rem 0;"),
    paste("Version", app_version, "· All numbers are illustrative.")),
  tab1, tab2, tabb, tab3, tab4
)

server <- function(input, output, session) {
  ok <- reactive(input$a > input$c)
  nw <- reactive(n_welfare(input$a, input$c, input$F))
  ne <- reactive(n_free_entry(input$a, input$c, input$F))
  cur <- reactive({ req(ok()); cournot_n(input$n, input$a, input$c, input$F) })

  output$guard <- renderUI({
    if (ok()) NULL else card(class = "border-warning", card_body(p(strong("No trade: "), "no buyer is willing to pay more than the marginal cost. Raise a or lower c.")))
  })

  w_note <- function(n) {
    if (is.infinite(n)) "No fixed cost: every extra firm helps"
    else if (n == 0) "Not even one firm adds to welfare"
    else paste0("Welfare there: ", num(cournot_n(n, input$a, input$c, input$F)$w))
  }
  e_note <- function(n) {
    if (is.infinite(n)) "No fixed cost: entry never stops"
    else if (n == 0) "Not even a monopolist covers the fixed cost"
    else paste0("Welfare there: ", num(cournot_n(n, input$a, input$c, input$F)$w))
  }

  output$v_w <- renderUI({ req(ok()); number_box("Welfare is highest with this many firms", fmt_n(nw(), nshow), w_note(nw())) })
  output$v_e <- renderUI({ req(ok()); number_box("Free entry stops at", fmt_n(ne(), nshow), e_note(ne()), lab_colors[["actual"]]) })
  output$v_cs <- renderUI({ req(ok()); number_box("Consumers prefer", "As many as possible",
                                                  "Consumer surplus rises with every firm; but firms enter only while they break even", col_cs) })

  output$b_p   <- renderUI(number_box(paste0("Price with ", input$n, if (input$n == 1) " firm" else " firms"), num(cur()$p, 2)))
  output$b_pi  <- renderUI(number_box("Profit per firm", num(cur()$profit, if (abs(cur()$profit) < 1) 2 else 1)))
  output$b_cs  <- renderUI(number_box("Consumer surplus", num(cur()$cs), NULL, col_cs))
  output$b_w   <- renderUI(number_box("Welfare", num(cur()$w)))
  output$b_l   <- renderUI(number_box("Lerner index", pct(cur()$lerner)))
  output$b_ps  <- renderUI(number_box("Producer surplus", num(cur()$ps), NULL, col_ps))

  output$plot1 <- renderPlot({
    validate(need(ok(), "Raise a or lower c."))
    welfare_plot(cournot_n(1:nshow, input$a, input$c, input$F), input$n, nw(), ne())
  })

  # Tab 3: price competition ------------------------------------------------------
  nwb <- reactive(n_welfare_b(input$a, input$c, input$F, input$g))
  neb <- reactive(n_free_entry_b(input$a, input$c, input$F, input$g))
  curb <- reactive({ req(ok()); bertrand_n(input$nb, input$a, input$c, input$F, input$g) })
  wb_note <- function(n) {
    if (isTRUE(input$F <= 0) && isTRUE(input$g <= 0)) "No fixed cost and no price competition: the number of firms does not matter"
    else if (is.infinite(n)) "No fixed cost: every extra firm helps"
    else if (n == 0) "Not even one firm adds to welfare"
    else paste0("Welfare there: ", num(bertrand_n(n, input$a, input$c, input$F, input$g)$w))
  }
  eb_note <- function(n) {
    if (is.infinite(n)) "No fixed cost: entry never stops"
    else if (n == 0) "Not even a monopolist covers the fixed cost"
    else paste0("Welfare there: ", num(bertrand_n(n, input$a, input$c, input$F, input$g)$w))
  }
  output$guard_b <- renderUI({
    if (ok()) NULL else card(class = "border-warning", card_body(p(strong("No trade: "), "no buyer is willing to pay more than the marginal cost. Raise a or lower c.")))
  })
  output$bv_w <- renderUI({ req(ok()); number_box("Welfare is highest with this many firms", fmt_n(nwb(), nshow), wb_note(nwb())) })
  output$bv_e <- renderUI({ req(ok()); number_box("Free entry stops at", fmt_n(neb(), nshow), eb_note(neb()), lab_colors[["actual"]]) })
  output$bv_c <- renderUI({ req(ok()); number_box("Tab 1 for comparison (same a, c and F)",
                                                  if (is.infinite(nw())) "As many as possible" else paste0("Best ", fmt_n(nw(), nshow), " \u00b7 Entry ", fmt_n(ne(), nshow)),
                                                  "Identical products, quantity competition", lab_colors[["muted"]]) })
  output$bb_p  <- renderUI(number_box(paste0("Price with ", input$nb, if (input$nb == 1) " firm" else " firms"), num(curb()$p, 2)))
  output$bb_pi <- renderUI(number_box("Profit per firm", num(curb()$profit, if (abs(curb()$profit) < 1) 2 else 1)))
  output$bb_l  <- renderUI(number_box("Lerner index", pct(curb()$lerner)))
  output$bb_cs <- renderUI(number_box("Consumer surplus", num(curb()$cs), NULL, col_cs))
  output$bb_w  <- renderUI(number_box("Welfare", num(curb()$w)))
  output$bb_ps <- renderUI(number_box("Producer surplus", num(curb()$ps), NULL, col_ps))
  output$plotb <- renderPlot({
    validate(need(ok(), "Raise a or lower c."))
    welfare_plot(bertrand_n(1:nshow, input$a, input$c, input$F, input$g), input$nb, nwb(), neb())
  })

  # Tab 2 ---------------------------------------------------------------------
  output$f_w <- renderUI({ req(ok()); number_box("Welfare is highest with this many firms", fmt_n(nw(), nshow), w_note(nw())) })
  output$f_e <- renderUI({ req(ok()); number_box("Free entry stops at", fmt_n(ne(), nshow), e_note(ne()), lab_colors[["actual"]]) })

  output$plot2 <- renderPlot({
    validate(need(ok(), "Raise a or lower c."))
    Fg <- exp(seq(log(5), log(1500), length.out = 240))
    d <- rbind(
      data.frame(F = Fg, n = pmin(sapply(Fg, function(f) n_welfare(input$a, input$c, f)), nshow), what = "Highest welfare"),
      data.frame(F = Fg, n = pmin(sapply(Fg, function(f) n_free_entry(input$a, input$c, f)), nshow), what = "Free entry"))
    cols <- c("Highest welfare" = col_w, "Free entry" = lab_colors[["actual"]])
    lab <- d[d$F == Fg[40], ]
    g <- ggplot(d, aes(F, n, color = what)) +
      geom_step(linewidth = 1.3) +
      geom_text(data = lab, aes(label = what), vjust = -0.9, hjust = 0, size = 4.6, show.legend = FALSE) +
      scale_color_manual(values = cols, guide = "none") +
      scale_x_log10(breaks = c(5, 10, 25, 50, 100, 250, 500, 1000, 1500), labels = function(x) num(x, 0)) +
      scale_y_continuous(breaks = seq(0, nshow, 2), limits = c(0, nshow + 1)) +
      labs(x = "Fixed cost per firm (log scale)", y = paste0("Number of firms (shown up to ", nshow, ")")) +
      lab_gg()
    if (input$F >= 5) g <- g + geom_vline(xintercept = input$F, linetype = "dotted", color = lab_colors[["muted"]])
    g
  })
}

shinyApp(ui, server)
