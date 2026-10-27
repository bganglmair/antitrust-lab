# Antitrust Lab: Tacit collusion
library(shiny)
library(bslib)
library(ggplot2)

# Shiny sources R/ automatically (theme.R, collusion.R, cases.R)

app_version <- "0.2 (2026-10-12)"

verdict_box <- function(title, value, ok, note = NULL) {
  value_box(
    title = title, value = value,
    theme = if (isTRUE(ok)) value_box_theme(bg = lab_colors[["pass"]], fg = lab_colors[["pass_fg"]])
            else value_box_theme(bg = lab_colors[["fail"]], fg = lab_colors[["fail_fg"]]),
    if (!is.null(note)) p(note)
  )
}
number_box <- function(title, value, note = NULL, color = lab_colors[["ink"]]) {
  value_box(title = title, value = value,
            theme = value_box_theme(bg = lab_colors[["bg"]], fg = color),
            if (!is.null(note)) p(note, style = paste0("color:", lab_colors[["muted"]])))
}
muted <- function(...) p(..., style = paste0("color:", lab_colors[["muted"]], "; font-size:.9rem;"))
choices_of <- function(presets) setNames(names(presets), sapply(presets, `[[`, "label"))
mm_row <- function(i) {
  div(style = "margin-bottom:.4rem;",
    checkboxInput(paste0("mm_on", i), uiOutput(paste0("mm_name", i), inline = TRUE), TRUE),
    div(class = "d-flex gap-2",
      numericInput(paste0("mm_pp", i), "After cheating", 1, min = 0, step = 1, width = "33%"),
      numericInput(paste0("mm_a", i), "Collusion adds", 2, min = 0.5, step = 1, width = "33%"),
      numericInput(paste0("mm_b", i), "Cheating adds", 2, min = 0.5, step = 1, width = "33%")))
}

# ---- Sidebar ----------------------------------------------------------------
sb <- sidebar(
  width = 330,
  conditionalPanel("input.tab == 't1'",
    selectInput("ic_preset", "Example", choices_of(ic_presets)),
    uiOutput("ic_note"),
    numericInput("ic_pp", "Profit per period after cheating, under competition (\u03c0\u1d56)", 2, min = 0, step = 1),
    numericInput("ic_a", "What collusion adds per period (\u03c0\u1d9c \u2212 \u03c0\u1d56)", 4, min = 0.5, step = 1),
    numericInput("ic_b", "What cheating adds once (\u03c0\u1d48 \u2212 \u03c0\u1d9c)", 4, min = 0.5, step = 1),
    uiOutput("ic_levels"),
    sliderInput("ic_d", "How patient are the firms? Discount factor \u03b4", 0.05, 0.95, 0.6, 0.05, ticks = FALSE),
    muted(em("A profit of 1 next period is worth \u03b4 today. Illustrative numbers."))
  ),
  conditionalPanel("input.tab == 't2'",
    selectInput("fe_preset", "Example", choices_of(fe_presets)),
    uiOutput("fe_note"),
    sliderInput("fe_n", "Number of sellers n", 2, 10, 4, 1, ticks = FALSE),
    sliderInput("fe_r", "Annual interest rate r", 5, 50, 20, 5, post = "%", ticks = FALSE),
    selectInput("fe_chg", "Time between price changes", names(fe_times), "3 months"),
    selectInput("fe_seen", "Time until a rival's price is seen", names(fe_times), "2 years"),
    muted(em("Identical products, price competition. Illustrative numbers."))
  ),
  conditionalPanel("input.tab == 't3'",
    selectInput("mm_preset", "Example", choices_of(mm_presets)),
    uiOutput("mm_note"),
    sliderInput("mm_d", "Discount factor \u03b4", 0.05, 0.95, 0.4, 0.01, ticks = FALSE),
    p(strong("Profit per period in each market"), style = "margin-bottom:.2rem;"),
    muted("For each market, enter the profit per period after cheating, what collusion adds to it per period, and what cheating adds once on top of collusion. Check a market if the same firms meet there."),
    mm_row(1), mm_row(2), mm_row(3), mm_row(4)
  ),
  conditionalPanel("input.tab == 't4'",
    sliderInput("pw_phi", "Probability that demand collapses in a period (\u03c6)", 0, 30, 10, 5, post = "%", ticks = FALSE),
    sliderInput("pw_d", "Discount factor \u03b4", 0.55, 0.95, 0.8, 0.05, ticks = FALSE),
    actionButton("pw_draw", "Draw new demand", class = "btn-outline-primary btn-sm mb-3"),
    muted(em("Two sellers, identical products. Illustrative numbers."))
  ),
  conditionalPanel("input.tab == 't5'",
    muted("Predict first, then check with the tool, then reveal.")
  )
)

# ---- Tabs -------------------------------------------------------------------
tab1 <- nav_panel("1 The incentive constraint", value = "t1",
  card(card_header("What this tab does"),
    card_body(
      p("Firms that meet again and again can keep prices high without an agreement. Each firm holds back because a price cut would end the high prices. ",
        "Cheating pays once. Afterwards the firms compete, and every later period brings less profit than collusion would have."),
      p("The discount factor \u03b4 measures patience, because a profit of 1 next period is worth \u03b4 today. ",
        "Collusion can be sustained if the gain today is no larger than the loss that follows, valued today. ",
        "The critical discount factor \u03b4\u0304 is the patience at which the two are equal. The condition says when collusion is possible, not that it happens."))),
  layout_columns(fill = FALSE, col_widths = c(4, 4, 4),
    uiOutput("ic_box_dbar"), uiOutput("ic_box_verdict"), uiOutput("ic_box_r")),
  card(card_header("Gain from cheating today and the loss that follows, valued today"),
       plotOutput("ic_plot", height = "400px"),
       card_footer("The flat line is the one-time gain from cheating. The curve is the loss in all later periods, valued today. It rises with patience. Where the curve lies above the line, cheating is not profitable. The shaded area marks the discount factors at which collusion can be sustained."))
)

tab2 <- nav_panel("2 What makes collusion easier?", value = "t2",
  card(
    card_body(danish_story),
    accordion(open = FALSE, accordion_panel("What happened? (open after you have made up your mind)", danish_decided)),
    card_footer(tagList("Source: ", danish_source))),
  card(card_header("What this tab does"),
    card_body(
      p("Here n sellers offer identical products and compete on price. Under collusion each earns a share 1/n of the monopoly profit. A seller that cuts its price takes the whole market until its rivals answer. After that, all compete and earn nothing."),
      p("Two things decide how fast rivals answer. Prices can change only so often, and a rival's price cut must first be seen. The slower of the two sets the response time. ",
        "The discount factor over the response time is what a profit of 1, earned one response time from now, is worth today. ",
        "A faster answer works like more patience, because the punishment comes sooner. Collusion can be sustained if the discount factor over the response time is at least 1 \u2212 1/n."),
      p("With a response time of one year, this is tab 1 with a collusive profit of 1/n of the monopoly profit, a profit from cheating equal to the monopoly profit, and zero afterwards. ",
        "Tacit collusion as such is not unlawful. Practices that make rivals answer faster, such as exchanging firm-level prices or promising to match any lower price, can be."))),
  layout_columns(fill = FALSE, col_widths = c(4, 4, 4),
    uiOutput("fe_box_eff"), uiOutput("fe_box_thr"), uiOutput("fe_box_max")),
  card(card_header("The threshold for each number of sellers and the discount factor over the response time"),
       plotOutput("fe_plot", height = "400px"),
       card_footer("Blue dots mark numbers of sellers for which collusion can be sustained, sand dots those for which it cannot. The large dot is your choice."))
)

tab3 <- nav_panel("3 Multi-market contact", value = "t3",
  card(card_header("What this tab does"),
    card_body(
      p("When the same firms meet in several markets, a firm that cheats in one market is punished in all of them. A firm that cheats will then cheat everywhere at once. ",
        "The threshold for all markets together is the sum of the gains from cheating divided by the sum of what is at stake (the gain from cheating plus what collusion adds). ",
        "In general it is not the average of the separate thresholds, because a market with more at stake counts for more."),
      p("Contact helps when the thresholds differ, because the room to spare in a market where collusion is easy covers a market where it is hard. When the thresholds are equal, contact changes nothing. ",
        "It never makes collusion harder, because the firms can still collude only in the markets where it pays."))),
  layout_columns(fill = FALSE, col_widths = c(4, 4, 4),
    uiOutput("mm_box_sep"), uiOutput("mm_box_pool"), uiOutput("mm_box_best")),
  card(card_header("The threshold in each market and in all checked markets together"),
       plotOutput("mm_plot", height = "400px"),
       card_footer("Each bar is a market on its own. A bar is blue if collusion can be sustained in that market alone, and sand if it cannot. The dark line is the threshold for all checked markets together. The dashed line is the discount factor. Collusion can be sustained where a threshold lies at or below the dashed line."))
)

tab4 <- nav_panel("4 Price wars without cheating", value = "t4",
  card(card_header("What this tab does"),
    card_body(
      p("Two sellers keep prices high. In some periods demand collapses, and a seller that sells nothing cannot tell whether demand collapsed or its rival undercut it. ",
        "So both follow a rule. After a period without sales, they compete for a number of periods and then return to the high price."),
      p("The price war must be long enough to deter cheating. The tool picks the shortest such war. In the simulation below, nobody cheats, yet every collapse during collusion starts a price war. ",
        "A price war is therefore not proof that a firm cheated. It can be evidence that a collusive scheme exists.")),
    card_footer(pw_note)),
  layout_columns(fill = FALSE, col_widths = c(4, 4, 4),
    uiOutput("pw_box_T"), uiOutput("pw_box_share"), uiOutput("pw_box_profit")),
  card(card_header("Simulated prices over 40 periods. Nobody cheats"),
       plotOutput("pw_plot", height = "320px"),
       card_footer("Triangles mark collapses of demand during collusion. Each starts a price war in the next period. A new draw gives another sequence of collapses."))
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
    experiment("1", "In tab 1, two firms earn 6 when they collude, 10 once when they cheat, and 2 afterwards. Can collusion be sustained at a discount factor of 0.4?",
      c("Yes", "No"),
      p(strong("No. "), "The critical discount factor is (10 \u2212 6)/(10 \u2212 2) = 0.50. At 0.4 the firms are not patient enough.")),
    experiment("2", "In the same example, the discount factor rises from 0.4 to 0.6. Are the firms more or less patient, and can collusion now be sustained?",
      c("More patient, and yes", "Less patient, and no", "More patient, but still no"),
      p(strong("More patient, and yes. "), "A higher discount factor means that the future counts more. At 0.6 the loss that follows cheating is 0.6/(1 \u2212 0.6) \u00d7 (6 \u2212 2) = 6, more than the gain of 10 \u2212 6 = 4.")),
    experiment("3", "In tab 1, cheating pays 8 once instead of 10. Does collusion become easier or harder?",
      c("Easier", "Harder", "No change"),
      p(strong("Easier. "), "The critical discount factor falls from 0.50 to (8 \u2212 6)/(8 \u2212 2) = 0.33.")),
    experiment("4", "In tab 1, each of three firms earns 8 per period under collusion, 12 once when it cheats, and 3 afterwards. A fourth firm enters, and the profit under collusion falls to 5. What happens?",
      c("Collusion becomes harder", "Collusion becomes easier", "No change"),
      p(strong("Collusion becomes harder. "), "The critical discount factor rises from (12 \u2212 8)/(12 \u2212 3) = 0.44 to (12 \u2212 5)/(12 \u2212 3) = 0.78. Cheating now gains 7 instead of 4, and each later period loses only 2 instead of 5.")),
    experiment("5", "In tab 2, compare the two Danish concrete examples. Why could publishing prices make collusion easier?",
      c("Rivals saw price cuts sooner", "Buyers paid more attention", "Costs rose"),
      p(strong("Rivals saw price cuts sooner. "), "The discount factor over the response time rises from 0.694 to 0.955. Before publication at most three sellers could sustain collusion, and afterwards four can. Publishing prices also helps buyers compare offers, but here the effect on rivals dominated.")),
    experiment("6", "In tab 2, a rival's price is seen within a day. The sellers change prices once a year instead of once a month. Does collusion become easier or harder?",
      c("Harder", "Easier", "No change"),
      p(strong("Harder. "), "A price cut now lasts a year before rivals can answer, so cheating pays for longer. At 20 percent interest the discount factor over the response time falls from 0.985 to 0.833, and the largest number of sellers that can sustain collusion falls from more than 10 to 6.")),
    experiment("7", "In tab 3, two markets that differ have thresholds of 1/2 and 1/4, and together 3/8, the average. Is the threshold for several markets always the average of the separate thresholds?",
      c("Yes", "No"),
      p(strong("No. "), "It is the sum of the gains from cheating divided by the sum of what is at stake. Here both markets have a stake of 4, so it equals the average. If market 2 paid 1 after cheating, 8 under collusion and 9 when cheating, its threshold would be 1/8, and the threshold for both markets 3/12 = 0.25, not the average of 0.31.")),
    experiment("8", "In tab 3, the two markets are identical. Does contact make collusion easier?",
      c("Yes", "No"),
      p(strong("No. "), "Each market has a threshold of 1/2, and so do both together. Contact helps only when the thresholds differ.")),
    experiment("9", "In tab 3, the discount factor is 0.42. The airlines first meet on routes X and Y, and then on all four routes. Can they sustain collusion on route X?",
      c("Not with X and Y alone, but yes with all four routes", "Yes in both cases", "No in both cases"),
      p(strong("Not with X and Y alone, but yes with all four routes. "), "The threshold for X and Y together is 0.44, above 0.42, so collusion can be sustained only on route Y. With all four routes the threshold falls to 0.35, so collusion can be sustained on all of them, including X. No route gained or lost an airline, so a route-by-route count of airlines would miss this.")),
    experiment("10", "In tab 4, price wars broke out several times, but no seller cheated. Why?",
      c("Demand collapsed, and sellers could not tell a collapse from cheating", "A seller cheated without being seen", "Costs fell"),
      p(strong("Demand collapsed, and sellers could not tell a collapse from cheating. "), "The price wars are part of the collusive scheme. They are not proof that a firm cheated, and they can be evidence that the scheme exists."))
  )
)

tab6 <- nav_panel("Math", value = "t6",
  withMathJax(),
  card(card_header("Notation"),
    p("\\(\\pi^c\\) is the profit per period under collusion, \\(\\pi^d\\) the profit in the period of cheating, and \\(\\pi^p\\) the profit per period after cheating. \\(\\delta = 1/(1 + r)\\) is the discount factor and \\(r\\) the interest rate per period (per year in tab 2). \\(\\bar\\delta\\) is the critical discount factor, \\(n\\) the number of sellers, \\(\\Pi^m\\) the monopoly profit, and \\(\\varphi\\) the probability that demand collapses in a period.")),
  card(card_header("The incentive constraint (tab 1)"),
    p("A firm compares cheating with colluding forever. Collusion can be sustained if $$\\pi^d - \\pi^c \\le \\frac{\\delta}{1 - \\delta}\\,(\\pi^c - \\pi^p), \\quad\\text{that is,}\\quad \\delta \\ge \\bar\\delta = \\frac{\\pi^d - \\pi^c}{\\pi^d - \\pi^p}.$$ The tool asks for \\(\\pi^p\\) and the two differences, so \\(\\pi^p < \\pi^c < \\pi^d\\) always holds. If one period is a year, collusion can be sustained at interest rates up to \\(1/\\bar\\delta - 1\\).")),
  card(card_header("What makes collusion easier (tab 2)"),
    p("Profits are a flow, and \\(T\\) is the response time in years. Collusion pays \\(\\Pi^m/n\\) per year forever. A seller that cuts its price earns \\(\\Pi^m\\) per year for \\(T\\) years and nothing afterwards. Collusion can be sustained if $$\\delta^T \\ge 1 - \\frac{1}{n}, \\qquad \\delta = \\frac{1}{1 + r}.$$ The largest number of sellers is the largest \\(n\\) with \\(n \\le 1/(1 - \\delta^T)\\)."),
    p("The tool sets \\(T\\) to the longer of the time between price changes and the time until a rival's price is seen. In general, rivals answer at the first price change after they see the cut, so the true response time lies between the longer of the two and their sum."),
    p("With quantity competition (Cournot) and linear demand, the threshold for \\(\\delta\\) is \\((n + 1)^2/(n^2 + 6n + 1)\\) instead of \\(1 - 1/n\\). Asymmetric capacities are not modeled. Small firms lack the capacity to punish a large firm that cheats, which makes collusion harder.")),
  card(card_header("Multi-market contact (tab 3)"),
    p("A firm that cheats in one market is punished in all markets where it meets the same rivals, so it cheats in all of them at once. For a set of markets indexed by \\(k\\), collusion can be sustained if $$\\delta \\ge \\frac{\\sum_k (\\pi^d_k - \\pi^c_k)}{\\sum_k (\\pi^d_k - \\pi^p_k)}.$$ The threshold for all markets together lies between the lowest and the highest separate threshold. Firms can always choose to collude in only some of the markets, so contact never makes collusion harder (Bernheim and Whinston 1990).")),
  card(card_header("Price wars without cheating (tab 4)"),
    p("Two sellers, identical products. In each period demand collapses with probability \\(\\varphi\\). After a period without sales, both compete for \\(T\\) periods and then return to collusion. With \\(V\\) the value of collusion and \\(\\Pi^m\\) the monopoly profit, $$V = (1 - \\varphi)\\left(\\tfrac{1}{2}\\Pi^m + \\delta V\\right) + \\varphi\\,\\delta^{T+1} V.$$ Cheating gives \\((1 - \\varphi)\\Pi^m + \\delta^{T+1} V\\). Collusion can be sustained if $$\\delta^{T+1}(1 - 2\\varphi) \\le 2(1 - \\varphi)\\delta - 1.$$ No price war is long enough if \\(2(1 - \\varphi)\\delta \\le 1\\). In the long run, a share \\(\\varphi T/(1 + \\varphi T)\\) of periods is spent in price wars, and average profit per period relative to collusion with stable demand is \\((1 - \\varphi)/(1 + \\varphi T)\\). This counts the lost sales in periods of collapse as well as the price wars. The model is the price-competition version of Green and Porter (1984) in Tirole (1988, section 6.7.1.1).")),
  card(card_header("Sources"),
    tags$ul(
      lab_ref("Albæk, Svend, Peter Møllgaard, and Per B. Overgaard. 1997. \"Government-Assisted Oligopoly Coordination? A Concrete Case.\" ", em("Journal of Industrial Economics"), " 45 (4): 429–443.", url = "https://doi.org/10.1111/1467-6451.00057"),
      lab_ref("Bernheim, B. Douglas, and Michael D. Whinston. 1990. \"Multimarket Contact and Collusive Behavior.\" ", em("RAND Journal of Economics"), " 21 (1): 1–26.", url = "https://doi.org/10.2307/2555490"),
      lab_ref("Ellison, Glenn. 1994. \"Theories of Cartel Stability and the Joint Executive Committee.\" ", em("RAND Journal of Economics"), " 25 (1): 37–57.", url = "https://doi.org/10.2307/2555852"),
      lab_ref("Green, Edward J., and Robert H. Porter. 1984. \"Noncooperative Collusion under Imperfect Price Information.\" ", em("Econometrica"), " 52 (1): 87–100.", url = "https://doi.org/10.2307/1911462"),
      lab_ref("Porter, Robert H. 1983. \"A Study of Cartel Stability: The Joint Executive Committee, 1880–1886.\" ", em("Bell Journal of Economics"), " 14 (2): 301–314.", url = "https://doi.org/10.2307/3003634"),
      lab_ref("Tirole, Jean. 1988. ", em("The Theory of Industrial Organization."), " Cambridge, MA: MIT Press.")))
)

ui <- page_navbar(
  id = "tab",
  fillable = FALSE,
  title = lab_title("Tacit collusion"),
  window_title = "Antitrust Lab",
  theme = lab_theme(),
  navbar_options = lab_navbar(),
  sidebar = sb,
  header = div(class = "container-fluid",
    lab_header("When can firms keep prices high without an agreement?", "Topic: tacit collusion", tool = "Tacit collusion")),
  footer = div(class = "container-fluid",
    style = paste0("color:", lab_colors[["muted"]], "; font-size:.85rem; padding:.5rem 0;"),
    paste("Version", app_version, "· Case facts and the figures quoted from the cases are real. All profits, discount factors and response times are illustrative.")),
  tab1, tab2, tab3, tab4, tab5, tab6
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {
  same <- function(a, b) length(a) == length(b) && all(!is.na(a)) && isTRUE(all.equal(as.numeric(a), as.numeric(b)))
  pos <- function(x) length(x) == 1 && is.finite(x) && x > 0
  nonneg <- function(x) length(x) == 1 && is.finite(x) && x >= 0

  # Tab 1 ---------------------------------------------------------------------
  observeEvent(input$ic_preset, {
    s <- ic_presets[[input$ic_preset]]
    updateNumericInput(session, "ic_pp", value = s$pp); updateNumericInput(session, "ic_a", value = s$pc - s$pp)
    updateNumericInput(session, "ic_b", value = s$pd - s$pc); updateSliderInput(session, "ic_d", value = s$d)
  })
  ic <- reactive({
    pp <- input$ic_pp; a <- input$ic_a; b <- input$ic_b
    validate(need(nonneg(pp) && pos(a) && pos(b), "Enter a profit after cheating of zero or more, and amounts above zero for what collusion and cheating add."))
    pc <- pp + a; pd <- pc + b
    list(pc = pc, pd = pd, pp = pp, d = input$ic_d, dbar = ic_dbar(pc, pd, pp))
  })
  output$ic_note <- renderUI({ s <- ic_presets[[input$ic_preset]]
    if (same(c(input$ic_pp, input$ic_a, input$ic_b, input$ic_d), c(s$pp, s$pc - s$pp, s$pd - s$pc, s$d))) muted(s$note) else muted("You changed the numbers of this example.") })
  output$ic_levels <- renderUI({ k <- ic()
    muted(paste0("So πᶜ = ", nz(k$pc), " and πᵈ = ", nz(k$pd), ".")) })
  output$ic_box_dbar <- renderUI({ k <- ic()
    number_box("Critical discount factor δ̄", num(k$dbar, 2),
      paste0("(", nz(k$pd), " − ", nz(k$pc), ")/(", nz(k$pd), " − ", nz(k$pp), ")")) })
  output$ic_box_verdict <- renderUI({ k <- ic(); ok <- ic_sustainable(k$d, k$dbar)
    verdict_box(paste0("Can collusion be sustained at δ = ", num(k$d, 2), "?"), if (ok) "Yes" else "No", ok,
      if (ok) "Cheating is not profitable. The firms are patient enough." else "Cheating is profitable. The firms are not patient enough.") })
  output$ic_box_r <- renderUI({ k <- ic()
    number_box("Highest interest rate at which collusion can be sustained", pct(1 / k$dbar - 1, 0), "If one period is a year") })

  output$ic_plot <- renderPlot({
    k <- ic(); gain <- k$pd - k$pc; stake <- k$pc - k$pp; ymax <- max(24, 3 * gain)
    dd <- data.frame(d = seq(0, 0.99, by = 0.0025)); dd$loss <- ic_loss(dd$d, k$pc, k$pp)
    cur <- data.frame(d = k$d, loss = ic_loss(k$d, k$pc, k$pp))
    lab_y <- function(y) pmin(y, ymax * 0.97)
    xat <- function(y) y / (y + stake)          # where the loss curve reaches height y
    # label of the curve: to its left, clear of the dashed line and of the gain line
    lab <- NULL
    for (f in c(0.75, 0.85, 0.65, 0.55)) { y <- f * ymax; xc <- xat(y)
      if (xc - 0.32 > 0 && !(k$dbar > xc - 0.33 && k$dbar < xc) && abs(y - gain) > 0.1 * ymax) { lab <- list(x = xc - 0.015, y = y, h = 1); break } }
    if (is.null(lab)) { y <- 0.5 * ymax; lab <- list(x = min(xat(y) + 0.02, 0.62), y = y, h = 0) }
    dlab_left <- k$dbar > 0.25
    # label of your delta: below and right of the point, but never on the gain line
    near <- abs(lab_y(cur$loss) - gain) < 0.08 * ymax
    ylab <- if (near) max(min(cur$loss, gain) - 0.07 * ymax, 0.03 * ymax) else lab_y(cur$loss)
    vlab <- if (near) 0.5 else 1.6
    ggplot(dd, aes(d, loss)) +
      annotate("rect", xmin = k$dbar, xmax = Inf, ymin = -Inf, ymax = Inf, fill = lab_colors[["pass"]], alpha = 0.15) +
      geom_hline(yintercept = gain, color = lab_colors[["ink"]], linewidth = 1.3) +
      annotate("text", x = 0.02, y = gain, label = paste("gain from cheating today:", nz(gain)), hjust = 0, vjust = -0.7, color = lab_colors[["ink"]], size = 4.8) +
      geom_line(color = lab_colors[["pass"]], linewidth = 1.3) +
      annotate("text", x = lab$x, y = lab$y, label = "loss that follows, valued today", hjust = lab$h, color = lab_colors[["pass"]], size = 4.8) +
      geom_vline(xintercept = k$dbar, linetype = "dashed", color = lab_colors[["ink"]]) +
      annotate("text", x = k$dbar, y = ymax * 0.97, label = paste0("δ̄ = ", num(k$dbar, 2)), hjust = if (dlab_left) 1.08 else -0.08, color = lab_colors[["ink"]], size = 4.8) +
      geom_point(data = transform(cur, loss = lab_y(loss)), size = 5, color = lab_colors[["actual"]]) +
      annotate("text", x = k$d, y = ylab, label = paste0("your δ = ", num(k$d, 2)), hjust = if (k$d > 0.75) 1.15 else -0.15, vjust = vlab, color = lab_colors[["actual"]], size = 4.6) +
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), expand = expansion(mult = c(0.01, 0))) +
      coord_cartesian(ylim = c(0, ymax)) +
      labs(x = "Discount factor δ (patience)", y = "Profit") + lab_gg()
  })

  # Tab 2 ---------------------------------------------------------------------
  observeEvent(input$fe_preset, {
    s <- fe_presets[[input$fe_preset]]
    updateSliderInput(session, "fe_n", value = s$n); updateSliderInput(session, "fe_r", value = s$r)
    updateSelectInput(session, "fe_chg", selected = s$chg); updateSelectInput(session, "fe_seen", selected = s$seen)
  })
  output$fe_note <- renderUI({ s <- fe_presets[[input$fe_preset]]
    if (same(c(input$fe_n, input$fe_r), c(s$n, s$r)) && identical(input$fe_chg, s$chg) && identical(input$fe_seen, s$seen)) muted(s$note)
    else muted("You changed the settings of this example.") })
  fe <- reactive({
    tc <- fe_times[[input$fe_chg]]; ts <- fe_times[[input$fe_seen]]; T <- max(tc, ts)
    r <- input$fe_r / 100; eff <- fe_eff(r, T)
    list(n = input$fe_n, r = r, T = T, eff = eff, d = 1 / (1 + r), thr = fe_threshold(input$fe_n), maxn = fe_max_n(eff),
         binding = if (abs(tc - ts) < 1e-12) "Both times are equal." else if (ts > tc) "The time until a rival's price is seen sets the response time." else "The time between price changes sets the response time.",
         Tlab = if (ts >= tc) input$fe_seen else input$fe_chg)
  })
  output$fe_box_eff <- renderUI({ k <- fe()
    number_box("Discount factor over the response time", num(k$eff, 3),
      paste0("δ = ", num(k$d, 3), " per year, response time ", k$Tlab, ". ", k$binding), lab_colors[["actual"]]) })
  output$fe_box_thr <- renderUI({ k <- fe(); ok <- k$eff >= k$thr - 1e-9
    verdict_box(paste0("Can ", k$n, " sellers sustain collusion?"), if (ok) "Yes" else "No", ok,
      paste0("Threshold 1 − 1/", k$n, " = ", num(k$thr, 3))) })
  output$fe_box_max <- renderUI({ k <- fe()
    number_box("Largest number of sellers that can sustain collusion",
      if (k$maxn < 2) "None" else if (k$maxn > 10) "More than 10" else as.character(k$maxn),
      if (k$maxn < 2) "Not even two sellers" else if (k$maxn > 10) paste0("Up to ", format(k$maxn, big.mark = ","), ", beyond the chart") else NULL) })

  output$fe_plot <- renderPlot({
    k <- fe(); d <- data.frame(n = 2:10); d$thr <- fe_threshold(d$n); d$ok <- k$eff >= d$thr - 1e-9
    sel <- d[d$n == k$n, ]
    ylo <- 0.25; yeff <- max(k$eff, ylo)
    ggplot(d, aes(n, thr)) +
      geom_line(color = lab_colors[["muted"]], linewidth = 0.6) +
      geom_point(aes(fill = ok), shape = 21, size = 4, color = lab_colors[["ink"]]) +
      geom_point(data = sel, aes(fill = ok), shape = 21, size = 8, color = lab_colors[["ink"]], stroke = 1.2) +
      scale_fill_manual(values = c(`TRUE` = lab_colors[["pass"]], `FALSE` = lab_colors[["fail"]]), guide = "none") +
      geom_hline(yintercept = yeff, color = lab_colors[["actual"]], linewidth = 1.3, linetype = if (k$eff < ylo) "dotted" else "solid") +
      annotate("text", x = if (k$eff < 0.75) 10 else 2, y = yeff, label = paste0("discount factor over the response time: ", num(k$eff, 3), if (k$eff < ylo) " (below the chart)" else ""),
               hjust = if (k$eff < 0.75) 1 else 0, vjust = if (k$eff > 0.93) 1.7 else -0.8, color = lab_colors[["actual"]], size = 4.8) +
      annotate("text", x = 10, y = fe_threshold(10), label = "threshold 1 − 1/n", hjust = 1, vjust = 2.2, color = lab_colors[["muted"]], size = 4.6) +
      scale_x_continuous(breaks = 2:10) + scale_y_continuous(limits = c(ylo, 1), breaks = seq(0.3, 1, 0.1)) +
      labs(x = "Number of sellers n", y = "Threshold and discount factor") + lab_gg()
  })

  # Tab 3 ---------------------------------------------------------------------
  observeEvent(input$mm_preset, {
    s <- mm_presets[[input$mm_preset]]
    for (i in 1:4) {
      updateCheckboxInput(session, paste0("mm_on", i), value = s$on[i])
      updateNumericInput(session, paste0("mm_pp", i), value = s$pp[i])
      updateNumericInput(session, paste0("mm_a", i), value = s$pc[i] - s$pp[i])
      updateNumericInput(session, paste0("mm_b", i), value = s$pd[i] - s$pc[i])
    }
    updateSliderInput(session, "mm_d", value = s$d)
  })
  output$mm_note <- renderUI({ s <- mm_presets[[input$mm_preset]]
    cur <- unlist(lapply(1:4, function(i) c(isTRUE(input[[paste0("mm_on", i)]]), input[[paste0("mm_pp", i)]], input[[paste0("mm_a", i)]], input[[paste0("mm_b", i)]])))
    ref <- unlist(lapply(1:4, function(i) c(s$on[i], s$pp[i], s$pc[i] - s$pp[i], s$pd[i] - s$pc[i])))
    if (same(c(cur, input$mm_d), c(ref, s$d))) muted(s$note) else muted("You changed the numbers of this example.") })
  for (i in 1:4) local({ j <- i; output[[paste0("mm_name", j)]] <- renderUI(mm_presets[[input$mm_preset]]$names[j]) })
  mm <- reactive({
    nm <- mm_presets[[input$mm_preset]]$names
    val <- function(id) sapply(1:4, function(i) { v <- input[[paste0(id, i)]]; if (is.null(v)) NA_real_ else as.numeric(v) })
    d <- data.frame(name = nm, on = sapply(1:4, function(i) isTRUE(input[[paste0("mm_on", i)]])),
                    pp = val("mm_pp"), a = val("mm_a"), b = val("mm_b"))
    d <- d[d$on, ]
    validate(need(nrow(d) > 0, "Check at least one market."),
             need(all(sapply(d$pp, nonneg)) && all(sapply(d$a, pos)) && all(sapply(d$b, pos)),
                  "In each checked market, enter a profit after cheating of zero or more, and amounts above zero for what collusion and cheating add."))
    d$pc <- d$pp + d$a; d$pd <- d$pc + d$b
    d$dbar <- mm_dbar(d$pc, d$pd, d$pp)
    best <- mm_best_set(d$pc, d$pd, d$pp, input$mm_d)
    list(d = d, pooled = mm_pooled(d$pc, d$pd, d$pp), delta = input$mm_d, best = best)
  })
  output$mm_box_sep <- renderUI({ k <- mm()
    number_box("Thresholds of the markets on their own", paste(num(k$d$dbar, 2), collapse = ", "),
      paste0("Separately, all checked markets need δ ≥ ", num(max(k$d$dbar), 2))) })
  output$mm_box_pool <- renderUI({ k <- mm()
    number_box("Threshold for all checked markets together", num(k$pooled, 2),
      paste0("(", paste(nz(k$d$b), collapse = " + "), ")/(", paste(nz(k$d$a + k$d$b), collapse = " + "), ")")) })
  output$mm_box_best <- renderUI({ k <- mm(); s <- k$best$set
    verdict_box(paste0("Where can collusion be sustained at δ = ", num(k$delta, 2), "?"),
      if (length(s) == 0) "Nowhere" else if (length(s) == nrow(k$d)) "In all checked markets" else paste(k$d$name[s], collapse = ", "),
      length(s) > 0,
      if (length(s) > 0) paste0("Threshold for these markets: ", num(k$best$value, 2)) else "Even the easiest market needs more patience") })

  output$mm_plot <- renderPlot({
    k <- mm(); d <- k$d; lv <- paste0(d$name, "\n", num(d$dbar, 2)); d$name <- factor(lv, levels = lv); d$ok <- d$dbar <= k$delta + 1e-9
    gap <- 0.06; yp <- k$pooled; yd <- k$delta
    if (abs(yp - yd) < gap) { mid <- (yp + yd) / 2; up <- if (yp >= yd) "p" else "d"
      yp <- mid + if (up == "p") gap / 2 else -gap / 2; yd <- mid + if (up == "d") gap / 2 else -gap / 2 }
    xr <- nrow(d) + 0.6; xe <- nrow(d) + 0.5
    ggplot(d, aes(name, dbar)) +
      geom_col(aes(fill = ok), width = 0.55, color = lab_colors[["ink"]], linewidth = 0.3) +
      scale_fill_manual(values = c(`TRUE` = lab_colors[["pass"]], `FALSE` = lab_colors[["fail"]]), guide = "none") +
      annotate("segment", x = 0.5, xend = xe, y = k$pooled, yend = k$pooled, color = lab_colors[["ink"]], linewidth = 1.3) +
      annotate("text", x = xr, y = yp, label = paste0("together: ", num(k$pooled, 2)), hjust = 0, color = lab_colors[["ink"]], size = 4.6) +
      annotate("segment", x = 0.5, xend = xe, y = k$delta, yend = k$delta, color = lab_colors[["actual"]], linewidth = 1.2, linetype = "dashed") +
      annotate("text", x = xr, y = yd, label = paste0("δ = ", num(k$delta, 2)), hjust = 0, color = lab_colors[["actual"]], size = 4.6) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), expand = expansion(mult = c(0, 0.02))) +
      scale_x_discrete(expand = expansion(add = c(0.5, 1.4))) +
      labs(x = NULL, y = "Threshold for δ") + lab_gg() +
      theme(axis.text.x = element_text(size = 13, lineheight = 1.1))
  })

  # Tab 4 ---------------------------------------------------------------------
  pw <- reactive({
    phi <- input$pw_phi / 100; d <- input$pw_d; m <- pw_min_T(phi, d)
    list(phi = phi, d = d, T = m$T, state = m$state,
         sim = pw_simulate(phi, if (m$state == "ok") m$T else NA, 40, seed = 1000 + input$pw_draw))
  })
  output$pw_box_T <- renderUI({ k <- pw()
    if (k$state == "ok") verdict_box("Shortest price war that deters cheating", paste(k$T, if (k$T == 1) "period" else "periods"), TRUE, "Collusion can be sustained")
    else verdict_box("Shortest price war that deters cheating", if (k$state == "never") "None works" else "More than 10 periods", FALSE,
      if (k$state == "never") "Even a permanent price war does not deter cheating, so the sellers compete" else "Collusion would need very long price wars") })
  output$pw_box_share <- renderUI({ k <- pw()
    number_box("Share of periods in price wars", if (k$state == "ok") pct(pw_share_war(k$phi, k$T), 0) else "No collusion",
      if (k$state == "ok") "In the long run. One draw of 40 periods will differ" else NULL) })
  output$pw_box_profit <- renderUI({ k <- pw()
    number_box("Average profit per period compared with stable demand", if (k$state == "ok") pct(pw_rel_profit(k$phi, k$T), 0) else "No collusion",
      if (k$state == "ok") "In the long run. Counts the lost sales when demand collapses and the price wars" else NULL) })

  output$pw_plot <- renderPlot({
    k <- pw(); s <- k$sim; ok <- k$state == "ok"
    s$price <- if (ok) ifelse(s$war, 0, 1) else 0
    starts <- if (ok) s[s$collapse & !s$war, ] else s[0, ]
    ggplot(s, aes(t, price)) +
      geom_step(color = lab_colors[["muted"]], linewidth = 0.6, direction = "mid") +
      geom_point(aes(fill = factor(price)), shape = 21, size = 3.4, color = lab_colors[["ink"]], stroke = 0.4) +
      scale_fill_manual(values = c(`1` = lab_colors[["pass"]], `0` = lab_colors[["fail"]]), guide = "none") +
      geom_point(data = starts, aes(t, -0.28), shape = 17, size = 3.5, color = lab_colors[["ink"]], inherit.aes = FALSE) +
      scale_y_continuous(limits = c(-0.35, 1.15), breaks = c(0, 1),
                         labels = c(if (ok) "Price equals cost\n(price war)" else "Price equals cost\n(no collusion)", "Collusive price")) +
      scale_x_continuous(breaks = seq(0, 40, 5)) +
      labs(x = "Period", y = NULL) + lab_gg()
  })
}

shinyApp(ui, server)
