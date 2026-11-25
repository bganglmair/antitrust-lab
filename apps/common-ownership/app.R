# Antitrust Lab: Common ownership calculator
library(shiny)
library(bslib)

# Shiny sources R/ automatically (theme.R, co.R)

app_version <- "0.1 (2026-11-21)"
nf_max <- 6; nk_max <- 6
firm_lab <- function(f) paste("Firm", f)
inv_lab <- function(k) paste("Investor", LETTERS[k])

# Starting values: three firms, three investors (two founders and one fund that holds all three firms)
start_share <- c(40, 35, 25, 0, 0, 0)
start_beta <- matrix(0, nk_max, nf_max)
start_beta[1, 1] <- 30; start_beta[2, 2] <- 25; start_beta[3, 1:3] <- c(10, 10, 15)

number_box <- function(title, value, note = NULL, color = lab_colors[["ink"]]) {
  value_box(title = title, value = value, theme = value_box_theme(bg = lab_colors[["bg"]], fg = color),
            if (!is.null(note)) p(note, style = paste0("color:", lab_colors[["muted"]])))
}
muted <- function(...) p(..., style = paste0("color:", lab_colors[["muted"]], "; font-size:.9rem;"))
cell_in <- function(id, value) numericInput(id, NULL, value, min = 0, max = 100, step = 1, width = "100%")

sb <- sidebar(width = 300,
  conditionalPanel("input.tab == 't1'",
    selectInput("nf", "Number of firms", 3:6, 3),
    selectInput("nk", "Number of investors", 2:6, 3),
    muted("Enter market shares and stakes in percent. Each firm's stakes can add up to less than 100 percent. The rest is held by many small shareholders who do not count for control. An investor with a stake in one firm only, such as a founder or a family, is entered as its own row."),
    actionButton("reset", "Back to the starting values", class = "btn-outline-primary btn-sm")),
  conditionalPanel("input.tab != 't1'", muted("This tab needs no settings.")))

tab1 <- nav_panel("Calculator", value = "t1",
  card(card_header("What this tab does"),
    card_body(
      p("When the same investors own shares in competing firms, each firm has a reason to care about its rivals' profits. The profit weight κ (kappa) of firm f on firm g is the value to firm f of one euro of firm g's profit. It is zero if no investor holds shares in both firms, and one if the two firms have the same owners in the same proportions."),
      p("The calculator assumes one share, one vote, so an investor's say in a firm grows with its stake. It turns the ownership table and the market shares into the profit weights and the modified HHI (MHHI). The MHHI adds to the ordinary HHI a term for the profit weights, the MHHI delta. Without common owners, the MHHI equals the HHI."),
      p(strong("Not every owner is a common owner. "), "A firm's shares are usually spread over three kinds of holders. Common owners hold shares in the firm and in its rivals, as a fund that invests in the whole industry does. Other large owners hold shares in this firm only, such as a founder, a family or a strategic investor. Many small shareholders hold the rest. Only common owners link the firms. Owners of one firm only pull the firm toward its own profit, so the larger their stakes, the smaller the firm's weight on its rivals. Small shareholders have little say and do not count."))),
  card(card_header("Step 1. Market shares (percent)"), uiOutput("share_ui"), card_footer(uiOutput("share_note"))),
  card(card_header("Step 2. Stake of each investor in each firm (percent)"), uiOutput("beta_ui"), card_footer(uiOutput("beta_note"))),
  layout_columns(fill = FALSE, col_widths = c(4, 4, 4), uiOutput("box_hhi"), uiOutput("box_delta"), uiOutput("box_mhhi")),
  card(card_header("Step 3. Profit weights κ"), uiOutput("kappa_ui"),
       card_footer("Read across a row. The number in row f and column g is the weight firm f puts on one euro of firm g's profit. A firm's weight on its own profit is 1. Darker cells are larger weights.")),
  card(card_header("Step 4. How one profit weight is computed"),
    layout_columns(col_widths = c(3, 9),
      div(uiOutput("pair_ui")),
      uiOutput("pair_calc"))),
  card(card_header("Step 5. Where the MHHI delta comes from"), uiOutput("delta_ui"),
       card_footer("Each pair of firms counts twice, once for each direction. The MHHI delta is the sum of the last column."))
)

tab2 <- nav_panel("Math", value = "t2",
  withMathJax(),
  card(card_header("Notation"),
    p("\\(\\beta_{fk}\\) is the stake of investor \\(k\\) in firm \\(f\\), as a fraction. \\(s_f\\) is the market share of firm \\(f\\) in percent. \\(\\kappa_{fg}\\) is the weight firm \\(f\\) puts on the profit of firm \\(g\\).")),
  card(card_header("Profit weights"),
    p("With one share, one vote, the manager of firm \\(f\\) acts for its owners in proportion to their stakes and maximizes \\(\\pi_f + \\sum_{g \\neq f} \\kappa_{fg}\\,\\pi_g\\), with $$\\kappa_{fg} = \\frac{\\sum_k \\beta_{fk}\\,\\beta_{gk}}{\\sum_k \\beta_{fk}^2}.$$ The numerator is large when the investors who own firm \\(f\\) also own firm \\(g\\). The denominator measures how concentrated the ownership of firm \\(f\\) is. Shares held by many small holders count for nothing in either. If no listed investor holds shares in firm \\(f\\), the tool sets all its weights on rivals to zero. A weight can exceed 1 if an investor holds a larger stake in the rival than in the firm itself.")),
  card(card_header("Modified HHI"),
    p("$$\\text{MHHI} = \\underbrace{\\sum_f s_f^2}_{\\text{HHI}} + \\underbrace{\\sum_f \\sum_{g \\neq f} \\kappa_{fg}\\,s_f\\,s_g}_{\\text{MHHI delta}}.$$ Market shares that do not add up to 100 are scaled so that they do. In Cournot competition the MHHI takes the place of the HHI, and the industry margin equals \\(\\text{MHHI}/(10{,}000 \\times |\\eta|)\\), where \\(\\eta\\) is the market elasticity of demand (Bresnahan and Salop 1986, O'Brien and Salop 2000).")),
  card(card_header("Limits"),
    p("The profit weights measure incentives, not conduct. Whether managers act on them, and how much control a passive fund has, is an open question. With control rights that differ from stakes (for example, funds that do not vote), the weights change.")),
  card(card_header("Sources"),
    tags$ul(
      lab_ref("Bresnahan, Timothy F., and Steven C. Salop. 1986. \"Quantifying the Competitive Effects of Production Joint Ventures.\" ", em("International Journal of Industrial Organization"), " 4 (2): 155–175.", url = "https://doi.org/10.1016/0167-7187(86)90028-7"),
      lab_ref("O'Brien, Daniel P., and Steven C. Salop. 2000. \"Competitive Effects of Partial Ownership: Financial Interest and Corporate Control.\" ", em("Antitrust Law Journal"), " 67 (3): 559–614."),
      lab_ref("Azar, José, Martin C. Schmalz, and Isabel Tecu. 2018. \"Anticompetitive Effects of Common Ownership.\" ", em("Journal of Finance"), " 73 (4): 1513–1565.", url = "https://doi.org/10.1111/jofi.12698")))
)

ui <- page_navbar(
  id = "tab", fillable = FALSE,
  title = lab_title("Common ownership"), window_title = "Antitrust Lab",
  theme = lab_theme(), navbar_options = lab_navbar(), sidebar = sb,
  header = div(class = "container-fluid",
    lab_header("How much do commonly owned firms care about each other's profits?", "Topic: common ownership", tool = "Common ownership")),
  footer = div(class = "container-fluid", style = paste0("color:", lab_colors[["muted"]], "; font-size:.85rem; padding:.5rem 0;"),
    paste("Version", app_version, "· All numbers are entered by the user.")),
  tab1, tab2
)

server <- function(input, output, session) {
  nf <- reactive(as.integer(input$nf)); nk <- reactive(as.integer(input$nk))
  # keep entered values when the grid grows or shrinks
  val <- function(id, default) { v <- isolate(input[[id]]); if (is.null(v)) default else v }
  grid_style <- function(n) paste0("display:grid; grid-template-columns: 9rem repeat(", n, ", minmax(4.5rem, 1fr)); gap:.4rem; align-items:center;")
  output$share_ui <- renderUI({ n <- nf()
    div(style = grid_style(n),
      div(), lapply(seq_len(n), function(f) strong(firm_lab(f))),
      div("Market share"), lapply(seq_len(n), function(f) cell_in(paste0("s", f), val(paste0("s", f), start_share[f])))) })
  output$beta_ui <- renderUI({ n <- nf(); m <- nk()
    div(style = grid_style(n),
      div(), lapply(seq_len(n), function(f) strong(firm_lab(f))),
      unlist(lapply(seq_len(m), function(k) c(list(div(inv_lab(k))),
        lapply(seq_len(n), function(f) cell_in(paste0("b", k, "_", f), val(paste0("b", k, "_", f), start_beta[k, f]))))), recursive = FALSE),
      div(em("Total listed")), lapply(seq_len(n), function(f) uiOutput(paste0("colsum", f))),
      div(em("Held by common owners")), lapply(seq_len(n), function(f) uiOutput(paste0("comsum", f)))) })
  observeEvent(input$reset, {
    updateSelectInput(session, "nf", selected = 3); updateSelectInput(session, "nk", selected = 3)
    for (f in seq_len(nf_max)) updateNumericInput(session, paste0("s", f), value = start_share[f])
    for (k in seq_len(nk_max)) for (f in seq_len(nf_max)) updateNumericInput(session, paste0("b", k, "_", f), value = start_beta[k, f])
  })

  read_num <- function(id) { v <- input[[id]]; if (is.null(v) || !is.finite(v)) NA_real_ else v }
  shares <- reactive({ s <- sapply(seq_len(nf()), function(f) read_num(paste0("s", f)))
    validate(need(all(!is.na(s)) && all(s >= 0) && sum(s) > 0, "Enter a market share of zero or more for each firm, with at least one above zero."))
    s / sum(s) })
  beta <- reactive({ n <- nf(); m <- nk()
    b <- matrix(sapply(seq_len(n), function(f) sapply(seq_len(m), function(k) read_num(paste0("b", k, "_", f)))), nrow = m)
    validate(need(all(!is.na(b)) && all(b >= 0), "Enter a stake of zero or more in every cell."))
    over <- which(colSums(b) > 100 + 1e-9)
    validate(need(length(over) == 0, paste0("The stakes in ", paste(firm_lab(over), collapse = " and "), " add up to more than 100 percent. Lower them.")))
    b / 100 })
  K <- reactive(co_kappa(beta()))
  for (f in seq_len(nf_max)) local({ g <- f; output[[paste0("comsum", g)]] <- renderUI({ req(g <= nf())
    b <- sapply(seq_len(nf()), function(h) sapply(seq_len(nk()), function(k) { x <- read_num(paste0("b", k, "_", h)); if (is.na(x)) 0 else x }))
    b <- matrix(b, nrow = nk()); common <- rowSums(b > 0) >= 2
    span(paste0(num(sum(b[common, g]), 0), "%"), style = paste0("color:", lab_colors[["muted"]], ";")) }) })
  for (f in seq_len(nf_max)) local({ g <- f; output[[paste0("colsum", g)]] <- renderUI({ req(g <= nf())
    v <- sum(sapply(seq_len(nk()), function(k) { x <- read_num(paste0("b", k, "_", g)); if (is.na(x)) 0 else x }))
    span(paste0(num(v, 0), "%"), style = if (v > 100) "color:#B3262B; font-weight:600;" else paste0("color:", lab_colors[["muted"]], ";")) }) })

  output$share_note <- renderUI({ s <- sapply(seq_len(nf()), function(f) read_num(paste0("s", f))); tot <- sum(s, na.rm = TRUE)
    muted(if (abs(tot - 100) < 1e-6) "The shares add up to 100 percent." else paste0("The shares add up to ", num(tot, 1), " percent. The tool scales them to 100.")) })
  output$beta_note <- renderUI({ b <- tryCatch(beta(), error = function(e) NULL); if (is.null(b)) return(NULL)
    none <- which(colSums(b) == 0)
    muted(if (length(none)) paste0(paste(firm_lab(none), collapse = " and "), if (length(none) == 1) " has no listed investor, so its weights on rivals are zero." else " have no listed investor, so their weights on rivals are zero.")
          else "Each column shows who owns a firm. An investor with stakes in two or more firms is a common owner. An investor with a stake in one firm only, such as a founder, is not. The rest of each firm is held by many small shareholders.") })

  output$box_hhi <- renderUI(number_box("HHI", num(co_hhi(shares()), 0), "From market shares alone"))
  output$box_delta <- renderUI(number_box("MHHI delta", num(co_delta(K(), shares()), 0), "Added by the profit weights", lab_colors[["actual"]]))
  output$box_mhhi <- renderUI({ v <- co_hhi(shares()) + co_delta(K(), shares())
    value_box(title = "MHHI", value = num(v, 0), theme = value_box_theme(bg = lab_colors[["pass"]], fg = lab_colors[["pass_fg"]]),
              p("HHI plus MHHI delta")) })

  output$kappa_ui <- renderUI({ k <- K(); n <- nf()
    cell <- function(f, g) { v <- k[f, g]; a <- if (f == g) 0 else min(1, v)
      bg <- if (f == g) lab_colors[["grid"]] else paste0("rgba(15, 84, 153, ", num(0.08 + 0.72 * a, 2), ")")
      fg <- if (f != g && a > 0.55) "#ffffff" else lab_colors[["ink"]]
      tags$td(style = paste0("text-align:center; background:", bg, "; color:", fg, ";"), num(v, 3)) }
    tags$table(class = "table table-sm", style = "max-width: 900px;",
      tags$thead(tags$tr(tags$th("Weight of ↓ on →"), lapply(seq_len(n), function(g) tags$th(style = "text-align:center;", firm_lab(g))))),
      tags$tbody(lapply(seq_len(n), function(f) tags$tr(tags$th(firm_lab(f)), lapply(seq_len(n), function(g) cell(f, g)))))) })

  output$pair_ui <- renderUI({ n <- nf(); ch <- setNames(seq_len(n), firm_lab(seq_len(n)))
    f <- min(as.integer(val("pf", 1)), n); g <- as.integer(val("pg", 2)); if (g > n || g == f) g <- setdiff(seq_len(n), f)[1]
    tagList(selectInput("pf", "Weight of", ch, f), selectInput("pg", "on the profit of", ch[-f], g)) })
  observeEvent(input$pf, { n <- nf(); f <- as.integer(input$pf); ch <- setNames(seq_len(n), firm_lab(seq_len(n)))[-f]
    g <- if (!is.null(input$pg) && as.integer(input$pg) != f && as.integer(input$pg) <= n) input$pg else ch[1]
    updateSelectInput(session, "pg", choices = ch, selected = g) }, ignoreInit = TRUE)
  output$pair_calc <- renderUI({ req(input$pf, input$pg); f <- as.integer(input$pf); g <- as.integer(input$pg); req(f != g, f <= nf(), g <= nf())
    b <- beta(); m <- nrow(b); pc <- function(x) num(x, 2)
    top <- paste(sapply(seq_len(m), function(k) paste0(pc(b[k, f]), " × ", pc(b[k, g]))), collapse = " + ")
    bot <- paste(sapply(seq_len(m), function(k) paste0(pc(b[k, f]), " × ", pc(b[k, f]))), collapse = " + ")
    nu <- sum(b[, f] * b[, g]); de <- sum(b[, f]^2)
    tagList(
      p(paste0("Stakes as fractions, one term per investor (", paste(inv_lab(seq_len(m)), collapse = ", "), ").")),
      p(strong("Shared ownership. "), top, " = ", num(nu, 4)),
      muted("Only investors with stakes in both firms add to this sum."),
      p(strong(paste0("Ownership of ", firm_lab(f), " with itself. ")), bot, " = ", num(de, 4)),
      muted(paste0("Every listed owner of ", firm_lab(f), " adds to this sum, including owners of ", firm_lab(f), " alone. A large owner of ", firm_lab(f), " alone makes this sum large and the profit weight small.")),
      p(strong("Profit weight. "), if (de > 0) paste0("κ = ", num(nu, 4), " / ", num(de, 4), " = ", num(nu / de, 3)) else "No listed investor holds shares in this firm, so κ = 0."),
      muted(paste0(firm_lab(f), " values one euro of ", firm_lab(g), "'s profit at ", num(100 * K()[f, g], 1), " cents."))) })

  output$delta_ui <- renderUI({ k <- K(); s <- 100 * shares(); n <- nf()
    rows <- do.call(rbind, lapply(seq_len(n), function(f) do.call(rbind, lapply(setdiff(seq_len(n), f), function(g)
      data.frame(f = f, g = g, k = k[f, g], sf = s[f], sg = s[g], c = k[f, g] * s[f] * s[g])))))
    rows <- rows[rows$k > 0, ]
    if (!nrow(rows)) return(muted("No firm puts a weight on a rival's profit, so the MHHI equals the HHI."))
    tags$table(class = "table table-sm", style = "max-width: 900px;",
      tags$thead(tags$tr(tags$th("Firm f"), tags$th("Rival g"), tags$th("κ of f on g"), tags$th("Share of f"), tags$th("Share of g"), tags$th("Contribution"))),
      tags$tbody(lapply(seq_len(nrow(rows)), function(i) with(rows[i, ], tags$tr(tags$td(firm_lab(f)), tags$td(firm_lab(g)), tags$td(num(k, 3)),
        tags$td(num(sf, 1)), tags$td(num(sg, 1)), tags$td(num(c, 0))))),
        tags$tr(tags$td(strong("MHHI delta"), colspan = 5), tags$td(strong(num(sum(rows$c), 0)))))) })
}

shinyApp(ui, server)
