# Antitrust Lab: Concentration and market power
library(shiny)
library(bslib)
library(ggplot2)

# Shiny sources R/ automatically (theme.R, conc.R, cases.R)

app_version <- "0.8 (2026-10-02)"

# Blue: a competition concern is indicated. Sand: no concern indicated.
verdict_box <- function(title, value, concern, note = NULL) {
  value_box(
    title = title, value = value,
    theme = if (isTRUE(concern)) value_box_theme(bg = lab_colors[["pass"]], fg = lab_colors[["pass_fg"]])
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

pct_slider <- function(id, label, value, min = 0, max = 100, step = 0.1) {
  sliderInput(id, label, min = min, max = max, value = value, step = step, post = "%", ticks = FALSE)
}

fmt_hhi <- function(x) formatC(round_half_up(x), format = "d", big.mark = ",")

# ---- Sidebar (one sidebar, contents follow the active tab) -------------------
mk <- market_cases$heinz
sb <- sidebar(
  width = 330,
  conditionalPanel("input.tab == 't1' || input.tab == 't2'",
    selectInput("mk_case", "Case", setNames(names(market_cases), sapply(market_cases, `[[`, "label"))),
    p(strong("Market shares")),
    pct_slider("s1", mk$names[1], mk$shares[1]),
    pct_slider("s2", mk$names[2], mk$shares[2]),
    pct_slider("s3", mk$names[3], mk$shares[3]),
    pct_slider("s4", mk$names[4], mk$shares[4]),
    uiOutput("others_ui"),
    numericInput("n_oth", "The rest is split equally among this many other firms", mk$n_oth, min = 1, max = 30, step = 1)
  ),
  conditionalPanel("input.tab == 't1'",
    tags$hr(),
    sliderInput("eta1", "Market elasticity of demand |η| (illustrative)", 0.5, 5, mk$eta, 0.1, ticks = FALSE)
  ),
  conditionalPanel("input.tab == 't2'",
    tags$hr(),
    selectInput("mA", "Merging firm A", setNames(1:4, mk$names), mk$mergeA),
    selectInput("mB", "Merging firm B", setNames(1:4, mk$names), mk$mergeB)
  ),
  conditionalPanel("input.tab == 't3'",
    sliderInput("n3", "Number of equal-sized firms n", 1, 10, 2, 1, ticks = FALSE),
    sliderInput("eta3", "Market elasticity of demand |η|", 1.2, 5, 2, 0.1, ticks = FALSE),
    muted(em("Illustrative numbers."))
  ),
  conditionalPanel("input.tab == 't4'",
    selectInput("g_case", "Case", setNames(names(guppi_cases), sapply(guppi_cases, `[[`, "label"))),
    pct_slider("gsA", "Share of A", guppi_cases$close$sA, max = 60, step = 0.1),
    pct_slider("gsB", "Share of B", guppi_cases$close$sB, max = 60, step = 0.1),
    pct_slider("gDAB", "Diversion from A to B", guppi_cases$close$DAB, max = 95, step = 1),
    pct_slider("gDBA", "Diversion from B to A", guppi_cases$close$DBA, max = 95, step = 1),
    pct_slider("gmA", "Margin of A, (p − c)/p", guppi_cases$close$mA, max = 95, step = 1),
    pct_slider("gmB", "Margin of B, (p − c)/p", guppi_cases$close$mB, max = 95, step = 1),
    muted(em("Both products sell at the same price. Rivals keep their relative sizes when you change A's or B's share."))
  ),
  conditionalPanel("input.tab == 't5'",
    muted("Predict first, then check with the tool, then reveal.")
  )
)

case_card <- function(story_id, decided_id, source_id) {
  card(
    card_body(uiOutput(story_id)),
    uiOutput(paste0(decided_id, "_wrap")),
    card_footer(uiOutput(source_id))
  )
}

# ---- Tabs -------------------------------------------------------------------
tab1 <- nav_panel("1 Shares and markups", value = "t1",
  case_card("mk_story", "mk_decided", "mk_source"),
  uiOutput("t1_guard"),
  layout_columns(fill = FALSE, col_widths = c(4, 4, 4),
    uiOutput("t1_box_top"), uiOutput("t1_box_ind"), uiOutput("t1_box_dom")),
  card(card_header("Each firm's Lerner index in Cournot competition (share divided by |η|)"),
       plotOutput("t1_plot", height = "360px"),
       card_footer("The formula uses the market share to calculate the markup. Each firm's Lerner index is its share divided by |η|. In the Cournot model, a firm with a lower cost sells more, so its lower cost gives it both a larger share and a larger markup. A firm that gains share without a cost advantage, for example by buying a rival, is a different case (tab 2). The elasticity is illustrative. For a real case, the numbers show what the Cournot formula gives for an identical product and quantity competition. They are not estimates of actual margins, because baby food, for example, is branded.")),
  accordion(open = FALSE, accordion_panel("Show the numbers", tableOutput("t1_table")))
)

tab2 <- nav_panel("2 HHI and the merger screen", value = "t2",
  case_card("mk_story2", "mk_decided2", "mk_source2"),
  layout_columns(fill = FALSE, col_widths = c(3, 3, 3, 3),
    uiOutput("t2_pre"), uiOutput("t2_post"), uiOutput("t2_delta"), uiOutput("t2_cr4")),
  layout_columns(fill = FALSE, col_widths = c(6, 6),
    uiOutput("t2_us"), uiOutput("t2_eu")),
  card(card_header("The merger on the screening map: HHI after the merger and its change"),
       plotOutput("t2_plot", height = "380px"),
       card_footer("The dot is this merger in both panels. Sand marks the area where the screen raises no concern, and blue marks the area where it does. In the United States, harm is presumed in the blue area. In the European Union, the sand area is the safe harbor, and a merger in the blue area is assessed on the merits without a presumption. The tests based on the combined share are not drawn. The vertical axis is fixed and ends at 1,000, and a merger with a larger change is drawn as a triangle at the top edge.")),
  muted("A blue tile means that the screen does not clear the merger. In the US, harm is then presumed. In the EU, the merger is outside the levels where concerns are unlikely, so the Commission looks closer, but nothing is presumed. A sand tile means that the screen raises no flag. The EU levels come from the Horizontal Merger Guidelines (2004). They have exceptions and create no presumption either way, and revised guidelines are expected by the end of 2026. The US levels come from the Merger Guidelines (2023).")
)

tab3 <- nav_panel("3 Is concentration market power?", value = "t3",
  card(card_body(
    p("The same market structure can produce very different prices. Compare three kinds of conduct for n equal-sized firms selling the same product."),
    p("HHI and market power can also diverge because of entry barriers, coordination, or buyer power. For differentiated products, see tab 4."))),
  layout_columns(fill = FALSE, col_widths = c(3, 3, 3, 3),
    uiOutput("t3_hhi"), uiOutput("t3_c"), uiOutput("t3_b"), uiOutput("t3_k")),
  card(card_header("Lerner index against HHI for three kinds of conduct"),
       plotOutput("t3_plot", height = "400px"),
       card_footer("Full coordination is an upper bound, and it gets harder to sustain with more firms."))
)

tab4 <- nav_panel("4 Diversion and GUPPI", value = "t4",
  uiOutput("t4_note"),
  layout_columns(fill = FALSE, col_widths = c(3, 3, 3, 3),
    uiOutput("t4_us"), uiOutput("t4_eu"), uiOutput("t4_gA"), uiOutput("t4_gB")),
  card(card_header("Upward pricing pressure on each product after the merger (GUPPI)"),
       plotOutput("t4_plot", height = "330px"),
       card_footer("GUPPI measures pricing pressure rather than the predicted price increase, because the effect also depends on pass-through, efficiencies and rivals' responses. No guideline sets a GUPPI threshold. Commissioner Wright proposed 5% as a safe harbor (Wright 2015), and the FTC majority rejected any GUPPI safe harbor (Federal Trade Commission 2015).")),
  uiOutput("t4_bench")
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
    experiment("1", "In Heinz/Beech-Nut (tab 2), is the merger of the number 2 and the number 3 caught by the US presumption?",
      c("Yes", "No, together they are still much smaller than Gerber", "Only under the share route"),
      p(strong("Yes. "), "After the merger the HHI is about 5,306, far above 1,800, and it rises by about 536, far above 100. The combined share (32.8%) also exceeds 30%. Being smaller than the leader does not help, because merging the second and third firm leaves Gerber facing one significant rival instead of two.")),
    experiment("2", "In tab 1, demand becomes twice as elastic for the same observed shares. What happens to the markups?",
      c("They halve", "They stay the same because shares did not change", "They double"),
      p(strong("They halve. "), "Each firm's Lerner index is its share divided by |η|. The HHI is unchanged, but the market power behind it is half as large. The same HHI can mean very different markups.")),
    experiment("3", "In tab 3, two firms sell an identical product at the same cost and compete on price. The HHI is 5,000. What is the markup?",
      c("Zero", "About 25% with |η| = 2", "The monopoly markup"),
      p(strong("Zero. "), "Each firm undercuts the other until price equals cost (Bertrand). With quantity competition (Cournot), the same HHI gives 25% at |η| = 2, and with full coordination it gives 50%. The HHI alone does not tell us which.")),
    experiment("4", "In tab 4, choose close substitutes with small shares. Does the HHI screen catch this merger?",
      c("Yes", "No"),
      p(strong("No. "), "The HHI rises from 1,006 to 1,166, so there is no US presumption and the merger is inside the EU safe harbor. Yet 40–45% of each brand's lost customers would go to the other, far more than the 9–11% their shares suggest. GUPPI is 16–18%.")),
    experiment("5", "In tab 4, choose large shares and distant substitutes. The HHI triggers the US presumption. Must prices rise?",
      c("Yes, the presumption decides", "Not necessarily"),
      p(strong("Not necessarily. "), "GUPPI is low (2.5% and 3.6%), so unilateral pricing pressure is weak. But the presumption still stands unless the parties rebut it with evidence, and with HHI at 2,500 the risk of coordination remains.")),
    experiment("6", "In Heinz/Beech-Nut, the two brands were rarely in the same store. Did they compete?",
      c("No, shoppers could not choose between them", "Yes, for something other than shoppers"),
      p(strong("Yes. "), "They competed for the second position on retailers' shelves, because most stores carried Gerber plus one other brand. Retailers chose between Heinz and Beech-Nut, and that competition gave them lower prices.")),
    experiment("7", "In tab 4 with the Heinz preset, shoppers' diversion between the two brands is low. What does the low GUPPI miss?",
      c("Nothing, because low diversion means no harm", "Competition for the shelf"),
      p(strong("Competition for the shelf. "), "GUPPI built on shoppers' switching sees little pressure, because few shoppers could choose between the brands. The harm is at the wholesale level, where retailers lose a bidder for their second shelf slot. Measure diversion where the competition takes place."))
  )
)

tab6 <- nav_panel("Math", value = "t6",
  withMathJax(),
  card(card_header("Notation"),
    p("\\(s_i\\) is the market share of firm \\(i\\) (a fraction in formulas, percent on screen). \\(\\eta < 0\\) is the market elasticity of demand, and formulas use \\(|\\eta|\\). \\(n\\) is the number of firms, \\(p\\) the price, and \\(c\\) the marginal cost. \\(L_i = (p_i - c_i)/p_i\\) is the Lerner index and \\(m = (p - c)/p\\) the margin. A and B are the merging firms or their products, and \\(D_{AB}\\) is the share of A's lost sales that go to B.")),
  card(card_header("Shares and markups (Cournot)"),
    p("With quantity competition and constant marginal costs (which may differ across firms), each firm's Lerner index is $$L_i = \\frac{p - c_i}{p} = \\frac{s_i}{|\\eta|}$$ (identical product, one market price \\(p\\)). Weighting by shares gives the industry Lerner index $$\\bar L = \\sum_i s_i L_i = \\frac{\\sum_i s_i^2}{|\\eta|} = \\frac{\\text{HHI}}{10{,}000 \\cdot |\\eta|}$$ (Cowling and Waterson 1976)."),
    p("The firm's own elasticity is \\(|\\eta_i| = |\\eta|/s_i\\), so a monopolist (\\(s_i = 1\\)) has \\(L = 1/|\\eta|\\). Shares and elasticity are consistent with Cournot (positive costs) only if \\(|\\eta|\\) exceeds every firm's share written as a fraction (65% is 0.65). The tool holds observed shares fixed when you change \\(|\\eta|\\).")),
  card(card_header("Concentration"),
    p("$$\\text{HHI} = 10{,}000 \\cdot \\sum_i s_i^2, \\qquad \\Delta\\text{HHI} = 10{,}000 \\cdot 2\\, s_A s_B, \\qquad \\text{CR}_4 = 100 \\cdot (s_{(1)} + s_{(2)} + s_{(3)} + s_{(4)}),$$ where \\(s_{(1)} \\ge s_{(2)} \\ge \\dots\\) are the shares sorted from largest to smallest. Thresholds are applied to the rounded HHI values shown on screen and to the combined share rounded to 0.01 percentage points. The three HHI tiles are rounded separately, so they can differ by one point from an exact sum. The change in HHI assumes the merged firm keeps the sum of the two shares."),
    p("Under the US Merger Guidelines (2023), harm is presumed if the HHI after the merger exceeds 1,800 and rises by more than 100, or if the merged firm's share exceeds 30% and the HHI rises by more than 100."),
    p("Under the EU Horizontal Merger Guidelines (2004), concerns are unlikely if the combined share is 25% or less, the HHI after the merger is below 1,000, between 1,000 and 2,000 with a change below 250, or above 2,000 with a change below 150. The HHI levels have exceptions (para 20) and create no presumption of either concerns or their absence (para 21)."),
    p("For dominance under EU law (Art. 102), a very large share over a sustained period is evidence of dominance, in particular a share of 50% or more (AKZO, C-62/86, para 60). Below 40%, dominance is generally unlikely (Commission Guidelines on exclusionary abuses, 2026, para 24). Being dominant is not itself unlawful, but abuse is.")),
  card(card_header("Conduct (tab 3)"),
    p("With \\(n\\) equal firms, Cournot competition gives \\(L = 1/(n|\\eta|)\\), Bertrand competition with identical products and equal costs gives \\(L = 0\\) for \\(n \\ge 2\\), and full coordination gives \\(L = 1/|\\eta|\\). The comparison uses the same \\(|\\eta|\\) for all three, which is exact with constant-elasticity demand. Otherwise \\(|\\eta|\\) is measured at each outcome's own price.")),
  card(card_header("Diversion and GUPPI (tab 4)"),
    p("$$\\text{GUPPI}_A = D_{AB} \\cdot \\frac{p_B - c_B}{p_A},$$ which with equal prices is \\(D_{AB} \\cdot m_B\\). Diversion in proportion to shares, with no customer leaving the market, \\(D_{AB} = s_B/(1 - s_A)\\), is the benchmark under which shares, and hence the HHI, measure how closely two firms compete. \\(D_{BA}\\) and \\(\\text{GUPPI}_B\\) are defined the same way with A and B swapped."),
    p("No guideline sets a GUPPI threshold. The US Horizontal Merger Guidelines (2010, section 6.1) introduced the value of diverted sales. GUPPI goes back to Salop and Moresi (2009) and Farrell and Shapiro (2010). Commissioner Wright proposed 5% as a safe harbor (Wright 2015), and the Commission majority rejected any GUPPI safe harbor (Federal Trade Commission 2015).")),
  card(card_header("Sources"),
    tags$ul(
      lab_ref("Case C-62/86, ", em("AKZO Chemie BV v Commission"), ", ECLI:EU:C:1991:286.", url = "https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=ecli:ECLI:EU:C:1991:286"),
      lab_ref("Cowling, Keith, and Michael Waterson. 1976. \"Price-Cost Margins and Market Structure.\" ", em("Economica"), " 43 (171): 267\u201374."),
      lab_ref("European Commission. 2004. \"Guidelines on the Assessment of Horizontal Mergers under the Council Regulation on the Control of Concentrations between Undertakings.\" ", em("Official Journal of the European Union"), " C 31, February 5: 5\u201318.", url = "https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:52004XC0205(02)"),
      lab_ref("European Commission. 2026a. \"Guidelines on the Application of Article 102 TFEU to Abusive Exclusionary Conduct by Dominant Undertakings.\" C(2026) 6118 final, September 3.", url = "https://competition-policy.ec.europa.eu/antitrust-and-cartels/legislation/application-article-102-tfeu_en"),
      lab_ref("European Commission. 2026b. Review of the Merger Guidelines. Draft revised guidelines published for consultation, April 30.", url = "https://competition-policy.ec.europa.eu/mergers/review-merger-guidelines_en"),
      lab_ref("Farrell, Joseph, and Carl Shapiro. 2010. \"Antitrust Evaluation of Horizontal Mergers: An Economic Alternative to Market Definition.\" ", em("B.E. Journal of Theoretical Economics"), " 10 (1): Article 9.", url = "https://doi.org/10.2202/1935-1704.1563"),
      lab_ref("Federal Trade Commission. 2015. \"Statement of the Federal Trade Commission in the Matter of Dollar Tree, Inc. and Family Dollar Stores, Inc.\" FTC File No. 141-0207, July 13.", url = "https://www.ftc.gov/system/files/documents/public_statements/681901/150714dollarstoresstatement.pdf"),
      lab_ref(em("Federal Trade Commission v. H.J. Heinz Co."), ", 246 F.3d 708 (D.C. Cir. 2001).", url = "https://law.justia.com/cases/federal/appellate-courts/F3/246/708/469087/"),
      lab_ref("Salop, Steven C., and Serge Moresi. 2009. \"Updating the Merger Guidelines: Comments.\" Comment submitted to the Horizontal Merger Guidelines Review Project, Federal Trade Commission, November 9.", url = "https://scholarship.law.georgetown.edu/facpub/1662/"),
      lab_ref("U.S. Department of Justice and Federal Trade Commission. 2010. \"Horizontal Merger Guidelines.\" August 19.", url = "https://www.justice.gov/atr/horizontal-merger-guidelines-08192010"),
      lab_ref("U.S. Department of Justice and Federal Trade Commission. 2023. \"Merger Guidelines.\" December 18.", url = "https://www.justice.gov/d9/2023-12/2023%20Merger%20Guidelines.pdf"),
      lab_ref("Wright, Joshua D. 2015. \"Statement of Commissioner Joshua D. Wright, Dissenting in Part and Concurring in Part, in the Matter of Dollar Tree, Inc. and Family Dollar Stores, Inc.\" Federal Trade Commission, July 13.", url = "https://www.ftc.gov/system/files/documents/public_statements/681781/150713dollartree-jdwstmt.pdf")))
)

ui <- page_navbar(
  id = "tab",
  fillable = FALSE,
  title = lab_title("Concentration"),
  window_title = "Antitrust Lab",
  theme = lab_theme(),
  navbar_options = lab_navbar(),
  sidebar = sb,
  header = div(class = "container-fluid",
    lab_header("How much market power do concentrated markets have?", "Topic: measuring market power", tool = "Concentration and market power")),
  footer = div(class = "container-fluid",
    style = paste0("color:", lab_colors[["muted"]], "; font-size:.85rem; padding:.5rem 0;"),
    paste("Version", app_version, "· Case facts are real. Elasticities, margins and diversion ratios are illustrative unless stated otherwise.")),
  tab1, tab2, tab3, tab4, tab5, tab6
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {

  # Market (tabs 1 and 2) -----------------------------------------------------
  mcase <- reactive(market_cases[[input$mk_case]])

  observeEvent(input$mk_case, {
    cs <- mcase()
    for (k in 1:4) updateSliderInput(session, paste0("s", k), label = cs$names[k], value = cs$shares[k])
    updateNumericInput(session, "n_oth", value = cs$n_oth)
    updateSliderInput(session, "eta1", value = cs$eta)
    updateSelectInput(session, "mA", choices = setNames(1:4, cs$names), selected = cs$mergeA)
    updateSelectInput(session, "mB", choices = setNames(1:4, cs$names), selected = cs$mergeB)
  }, ignoreInit = TRUE)

  named <- reactive(c(input$s1, input$s2, input$s3, input$s4) / 100)
  others <- reactive(max(0, 1 - sum(named())))
  valid_market <- reactive(sum(named()) <= 1 + 1e-9)

  output$others_ui <- renderUI({
    if (!valid_market()) p(strong("The four shares add up to more than 100%. Lower one of them."), style = "color:#8a4b00;")
    else muted(paste0("Other firms together: ", formatC(100 * others(), format = "f", digits = 1), "%"))
  })

  shares <- reactive({
    req(valid_market())
    n_oth <- if (is.na(input$n_oth)) 1 else input$n_oth
    market_shares(named(), others(), n_oth)
  })

  firm_table <- reactive({
    req(valid_market())
    cs <- mcase()
    n_oth <- max(1, round(if (is.na(input$n_oth)) 1 else input$n_oth))
    keep <- named() > 0
    d <- data.frame(firm = cs$names[keep], s = named()[keep])
    if (others() > 1e-9) d <- rbind(d, data.frame(
      firm = if (n_oth == 1) "Others" else paste0("Others (each of ", n_oth, ")"), s = others() / n_oth))
    d
  })

  for (sfx in c("", "2")) local({
    s <- sfx
    output[[paste0("mk_story", s)]] <- renderUI(mcase()$story)
    output[[paste0("mk_decided", s, "_wrap")]] <- renderUI({
      if (is.null(mcase()$decided)) NULL else
        accordion(open = FALSE, accordion_panel("What was decided? (open after you have made up your mind)", mcase()$decided))
    })
    output[[paste0("mk_source", s)]] <- renderUI(if (is.null(mcase()$source)) NULL else tagList("Source: ", mcase()$source))
  })

  # Tab 1 ---------------------------------------------------------------------
  t1_ok <- reactive(cournot_consistent(shares(), input$eta1))

  output$t1_guard <- renderUI({
    if (t1_ok()) NULL else card(class = "border-warning", card_body(p(strong("These numbers are inconsistent. "),
      paste0("Cournot competition with positive costs requires that |η| exceeds the largest share written as a fraction (",
             pct(max(shares())), " = ", formatC(max(shares()), format = "f", digits = 2), "). Raise the elasticity."))))
  })

  output$t1_box_top <- renderUI({
    d <- firm_table(); i <- which.max(d$s)
    if (!t1_ok()) return(number_box("Largest firm's Lerner index", "–"))
    number_box(paste0("Lerner index of ", d$firm[i]), pct(lerner_firm(d$s[i], input$eta1)),
               paste0("Share ", pct(d$s[i]), " / |η| ", formatC(input$eta1, format = "f", digits = 1), " (illustrative)"))
  })
  output$t1_box_ind <- renderUI({
    if (!t1_ok()) return(number_box("Industry Lerner index", "–"))
    number_box("Industry Lerner index", pct(lerner_industry(shares(), input$eta1)),
               paste0("HHI ", fmt_hhi(hhi(shares())), " / (10,000 × |η|)"), lab_colors[["actual"]])
  })
  output$t1_box_dom <- renderUI({
    smax <- max(shares())
    if (round(10000 * smax) >= 4000 && round(10000 * smax) < 5000)
      number_box("Largest share: EU dominance benchmark (for comparison)", pct(smax), dominance_text(smax))   # neither flagged nor cleared
    else verdict_box("Largest share: EU dominance benchmark (for comparison)", pct(smax), round(10000 * smax) >= 5000, dominance_text(smax))
  })

  output$t1_plot <- renderPlot({
    d <- firm_table()
    validate(need(t1_ok(), "Raise the elasticity (see the check above)."))
    d$L <- lerner_firm(d$s, input$eta1)
    d$firm <- factor(d$firm, levels = rev(d$firm))
    Lbar <- lerner_industry(shares(), input$eta1)
    xmax <- max(0.2, max(d$L), Lbar) * 1.25
    ggplot(d, aes(L, firm)) +
      geom_col(width = 0.6, fill = lab_colors[["pass"]]) +
      geom_text(aes(label = pct(L)), hjust = -0.15, size = 5, color = lab_colors[["ink"]]) +
      geom_vline(xintercept = Lbar, color = lab_colors[["actual"]], linewidth = 1.2, linetype = "dashed") +
      annotate("text", x = Lbar, y = 0.45, label = paste("Industry", pct(Lbar)), hjust = -0.05, vjust = 0,
               color = lab_colors[["actual"]], size = 4.8) +
      scale_x_continuous(labels = function(x) pct(x, 0), limits = c(0, xmax), expand = expansion(mult = c(0, 0.02))) +
      labs(x = "Lerner index (p − c)/p", y = NULL) +
      lab_gg() + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = lab_colors[["grid"]]))
  })

  output$t1_table <- renderTable({
    d <- firm_table()
    data.frame(Firm = d$firm, Share = pct(d$s),
               `Lerner index` = if (t1_ok()) pct(lerner_firm(d$s, input$eta1)) else "–",
               `Own elasticity |η_i| = |η|/s_i` = formatC(input$eta1 / d$s, format = "f", digits = 1), check.names = FALSE)
  }, striped = TRUE, width = "100%")

  # Tab 2 ---------------------------------------------------------------------
  scr <- reactive({
    validate(need(valid_market(), "The four shares add up to more than 100%. Lower one of them."))
    iA <- as.integer(input$mA); iB <- as.integer(input$mB)
    validate(need(iA != iB, "Choose two different firms."),
             need(named()[iA] > 0 && named()[iB] > 0, "Both merging firms need a positive share."))
    s <- shares()
    # named firms come first in shares(); positions 1-4 map directly
    merger_screen(s, iA, iB)
  })
  mnames <- reactive(mcase()$names[c(as.integer(input$mA), as.integer(input$mB))])

  output$t2_pre   <- renderUI(number_box("HHI before", fmt_hhi(scr()$pre)))
  output$t2_post  <- renderUI(number_box("HHI after", fmt_hhi(scr()$post), NULL, lab_colors[["actual"]]))
  output$t2_delta <- renderUI(number_box("Change in HHI", fmt_hhi(scr()$delta), paste0("2 × ", formatC(100 * named()[as.integer(input$mA)], format = "f", digits = 1),
                                          " × ", formatC(100 * named()[as.integer(input$mB)], format = "f", digits = 1)), lab_colors[["actual"]]))
  output$t2_cr4   <- renderUI(number_box("CR4 before", paste0(formatC(scr()$cr4, format = "f", digits = 1), "%"),
                                         paste0("Share of the four largest firms, with the other firms split as set in the sidebar. The three largest have ", formatC(scr()$cr3, format = "f", digits = 1), "%.")))
  output$t2_us <- renderUI({
    r <- scr()
    why <- if (r$post > 1800 && r$delta > 100) "HHI above 1,800 and change above 100"
           else if (r$comb > 0.30 && r$delta > 100) "combined share above 30% and change above 100"
           else "neither test is met"
    verdict_box("Do the US 2023 Guidelines presume harm?", if (r$us) "Yes" else "No", r$us, why)
  })
  output$t2_eu <- renderUI({
    r <- scr()
    verdict_box("Is the merger outside the EU 2004 levels where concerns are unlikely?", if (r$eu) "No" else "Yes", !r$eu,
      if (r$eu) paste0("Inside the safe harbor, because ", r$eu_route, ".") else "Outside the safe harbor, there is no presumption either way, and the Commission assesses the effects.")
  })

  output$t2_plot <- renderPlot({
    r <- scr()
    ymax <- 1000   # fixed axis, so that mergers can be compared; larger changes sit at the top edge
    us <- "United States (2023 Guidelines)"; eu <- "European Union (2004 Guidelines)"
    lv <- c(us, eu)
    # Same colors in both panels: sand where the screen raises no concern, blue where it does
    reg <- rbind(
      data.frame(panel = us, xmin = c(0, 1800), xmax = c(1800, 10000), ymin = 0, ymax = c(ymax, 100), kind = "calm"),
      data.frame(panel = us, xmin = 1800, xmax = 10000, ymin = 100, ymax = ymax, kind = "concern"),
      data.frame(panel = eu, xmin = c(0, 1000, 2000), xmax = c(1000, 2000, 10000), ymin = 0, ymax = c(ymax, 250, 150), kind = "calm"),
      data.frame(panel = eu, xmin = c(1000, 2000), xmax = c(2000, 10000), ymin = c(250, 150), ymax = ymax, kind = "concern"))
    reg$panel <- factor(reg$panel, levels = lv)
    txt <- data.frame(panel = factor(c(us, us, eu, eu), levels = lv),
                      x = c(9800, 100, 100, 9800), y = c(0.95, 0.95, 0.95, 0.95) * ymax,
                      hjust = c(1, 0, 0, 1),
                      lab = c("Harm is presumed", "No presumption", "Concerns\nunlikely", "Outside the safe harbor:\nassessed on the merits"),
                      col = c(lab_colors[["pass"]], lab_colors[["ink"]], lab_colors[["ink"]], lab_colors[["pass"]]))
    off <- r$delta > ymax
    pt <- data.frame(panel = factor(lv, levels = lv), x = r$post, y = min(r$delta, ymax),
                     lab = paste0(paste(mnames(), collapse = " + "), "\nHHI ", fmt_hhi(r$post), ", change ", fmt_hhi(r$delta),
                                  if (off) "\n(above the top of the chart)" else ""))
    ggplot() +
      geom_rect(data = reg, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = kind, alpha = kind)) +
      scale_fill_manual(values = c(concern = lab_colors[["pass"]], calm = lab_colors[["fail"]]), guide = "none") +
      scale_alpha_manual(values = c(concern = 0.22, calm = 0.85), guide = "none") +
      geom_text(data = txt, aes(x, y, label = lab, hjust = hjust), vjust = 1, color = txt$col, size = 4.6, lineheight = 0.9) +
      geom_point(data = pt, aes(x, y), size = 5, color = lab_colors[["actual"]], shape = if (off) 17 else 16) +
      geom_text(data = pt, aes(x, y, label = lab), hjust = ifelse(r$post > 5000, 1.06, -0.06),
                vjust = if (pt$y[1] > 0.6 * ymax) 1.6 else -0.4,
                size = 4.4, lineheight = 0.95, color = lab_colors[["ink"]]) +
      facet_wrap(~panel, nrow = 1) +
      scale_x_continuous(labels = function(x) formatC(x, format = "d", big.mark = ","), limits = c(0, 10000),
                         breaks = c(0, 2000, 5000, 10000), expand = expansion(mult = c(0, 0.02))) +
      scale_y_continuous(labels = function(x) formatC(x, format = "d", big.mark = ","), limits = c(0, ymax),
                         breaks = c(0, 100, 250, 500, 1000), expand = expansion(mult = c(0, 0.02))) +
      labs(x = "HHI after the merger", y = "Change in HHI") +
      lab_gg() + theme(strip.text = element_text(size = 15, face = "bold", hjust = 0), panel.spacing = unit(1.6, "lines"), plot.margin = margin(6, 22, 6, 6))
  })

  # Tab 3 ---------------------------------------------------------------------
  t3 <- reactive(lerner_sym(1:10, input$eta3))
  t3n <- reactive(t3()[t3()$n == input$n3, ])
  output$t3_hhi <- renderUI(number_box("HHI", fmt_hhi(t3n()$hhi), paste(input$n3, "equal firms")))
  output$t3_c <- renderUI(number_box("Cournot (quantities)", pct(t3n()$cournot), NULL, lab_colors[["pass"]]))
  output$t3_b <- renderUI(number_box("Bertrand (prices, same product)", pct(t3n()$bertrand), NULL, lab_colors[["actual"]]))
  output$t3_k <- renderUI(number_box("Full coordination", pct(t3n()$coordination), "upper bound", lab_colors[["critical"]]))

  output$t3_plot <- renderPlot({
    d <- t3()
    long <- rbind(data.frame(hhi = d$hhi, L = d$cournot, conduct = "Cournot"),
                  data.frame(hhi = d$hhi, L = d$bertrand, conduct = "Bertrand"),
                  data.frame(hhi = d$hhi, L = d$coordination, conduct = "Full coordination"))
    cols <- c("Cournot" = lab_colors[["pass"]], "Bertrand" = lab_colors[["actual"]], "Full coordination" = lab_colors[["critical"]])
    sel <- t3n()
    ends <- long[long$hhi == 10000 / 2, ]
    ggplot(long, aes(hhi, L, color = conduct)) +
      geom_vline(xintercept = sel$hhi, color = lab_colors[["muted"]], linetype = "dotted") +
      geom_line(data = long[long$hhi <= 5000, ], linewidth = 1.2) + geom_point(size = 2.5) +
      annotate("text", x = 10000, y = 1 / input$eta3, label = "Monopoly (n = 1)", hjust = 1, vjust = 2, size = 4.5, color = lab_colors[["ink"]]) +
      geom_point(data = long[long$hhi == sel$hhi, ], size = 5) +
      geom_text(data = ends, aes(label = conduct), hjust = 0.5, vjust = -0.9, size = 4.8, show.legend = FALSE) +
      scale_color_manual(values = cols, guide = "none") +
      scale_x_continuous(labels = function(x) formatC(x, format = "d", big.mark = ","), breaks = c(1000, 2000, 3333, 5000, 10000)) +
      scale_y_continuous(labels = function(x) pct(x, 0), limits = c(0, max(d$coordination) * 1.15)) +
      labs(x = "HHI (n equal firms: 10,000 / n)", y = "Lerner index (p − c)/p") +
      lab_gg()
  })

  # Tab 4 ---------------------------------------------------------------------
  gcase <- reactive(guppi_cases[[input$g_case]])
  observeEvent(input$g_case, {
    cs <- gcase()
    updateSliderInput(session, "gsA", value = cs$sA); updateSliderInput(session, "gsB", value = cs$sB)
    updateSliderInput(session, "gDAB", value = cs$DAB); updateSliderInput(session, "gDBA", value = cs$DBA)
    updateSliderInput(session, "gmA", value = cs$mA); updateSliderInput(session, "gmB", value = cs$mB)
  }, ignoreInit = TRUE)

  g <- reactive({
    sA <- input$gsA / 100; sB <- input$gsB / 100
    validate(need(sA + sB <= 1, "The two shares add up to more than 100%."), need(sA > 0 && sB > 0, "Both shares must be positive."))
    rivals <- rescale_rivals(gcase()$rivals / 100, sA, sB)
    s <- c(sA, sB, rivals)
    r <- merger_screen(s, 1, 2)
    r$gA <- guppi(input$gDAB / 100, input$gmB / 100)
    r$gB <- guppi(input$gDBA / 100, input$gmA / 100)
    r$bAB <- diversion_share_benchmark(sA, sB); r$bBA <- diversion_share_benchmark(sB, sA)
    r$nriv <- length(rivals)
    r
  })

  output$t4_note <- renderUI({ n <- gcase()$note; if (is.null(n)) NULL else card(card_body(p(n))) })
  output$t4_us <- renderUI(verdict_box("US presumption?", if (g()$us) "Yes" else "No", g()$us,
                                       paste0("HHI ", fmt_hhi(g()$pre), " → ", fmt_hhi(g()$post), ", change ", fmt_hhi(g()$delta))))
  output$t4_eu <- renderUI(verdict_box("Outside the EU safe harbor?", if (g()$eu) "No" else "Yes", !g()$eu,
                                       if (g()$eu) "Inside the safe harbor, concerns are unlikely" else "No presumption either way"))
  output$t4_gA <- renderUI(number_box("GUPPI of A", pct(g()$gA), paste0(input$gDAB, "% diversion × ", input$gmB, "% margin of B"), lab_colors[["actual"]]))
  output$t4_gB <- renderUI(number_box("GUPPI of B", pct(g()$gB), paste0(input$gDBA, "% diversion × ", input$gmA, "% margin of A"), lab_colors[["actual"]]))

  output$t4_plot <- renderPlot({
    r <- g()
    d <- data.frame(prod = factor(c("Product A", "Product B"), levels = c("Product A", "Product B")), v = c(r$gA, r$gB))
    ymax <- max(0.08, max(d$v) * 1.3)
    ggplot(d, aes(prod, v)) +
      geom_col(width = 0.5, fill = lab_colors[["actual"]]) +
      geom_hline(yintercept = 0.05, color = lab_colors[["critical"]], linetype = "dashed", linewidth = 1) +
      annotate("text", x = 1.5, y = 0.05, label = "5%", hjust = 0.5, vjust = -0.4, color = lab_colors[["critical"]], size = 4.6) +
      geom_text(aes(label = pct(v)), vjust = -0.5, size = 5.2, color = lab_colors[["ink"]]) +
      scale_y_continuous(labels = function(x) pct(x, 0), limits = c(0, ymax), expand = expansion(mult = c(0, 0.02))) +
      labs(x = NULL, y = "GUPPI") + lab_gg()
  })

  output$t4_bench <- renderUI({
    r <- g()
    cls <- function(D, b) if (D > 1.5 * b) "closer" else if (D < b / 1.5) "distant" else "similar"
    k <- c(cls(input$gDAB / 100, r$bAB), cls(input$gDBA / 100, r$bBA))
    verdict <- if (all(k == "closer")) "The two products are much closer substitutes than their shares suggest, so the HHI understates the unilateral pricing pressure."
      else if (all(k == "distant")) "Shoppers switch between the two products less than their shares suggest, so the HHI overstates the unilateral pricing pressure from this switching."
      else if (all(k == "similar")) "Diversion is close to what the shares suggest, so HHI and GUPPI tell a similar story."
      else "Diversion differs in the two directions, so compare each GUPPI with what the shares suggest." 
    card(card_body(
      p(strong("What the shares would predict. "),
        paste0("If customers left in proportion to market shares, A would lose ", pct(r$bAB), " of its lost sales to B and B ", pct(r$bBA),
               " to A. The actual diversion is ", input$gDAB, "% and ", input$gDBA, "%. ",
               verdict)),
      if (!is.null(gcase()$bench_note)) p(strong(gcase()$bench_note)),
      muted(paste0("Rivals: ", r$nriv, " firms sharing the rest of the market in the preset's proportions."))))
  })
}

shinyApp(ui, server)
