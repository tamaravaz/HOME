# ==============================================================================
# HOME: Household-based Orphanhood Mortality Estimation
# Shiny Application — inst/shiny/app.R
#
# This app provides an interactive interface for the HOME package functions.
# Launch via: shiny::runApp(system.file("shiny", package = "HOME"))
#
# Dependencies (must be listed in DESCRIPTION Suggests field):
#   shiny, bslib, ggplot2, plotly, DT, readxl, writexl
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Package Imports
# ------------------------------------------------------------------------------

library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(DT)
library(readxl)
library(writexl)

# ------------------------------------------------------------------------------
# 1. Constants
# ------------------------------------------------------------------------------

.METHODS <- c(
  "Luy (2012)"     = "luy",
  "Brass (1973)"   = "brass",
  "Timaeus (1992)" = "timaeus"
)

.FAMILIES <- list(
  "UN families" = c(
    "UN General"        = "General",
    "UN Latin"          = "Latin",
    "UN Chilean"        = "Chilean",
    "UN South Asian"    = "South_Asian",
    "UN Far East Asian" = "Far_East_Asian"
  ),
  "CD families" = c(
    "CD West"  = "West",
    "CD North" = "North",
    "CD East"  = "East",
    "CD South" = "South"
  )
)

.METRICS <- c(
  "\u2083\u2080q\u2083\u2080 (Prob. of dying ages 30\u201360)" = "30q30",
  "e\u2083\u2080 (Life expectancy at age 30)"                  = "e30",
  "\u2084\u2085q\u2081\u2085 (Prob. of dying ages 15\u201360)" = "45q15"
)

# Axis labels using HTML subscript notation (compatible with plotly layout())
.AXIS_LABELS <- c(
  "30q30" = "<sub>30</sub>q<sub>30</sub>",
  "e30"   = "e<sub>30</sub>",
  "45q15" = "<sub>45</sub>q<sub>15</sub>"
)

# Example dataset: maternal orphanhood data for Roma population in 13 European
# countries (Austria, Bulgaria, Croatia, Czechia, France, Greece, Hungary,
# Italy, Portugal, Romania, Slovakia, Slovenia, Spain), collected 2024.
#
# Source: European Union Agency for Fundamental Rights (2025).
#   Rights of Roma and Travellers in 13 European Countries: Perspectives from
#   the Roma Survey 2024. Publications Office of the EU.
#   https://doi.org/10.2811/5671307
#
# Column n:  lower bound of five-year respondent age group (e.g., 15 = age 15-19).
# Column sn: proportion of respondents with mother reported alive.
# Column mn: mean age of mothers at respondent's birth (M_n), years.
.TEMPLATE <- data.frame(
  n  = c(15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L, 60L),
  sn = c(
    0.95772787,  # age group 15-19
    0.94418605,  # age group 20-24
    0.89402174,  # age group 25-29
    0.84395199,  # age group 30-34
    0.77974435,  # age group 35-39
    0.67717391,  # age group 40-44
    0.49225268,  # age group 45-49
    0.33670034,  # age group 50-54
    0.20071685,  # age group 55-59
    0.09517426   # age group 60-64
  ),
  mn = c(24.52141, 24.20389, 23.69681, 23.48293, 23.26149,
         23.43733, 22.00104, 23.37398, 21.069,   19.92958),
  stringsAsFactors = FALSE
)

# Survey date: Roma Survey 2024 (decimal year ~2024 + 9.88/12 = Oct 2024)
.EXAMPLE_SURVEY_DATE <- 2024.752792

# ------------------------------------------------------------------------------
# 2. Helper Functions
# ------------------------------------------------------------------------------

#' Validate uploaded data frame
validate_input_data <- function(df) {
  shiny::validate(
    shiny::need(
      all(c("n", "sn", "mn") %in% names(df)),
      "Uploaded file must contain columns: n (age group), sn (survival proportion), mn (mean age at parenthood)."
    ),
    shiny::need(
      is.numeric(df$n) && all(df$n > 0),
      "Column 'n' must contain positive numeric values representing respondent age groups."
    ),
    shiny::need(
      is.numeric(df$sn) && all(df$sn > 0 & df$sn <= 1),
      "Column 'sn' must contain survival proportions in the interval (0, 1]."
    ),
    shiny::need(
      is.numeric(df$mn) && all(df$mn > 0),
      "Column 'mn' must contain positive numeric values for mean age at parenthood."
    )
  )
  invisible(TRUE)
}

#' Read uploaded file as data frame
read_input_file <- function(file_info) {
  ext <- tools::file_ext(file_info$name)
  df  <- switch(ext,
                csv  = utils::read.csv(file_info$datapath, stringsAsFactors = FALSE),
                xlsx = readxl::read_excel(file_info$datapath),
                shiny::validate(shiny::need(FALSE, "Unsupported file format. Please upload a .csv or .xlsx file."))
  )
  names(df) <- tolower(trimws(names(df)))
  df
}

# ------------------------------------------------------------------------------
# 3. UI
# ------------------------------------------------------------------------------

.THEME <- bslib::bs_theme(
  version       = 5,
  bg            = "#FFFFFF",
  fg            = "#1A2340",
  primary       = "#003082",
  secondary     = "#5A6A85",
  success       = "#2E7D52",
  info          = "#1B6CA8",
  warning       = "#F5C400",
  danger        = "#C0392B",
  base_font     = bslib::font_google("Source Sans 3", wght = c(300, 400, 600)),
  heading_font  = bslib::font_google("Source Serif 4", wght = c(400, 600)),
  font_scale    = 0.95,
  `navbar-bg`   = "#003082",
  `sidebar-bg`  = "#F4F6FA",
  `card-border-color` = "#DDE3EE"
)

ui <- bslib::page_sidebar(
  theme = .THEME,
  title = shiny::span(
    style = "color: #FFFFFF; font-weight: 600; letter-spacing: 0.02em;",
    "HOME",
    shiny::span(style = "color: #F5C400; margin: 0 6px;", "\u2014"),
    shiny::span(style = "font-weight: 300; color: #B8C8E8;",
                "Indirect Adult Mortality Estimation")
  ),

  tags$head(tags$style(HTML("
    table.dataTable tbody td { padding: 4px 8px !important; font-size: 0.875rem; }
    .btn-icon { padding: 2px 6px; font-size: 0.8rem; color: #5A6A85; border-color: #DDE3EE; }
    .navbar { border-bottom: 3px solid #F5C400 !important; }
    .bslib-sidebar-layout > .sidebar { border-right: 1px solid #DDE3EE !important; }
    .sidebar .sidebar-title {
      color: #003082 !important; font-weight: 600 !important; font-size: 0.85rem !important;
      text-transform: uppercase !important; letter-spacing: 0.08em !important;
      border-bottom: 2px solid #F5C400; padding-bottom: 6px; margin-bottom: 12px;
    }
    .card { border-radius: 6px !important; box-shadow: 0 1px 4px rgba(0,48,130,0.07) !important; }
    .card-header {
      background-color: #F4F6FA !important; border-bottom: 1px solid #DDE3EE !important;
      color: #003082 !important; font-weight: 600 !important; font-size: 0.9rem !important;
    }
    .nav-underline .nav-link.active {
      color: #003082 !important; border-bottom-color: #F5C400 !important;
      border-bottom-width: 3px !important; font-weight: 600 !important;
    }
    .nav-underline .nav-link { color: #5A6A85 !important; }
    .nav-underline .nav-link:hover { color: #003082 !important; border-bottom-color: #DDE3EE !important; }
    .btn-primary {
      background-color: #003082 !important; border-color: #003082 !important;
      font-weight: 600 !important; letter-spacing: 0.03em !important;
    }
    .btn-primary:hover { background-color: #00216A !important; border-color: #00216A !important; }
    .form-check-input:checked { background-color: #003082 !important; border-color: #003082 !important; }
    .form-label, .control-label, label {
      color: #1A2340 !important; font-weight: 600 !important; font-size: 0.82rem !important;
    }
    .irs--shiny .irs-bar { background: #003082 !important; border-color: #003082 !important; }
    .irs--shiny .irs-handle { background: #003082 !important; border-color: #003082 !important; }
    .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background: #003082 !important; }
    table.dataTable thead th {
      background-color: #F4F6FA !important; color: #003082 !important;
      font-weight: 600 !important; border-bottom: 2px solid #DDE3EE !important;
    }
    .btn-link { color: #003082 !important; }
    .btn-link:hover { color: #F5C400 !important; }
  "))),

  # ------ Sidebar ------
  sidebar = bslib::sidebar(
    title = "Estimation Setup",
    width = 290,

    shiny::fileInput("file_upload", "Upload Survey Data",
                     accept = c(".xlsx", ".csv"),
                     placeholder = "Requires columns: n, sn, mn"),

    shiny::selectInput("method", "Estimation Method", choices = .METHODS),
    shiny::selectInput("sex",    "Parent Sex",        choices = c("Female", "Male")),

    shiny::numericInput("survey_date", "Survey Date (decimal year)",
                        value = .EXAMPLE_SURVEY_DATE, min = 1950, max = 2100, step = 0.01),

    shiny::selectInput("family", "Model Life Table Family", choices = .FAMILIES),

    shiny::hr(),

    shiny::actionButton("run", "Compute Estimates", class = "btn-primary w-100"),
    shiny::br(),
    shiny::downloadButton("download_template", "Download Example Data",
                          class = "btn-link btn-sm w-100 mt-1"),

    shiny::hr(),

    shiny::tags$small(
      class = "text-muted d-block",
      style = "font-size: 0.75rem; line-height: 1.4;",
      shiny::tags$strong("Example data:"),
      " Roma Survey 2024 (13 European countries).",
      shiny::tags$br(),
      "Source: EU Agency for Fundamental Rights (2025),",
      shiny::tags$em("Rights of Roma and Travellers."),
      shiny::tags$a("doi:10.2811/9919091",
                    href = "https://data.europa.eu/doi/10.2811/9919091",
                    target = "_blank", style = "font-size:0.75rem;")
    )
  ),

  # ------ Main panel ------
  bslib::navset_card_underline(

    # --- Tab 1: Estimates ---
    bslib::nav_panel(
      title = "Estimates",
      bslib::layout_columns(
        col_widths = c(8, 4),

        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            class = "d-flex justify-content-between align-items-center",
            "Temporal Trend",
            shiny::selectInput("plot_metric", NULL, choices = .METRICS, width = "280px")
          ),
          shiny::uiOutput("ui_year_slider"),
          plotly::plotlyOutput("plot_main", height = "420px")
        ),

        bslib::card(
          bslib::card_header(
            class = "d-flex justify-content-between align-items-center",
            "Estimates Table",
            shiny::downloadButton("download_results", NULL,
                                  icon = shiny::icon("download"), class = "btn-icon")
          ),
          DT::DTOutput("table_results")
        )
      )
    ),

    # --- Tab 2: Diagnostics ---
    bslib::nav_panel(
      title = "Diagnostics",
      bslib::navset_card_underline(

        bslib::nav_panel(
          title = "Internal Consistency",
          bslib::card(
            bslib::card_header("Internal Consistency \u2014 Linearity Test"),
            shiny::plotOutput("plot_linearity", height = "500px")
          )
        ),

        bslib::nav_panel(
          title = "Sensitivity",
          bslib::layout_columns(
            col_widths = c(12),
            bslib::card(
              bslib::card_body(
                class = "py-2",
                shiny::radioButtons(
                  "diag_metric", "Mortality indicator",
                  choices = c(
                    "\u2083\u2080q\u2083\u2080 (Prob. of dying ages 30\u201360)" = "30q30",
                    "\u2084\u2085q\u2081\u2085 (Prob. of dying ages 15\u201360)" = "45q15",
                    "e\u2083\u2080 (Life expectancy at age 30)"                   = "e30"
                  ),
                  selected = "30q30", inline = TRUE
                )
              )
            )
          ),
          bslib::layout_columns(
            col_widths = c(6, 6),

            bslib::card(
              bslib::card_header("Sensitivity: Mean Age at Parenthood (M\u2099)"),
              bslib::card_body(
                shiny::fluidRow(
                  shiny::column(6, shiny::numericInput("sens_mn_min", "Offset from (years)",
                                                       value=-2, min=-10, max=-0.5, step=0.5)),
                  shiny::column(6, shiny::numericInput("sens_mn_max", "Offset to (years)",
                                                       value=2,  min=0.5, max=10,  step=0.5))
                ),
                shiny::plotOutput("plot_sens_mn", height = "380px")
              )
            ),

            bslib::card(
              bslib::card_header("Sensitivity: Model Life Table Family"),
              bslib::card_body(
                shiny::selectInput("sens_fam_type", "Family system",
                                   choices  = c("All families"="All","UN families"="UN","CD families"="CD"),
                                   selected = "All"),
                shiny::plotOutput("plot_sens_family", height = "380px")
              )
            )
          )
        )
      )
    ),

    # --- Tab 3: Method Comparison ---
    bslib::nav_panel(
      title = "Method Comparison",
      bslib::layout_columns(
        col_widths = c(3, 9),

        bslib::card(
          bslib::card_body(
            class = "py-3 px-3",
            shiny::radioButtons(
              "comp_metric", "Mortality indicator",
              choices  = c("₃₀q₃₀ (Prob. dying 30\u201360)" = "30q30",
                           "₄₅q₁₅ (Prob. dying 15\u201360)" = "45q15",
                           "e₃₀ (Life expectancy at 30)"     = "e30"),
              selected = "30q30", inline = FALSE
            ),
            shiny::tags$hr(style = "margin: 8px 0;"),
            shiny::selectInput("comp_family", "Model life table family",
                               choices = .FAMILIES, selected = "General"),
            shiny::tags$hr(style = "margin: 8px 0;"),
            shiny::uiOutput("ui_method_warnings"),
            shiny::tags$hr(style = "margin: 8px 0;"),
            shiny::tags$div(
              style = "display:flex; gap:6px;",
              shiny::actionButton("run_compare",    "Compare",
                                  class = "btn-primary btn-sm"),
              shiny::actionButton("show_comp_table", shiny::icon("table"),
                                  title = "View estimates table",
                                  class = "btn-outline-secondary btn-sm"),
              shiny::downloadButton("download_comparison", "",
                                    icon  = shiny::icon("download"),
                                    title = "Download estimates",
                                    class = "btn-outline-secondary btn-sm")
            )
          )
        ),

        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Mortality Estimates by Method"),
          plotly::plotlyOutput("plot_comparison", height = "520px")
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# 4. Server
# ------------------------------------------------------------------------------

server <- function(input, output, session) {

  # 4.1 Template download --------------------------------------------------

  output$download_template <- shiny::downloadHandler(
    filename = function() "HOME_example_RomaSurvey2024.xlsx",
    content  = function(file) writexl::write_xlsx(.TEMPLATE, file)
  )

  # 4.2 Reactive: parsed and validated input data --------------------------

  r_data <- shiny::reactive({
    shiny::req(input$file_upload)
    df <- read_input_file(input$file_upload)
    validate_input_data(df)
    df
  })

  # 4.3 Reactive: estimation results (triggered by button) -----------------

  r_estimates <- shiny::eventReactive(input$run, {
    df <- r_data()
    HOME::om_estimate_index(
      method          = input$method,
      sex_parent      = input$sex,
      age_respondent  = df$n,
      p_surv          = df$sn,
      mean_age_parent = df$mn,
      surv_date       = input$survey_date,
      model_family    = input$family
    )
  })

  # 4.4 Dynamic year-range slider ------------------------------------------

  output$ui_year_slider <- shiny::renderUI({
    shiny::req(r_estimates())
    yrs <- stats::na.omit(r_estimates()$estimates$RefYear)
    shiny::sliderInput(
      inputId = "year_range",
      label   = "Reference period",
      min     = floor(min(yrs)),
      max     = ceiling(max(yrs)),
      value   = range(yrs),
      step    = 1L,
      width   = "100%",
      sep     = ""
    )
  })

  # 4.5 Main trend plot ----------------------------------------------------

  output$plot_main <- plotly::renderPlotly({
    shiny::req(r_estimates(), input$year_range)

    metric <- input$plot_metric
    df_est <- r_estimates()$estimates
    df_sub <- df_est[
      !is.na(df_est$RefYear) &
        df_est$RefYear >= input$year_range[1] &
        df_est$RefYear <= input$year_range[2],
    ]

    p <- ggplot2::ggplot(
      df_sub,
      ggplot2::aes(x = RefYear, y = .data[[metric]])
    ) +
      ggplot2::geom_line(colour = "#2c3e50", linewidth = 0.8) +
      ggplot2::geom_point(size = 2.5) +
      ggplot2::labs(x = "Reference year", y = "") +
      ggplot2::theme_minimal(base_size = 13)

    plotly::ggplotly(p, tooltip = c("x", "y")) |>
      plotly::layout(yaxis = list(title = .AXIS_LABELS[[metric]]))
  })

  # 4.6 Results table ------------------------------------------------------

  output$table_results <- DT::renderDT({
    shiny::req(r_estimates())

    tab <- r_estimates()$estimates

    # Rename mortality index columns to unicode display labels
    rename_map <- c(
      "30q30" = "\u2083\u2080q\u2083\u2080",
      "e30"   = "e\u2083\u2080",
      "45q15" = "\u2084\u2085q\u2081\u2085"
    )
    for (old in names(rename_map)) {
      if (old %in% names(tab)) names(tab)[names(tab) == old] <- rename_map[[old]]
    }

    # RefYear formatted without thousand separator; all other numerics rounded
    numeric_cols <- names(tab)[sapply(tab, is.numeric)]
    round_cols   <- setdiff(numeric_cols, "RefYear")

    DT::datatable(
      tab,
      rownames = FALSE,
      options  = list(pageLength = 10, dom = "tp", scrollX = TRUE)
    ) |>
      DT::formatRound(columns = round_cols, digits = 3L) |>
      DT::formatRound(columns = "RefYear",  digits = 2L, mark = "")
  })

  # 4.7 Diagnostic plots ---------------------------------------------------

  output$plot_linearity <- shiny::renderPlot({
    shiny::req(r_estimates())
    print(HOME::om_plot_linearity(r_estimates()))
  })

  output$plot_sens_mn <- shiny::renderPlot({
    shiny::req(r_estimates(), input$sens_mn_min, input$sens_mn_max, input$diag_metric)
    shiny::validate(
      shiny::need(isTRUE(input$sens_mn_min < input$sens_mn_max),
                  "Offset minimum must be less than maximum.")
    )
    range_m <- seq(input$sens_mn_min, input$sens_mn_max, by = 0.5)
    sens    <- HOME::om_sensitivity(r_estimates(), range_m = range_m)
    p <- tryCatch(plot(sens, index = input$diag_metric), error = function(e) plot(sens))
    print(p)
  })

  output$plot_sens_family <- shiny::renderPlot({
    shiny::req(r_estimates(), input$sens_fam_type, input$diag_metric)
    sens_fam <- HOME::om_sensitivity_family(r_estimates(), type = input$sens_fam_type)
    p <- tryCatch(plot(sens_fam, index = input$diag_metric), error = function(e) plot(sens_fam))
    print(p)
  })

  # 4.8 Results export -----------------------------------------------------

  output$download_results <- shiny::downloadHandler(
    filename = function() paste0("HOME_estimates_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content  = function(file) writexl::write_xlsx(r_estimates()$estimates, file)
  )

  # 4.9 Method comparison ---------------------------------------------------

  .TIMAEUS_MAX_AGE <- 45L

  r_comparison <- shiny::eventReactive(input$run_compare, {
    shiny::req(r_data())
    df <- r_data()

    run_method <- function(method) {
      tryCatch(
        HOME::om_estimate_index(
          method          = method,
          sex_parent      = input$sex,
          age_respondent  = df$n,
          p_surv          = df$sn,
          mean_age_parent = df$mn,
          surv_date       = input$survey_date,
          model_family    = input$comp_family
        )$estimates,
        error = function(e) NULL
      )
    }

    luy     <- run_method("luy")
    timaeus <- run_method("timaeus")
    brass   <- run_method("brass")

    if (!is.null(luy))     luy$MethodLabel     <- "Luy (2012)"
    if (!is.null(timaeus)) timaeus$MethodLabel  <- "Timaeus (1992)"
    if (!is.null(brass))   brass$MethodLabel    <- "Brass (1973)"

    do.call(rbind, Filter(Negate(is.null), list(luy, timaeus, brass)))
  })

  # Method limitation warnings
  output$ui_method_warnings <- shiny::renderUI({
    shiny::req(r_data())
    df    <- r_data()
    warns <- list()

    if (max(df$n, na.rm = TRUE) > .TIMAEUS_MAX_AGE) {
      warns[["timaeus"]] <- shiny::tags$p(
        style = "color:#856404; font-size:0.78rem; margin:0 0 4px 0; line-height:1.3;",
        shiny::tags$strong("\u26a0 Timaeus (1992):"),
        " weights only published up to age group 45."
      )
    }

    if (length(warns) > 0L) {
      shiny::tags$div(
        style = "background:#fffbea; border-left:3px solid #F5C400; padding:6px 8px; border-radius:3px;",
        do.call(shiny::tagList, warns)
      )
    }
  })

  # Helper: build comparison table with correct RefYear formatting
  .comp_table_dt <- function(tab, metric) {
    DT::datatable(
      tab,
      rownames = FALSE,
      options  = list(pageLength = 15L, dom = "tp", scrollX = TRUE,
                      order = list(list(0L, "asc"), list(2L, "desc")))
    ) |>
      DT::formatRound(columns = c("Alpha", metric), digits = 3L) |>
      DT::formatRound(columns = "RefYear", digits = 2L, mark = "")
  }

  # Modal table on button click
  shiny::observeEvent(input$show_comp_table, {
    shiny::req(r_comparison())
    metric <- input$comp_metric
    tab    <- r_comparison()[, c("MethodLabel", "Age", "RefYear", "Alpha", metric)]
    names(tab) <- c("Method", "Age group", "RefYear", "Alpha", metric)

    shiny::showModal(shiny::modalDialog(
      title  = "Estimates by Method",
      size   = "l",
      footer = shiny::tagList(
        shiny::downloadButton("download_comparison", "Download", class = "btn-sm"),
        shiny::modalButton("Close")
      ),
      DT::renderDT(.comp_table_dt(tab, metric))
    ))
  })

  # Render table for modal (also used by download)
  output$table_comparison <- DT::renderDT({
    shiny::req(r_comparison())
    metric <- input$comp_metric
    tab    <- r_comparison()[, c("MethodLabel", "Age", "RefYear", "Alpha", metric)]
    names(tab) <- c("Method", "Age group", "RefYear", "Alpha", metric)
    .comp_table_dt(tab, metric)
  })

  # Comparison plot
  output$plot_comparison <- plotly::renderPlotly({
    shiny::req(r_comparison())

    df_comp <- r_comparison()
    metric  <- input$comp_metric
    df_comp <- df_comp[!is.na(df_comp$RefYear) & !is.na(df_comp[[metric]]), ]

    pal <- c(
      "Luy (2012)"     = "#003082",
      "Timaeus (1992)" = "#5A8FC2",
      "Brass (1973)"   = "#F5C400"
    )

    p <- ggplot2::ggplot(
      df_comp,
      ggplot2::aes(
        x      = RefYear,
        y      = .data[[metric]],
        colour = MethodLabel,
        group  = MethodLabel,
        text   = paste0(
          "Method: ", MethodLabel, "<br>",
          "Year: ",   round(RefYear, 2), "<br>",
          metric, ": ", round(.data[[metric]], 4)
        )
      )
    ) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::geom_point(size = 2.5) +
      ggplot2::scale_colour_manual(values = pal) +
      ggplot2::labs(x = "Reference year", y = "", colour = "Method") +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(legend.position = "bottom")

    plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(
        yaxis  = list(title = .AXIS_LABELS[[metric]]),
        legend = list(orientation = "h", x = 0, y = -0.2)
      )
  })

  # Comparison export
  output$download_comparison <- shiny::downloadHandler(
    filename = function() paste0("HOME_comparison_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content  = function(file) writexl::write_xlsx(r_comparison(), file)
  )
}

# ------------------------------------------------------------------------------
# 5. App Entry Point
# ------------------------------------------------------------------------------

shiny::shinyApp(ui = ui, server = server)
