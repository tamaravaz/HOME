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

# Default template values for the downloadable data template
.TEMPLATE <- data.frame(
  n  = seq(15L, 60L, by = 5L),
  sn = c(0.98, 0.97, 0.95, 0.92, 0.88, 0.82, 0.75, 0.65, 0.55, 0.40),
  mn = rep(27, 10L),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# 2. Helper Functions
# ------------------------------------------------------------------------------

#' Validate uploaded data frame
#'
#' Checks that required columns are present and that values are within
#' plausible ranges for indirect mortality estimation.
#'
#' @param df A data frame read from the uploaded file.
#' @return Invisibly returns \code{TRUE} on success; throws a shiny
#'   \code{validate()} error otherwise.
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
#'
#' Supports \code{.csv} and \code{.xlsx} formats. Column names are
#' coerced to lowercase for consistency.
#'
#' @param file_info The list returned by \code{shiny::fileInput()}.
#' @return A data frame with lowercase column names.
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

# OeAW/VID institutional palette
# Primary blue:  #003082  (OeAW navy)
# Accent yellow: #F5C400  (VID gold)
# Light surface: #F4F6FA
# Muted text:    #5A6A85

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
    shiny::span(
      style = "color: #F5C400; margin: 0 6px;", "\u2014"
    ),
    shiny::span(
      style = "font-weight: 300; color: #B8C8E8;",
      "Indirect Adult Mortality Estimation"
    )
  ),

  tags$head(
    tags$style(HTML("
      /* ── Layout ── */
      table.dataTable tbody td { padding: 4px 8px !important; font-size: 0.875rem; }
      .btn-icon { padding: 2px 6px; font-size: 0.8rem; color: #5A6A85; border-color: #DDE3EE; }

      /* ── Navbar / title bar ── */
      .navbar { border-bottom: 3px solid #F5C400 !important; }

      /* ── Sidebar ── */
      .bslib-sidebar-layout > .sidebar {
        border-right: 1px solid #DDE3EE !important;
      }
      .sidebar .sidebar-title {
        color: #003082 !important;
        font-weight: 600 !important;
        font-size: 0.85rem !important;
        text-transform: uppercase !important;
        letter-spacing: 0.08em !important;
        border-bottom: 2px solid #F5C400;
        padding-bottom: 6px;
        margin-bottom: 12px;
      }

      /* ── Cards ── */
      .card { border-radius: 6px !important; box-shadow: 0 1px 4px rgba(0,48,130,0.07) !important; }
      .card-header {
        background-color: #F4F6FA !important;
        border-bottom: 1px solid #DDE3EE !important;
        color: #003082 !important;
        font-weight: 600 !important;
        font-size: 0.9rem !important;
      }

      /* ── Nav tabs (underline style) ── */
      .nav-underline .nav-link.active {
        color: #003082 !important;
        border-bottom-color: #F5C400 !important;
        border-bottom-width: 3px !important;
        font-weight: 600 !important;
      }
      .nav-underline .nav-link {
        color: #5A6A85 !important;
      }
      .nav-underline .nav-link:hover {
        color: #003082 !important;
        border-bottom-color: #DDE3EE !important;
      }

      /* ── Primary button ── */
      .btn-primary {
        background-color: #003082 !important;
        border-color: #003082 !important;
        font-weight: 600 !important;
        letter-spacing: 0.03em !important;
      }
      .btn-primary:hover {
        background-color: #00216A !important;
        border-color: #00216A !important;
      }

      /* ── Radio buttons & inputs ── */
      .form-check-input:checked {
        background-color: #003082 !important;
        border-color: #003082 !important;
      }
      .form-label, .control-label, label {
        color: #1A2340 !important;
        font-weight: 600 !important;
        font-size: 0.82rem !important;
      }

      /* ── Slider ── */
      .irs--shiny .irs-bar { background: #003082 !important; border-color: #003082 !important; }
      .irs--shiny .irs-handle { background: #003082 !important; border-color: #003082 !important; }
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
        background: #003082 !important;
      }

      /* ── DT table header ── */
      table.dataTable thead th {
        background-color: #F4F6FA !important;
        color: #003082 !important;
        font-weight: 600 !important;
        border-bottom: 2px solid #DDE3EE !important;
      }

      /* ── Download link ── */
      .btn-link { color: #003082 !important; }
      .btn-link:hover { color: #F5C400 !important; }
    "))
  ),

  # ------ Sidebar ------
  sidebar = bslib::sidebar(
    title = "Estimation Setup",
    width = 290,

    shiny::fileInput(
      inputId     = "file_upload",
      label       = "Upload Survey Data",
      accept      = c(".xlsx", ".csv"),
      placeholder = "Requires columns: n, sn, mn"
    ),

    shiny::selectInput(
      inputId  = "method",
      label    = "Estimation Method",
      choices  = .METHODS
    ),

    shiny::selectInput(
      inputId  = "sex",
      label    = "Parent Sex",
      choices  = c("Female", "Male")
    ),

    shiny::numericInput(
      inputId = "survey_date",
      label   = "Survey Date (decimal year)",
      value   = 2024.75,
      min     = 1950,
      max     = 2100,
      step    = 0.01
    ),

    shiny::selectInput(
      inputId  = "family",
      label    = "Model Life Table Family",
      choices  = .FAMILIES
    ),

    shiny::hr(),

    shiny::actionButton(
      inputId = "run",
      label   = "Compute Estimates",
      class   = "btn-primary w-100"
    ),

    shiny::br(),

    shiny::downloadButton(
      outputId = "download_template",
      label    = "Download Data Template",
      class    = "btn-link btn-sm w-100 mt-1"
    )
  ),

  # ------ Main panel ------
  bslib::navset_card_underline(

    # --- Tab 1: Main Estimates ---
    bslib::nav_panel(
      title = "Estimates",

      bslib::layout_columns(
        col_widths = c(8, 4),

        # Trend plot card
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            class = "d-flex justify-content-between align-items-center",
            "Temporal Trend",
            shiny::selectInput(
              inputId  = "plot_metric",
              label    = NULL,
              choices  = .METRICS,
              width    = "280px"
            )
          ),
          shiny::uiOutput("ui_year_slider"),
          plotly::plotlyOutput("plot_main", height = "420px")
        ),

        # Results table card
        bslib::card(
          bslib::card_header(
            class = "d-flex justify-content-between align-items-center",
            "Estimates Table",
            shiny::downloadButton(
              outputId = "download_results",
              label    = NULL,
              icon     = shiny::icon("download"),
              class    = "btn-icon"
            )
          ),
          DT::DTOutput("table_results")
        )
      )
    ),

    # --- Tab 2: Diagnostics ---
    bslib::nav_panel(
      title = "Diagnostics",

      # Sub-tabs: Internal Consistency | Sensitivity
      # The mortality indicator selector appears only in the Sensitivity sub-tab,
      # as om_plot_linearity() plots Alpha (mortality level) independently of
      # any derived index (30q30, 45q15, e30).
      bslib::navset_card_underline(

        # Sub-tab 1: Linearity test (no indicator selector needed)
        bslib::nav_panel(
          title = "Internal Consistency",
          bslib::card(
            bslib::card_header("Internal Consistency \u2014 Linearity Test"),
            shiny::plotOutput("plot_linearity", height = "500px")
          )
        ),

        # Sub-tab 2: Sensitivity analyses
        bslib::nav_panel(
          title = "Sensitivity",

          # Mortality indicator selector — inline in this sub-tab only
          bslib::layout_columns(
            col_widths = c(12),
            bslib::card(
              bslib::card_body(
                class = "py-2",
                shiny::radioButtons(
                  inputId  = "diag_metric",
                  label    = "Mortality indicator",
                  choices  = c(
                    "\u2083\u2080q\u2083\u2080 (Prob. of dying ages 30\u201360)" = "30q30",
                    "\u2084\u2085q\u2081\u2085 (Prob. of dying ages 15\u201360)" = "45q15",
                    "e\u2083\u2080 (Life expectancy at age 30)"                     = "e30"
                  ),
                  selected = "30q30",
                  inline   = TRUE
                )
              )
            )
          ),

          bslib::layout_columns(
            col_widths = c(6, 6),

            # Sensitivity: Mean Age at Parenthood
            bslib::card(
              bslib::card_header(
                "Sensitivity: Mean Age at Parenthood (M\u2099)"
              ),
              bslib::card_body(
                shiny::fluidRow(
                  shiny::column(
                    width = 6,
                    shiny::numericInput(
                      inputId = "sens_mn_min",
                      label   = "Offset from (years)",
                      value   = -2,
                      min     = -10,
                      max     = -0.5,
                      step    = 0.5
                    )
                  ),
                  shiny::column(
                    width = 6,
                    shiny::numericInput(
                      inputId = "sens_mn_max",
                      label   = "Offset to (years)",
                      value   = 2,
                      min     = 0.5,
                      max     = 10,
                      step    = 0.5
                    )
                  )
                ),
                shiny::plotOutput("plot_sens_mn", height = "380px")
              )
            ),

            # Sensitivity: Model Life Table Family
            bslib::card(
              bslib::card_header(
                "Sensitivity: Model Life Table Family"
              ),
              bslib::card_body(
                shiny::selectInput(
                  inputId  = "sens_fam_type",
                  label    = "Family system",
                  choices  = c(
                    "All families" = "All",
                    "UN families"  = "UN",
                    "CD families"  = "CD"
                  ),
                  selected = "All"
                ),
                shiny::plotOutput("plot_sens_family", height = "380px")
              )
            )
          )
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
    filename = function() "HOME_data_template.xlsx",
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
    yrs <- stats::na.omit(r_estimates()$estimates$RefTime)
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
      !is.na(df_est$RefTime) &
        df_est$RefTime >= input$year_range[1] &
        df_est$RefTime <= input$year_range[2],
    ]

    p <- ggplot2::ggplot(
      df_sub,
      ggplot2::aes(x = RefTime, y = .data[[metric]])
    ) +
      ggplot2::geom_line(colour = "#2c3e50", linewidth = 0.8) +
      ggplot2::geom_point(size = 2.5) +
      ggplot2::labs(
        x = "Reference time (year)",
        y = ""           # y-axis title set via plotly layout() below
      ) +
      ggplot2::theme_minimal(base_size = 13)

    plotly::ggplotly(p, tooltip = c("x", "y")) |>
      plotly::layout(
        yaxis = list(title = .AXIS_LABELS[[metric]])
      )
  })

  # 4.6 Results table ------------------------------------------------------

  output$table_results <- DT::renderDT({
    shiny::req(r_estimates())

    tab <- r_estimates()$estimates

    # Rename columns to display-friendly labels
    rename_map <- c(
      "30q30" = "\u2083\u2080q\u2083\u2080",
      "e30"   = "e\u2083\u2080",
      "45q15" = "\u2084\u2085q\u2081\u2085"
    )
    for (old in names(rename_map)) {
      if (old %in% names(tab)) names(tab)[names(tab) == old] <- rename_map[[old]]
    }

    DT::datatable(
      tab,
      rownames  = FALSE,
      options   = list(
        pageLength = 10,
        dom        = "tp",
        scrollX    = TRUE
      )
    ) |>
      DT::formatRound(columns = seq(2L, ncol(tab)), digits = 3L)
  })

  # 4.7 Diagnostic plots ---------------------------------------------------
  # NOTE: om_plot_linearity() returns a ggplot object directly.
  # om_sensitivity() and om_sensitivity_family() return S3 objects of class
  # OrphanhoodSensitivity and OrphanhoodSensitivityFamily respectively;
  # plot() dispatches to their registered S3 plot methods to render the figure.

  output$plot_linearity <- shiny::renderPlot({
    shiny::req(r_estimates())
    print(HOME::om_plot_linearity(r_estimates()))
  })

  output$plot_sens_mn <- shiny::renderPlot({
    shiny::req(r_estimates(), input$sens_mn_min, input$sens_mn_max, input$diag_metric)
    shiny::validate(
      shiny::need(
        isTRUE(input$sens_mn_min < input$sens_mn_max),
        "Offset minimum must be less than maximum."
      )
    )
    range_m <- seq(input$sens_mn_min, input$sens_mn_max, by = 0.5)
    sens    <- HOME::om_sensitivity(r_estimates(), range_m = range_m)
    # Pass index if the plot method supports it; otherwise fall back gracefully
    p <- tryCatch(
      plot(sens, index = input$diag_metric),
      error = function(e) plot(sens)
    )
    print(p)
  })

  output$plot_sens_family <- shiny::renderPlot({
    shiny::req(r_estimates(), input$sens_fam_type, input$diag_metric)
    sens_fam <- HOME::om_sensitivity_family(r_estimates(), type = input$sens_fam_type)
    p <- tryCatch(
      plot(sens_fam, index = input$diag_metric),
      error = function(e) plot(sens_fam)
    )
    print(p)
  })

  # 4.8 Results export -----------------------------------------------------

  output$download_results <- shiny::downloadHandler(
    filename = function() {
      paste0("HOME_estimates_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(r_estimates()$estimates, file)
    }
  )
}

# ------------------------------------------------------------------------------
# 5. App Entry Point
# ------------------------------------------------------------------------------

shiny::shinyApp(ui = ui, server = server)
