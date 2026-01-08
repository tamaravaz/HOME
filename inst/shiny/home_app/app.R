options(shiny.maxRequestSize = 30 * 1024^2) # 30MB

# ==============================================================================
# UI
# ==============================================================================
ui <- bslib::page_sidebar(
  theme = bslib::bs_theme(version = 5, bootswatch = "zephyr"),
  title = "HOME Dashboard",

  tags$head(
    tags$style(HTML("
      table.dataTable tbody th, table.dataTable tbody td {
        padding: 4px 8px !important; font-size: 0.9rem;
      }
      .btn-discrete {
        padding: 2px 6px; font-size: 0.8rem; color: #6c757d;
        border-color: #dee2e6; background-color: transparent;
      }
      .btn-discrete:hover { background-color: #e9ecef; color: #000; }
      .card-header .form-group { margin-bottom: 0 !important; }
      .header-slider .irs-single { font-size: 0.8rem; }
    "))
  ),

  sidebar = bslib::sidebar(
    title = "Setup",
    width = 300,
    shiny::fileInput("file_upload", "Upload Data",
                     accept = c(".xlsx", ".csv"),
                     placeholder = "No file..."),
    shiny::selectInput("method", "Method",
                       choices = c("Luy (2012)"="luy",
                                   "Brass (1973)"="brass",
                                   "Timaeus (1992)"="timaeus")),
    shiny::selectInput("sex", "Parent Sex",
                       choices = c("Female", "Male")),
    shiny::numericInput("survey_date", "Survey Year",
                        value = 2024.75, step = 0.01),
    shiny::selectInput("family", "Model Family",
                       choices = c("UN General"="General",
                                   "UN Latin"="Latin",
                                   "UN Chilean"="Chilean",
                                   "UN South Asian"="South_Asian",
                                   "UN Far East"="Far_East_Asian",
                                   "CD West"="West",
                                   "CD North"="North",
                                   "CD East"="East",
                                   "CD South"="South")),
    shiny::hr(),
    shiny::actionButton("run", "Estimate",
                        class = "btn-primary w-100"),
    shiny::br(), shiny::br(),
    shiny::downloadButton("download_template",
                          "Get Template",
                          class = "btn-xs btn-link")
  ),

  bslib::navset_card_underline(

    # ================= TAB 1 =================
    bslib::nav_panel(
      "Estimates",
      bslib::layout_columns(
        col_widths = c(8, 4),

        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            "Trend Analysis",
            shiny::checkboxInput("fix_y_scale", "Fix Y-Axis", TRUE),
            shiny::uiOutput("ui_year_slider"),
            shiny::selectInput("plot_index", NULL,
                               choices = c("30q30", "e30", "45q15"),
                               selected = "30q30",
                               selectize = FALSE)
          ),
          plotly::plotlyOutput("plot_main", height = "500px")
        ),

        bslib::card(
          bslib::card_header(
            "Estimates Table",
            shiny::downloadButton(
              "download_results",
              label = NULL,
              icon = shiny::icon("download"),
              class = "btn-discrete"
            )
          ),
          DT::DTOutput("table_results")
        )
      )
    ),

    # ================= TAB 2 =================
    bslib::nav_panel(
      "Diagnostics",

      bslib::card(
        height = "320px",
        bslib::card_header("Internal Consistency (Linearity)"),
        shiny::plotOutput("plot_linearity")
      ),

      shiny::radioButtons(
        "sens_metric", NULL,
        choices = c("Prob 30q30" = "30q30",
                    "Prob 45q15" = "45q15",
                    "Expectancy e30" = "e30"),
        inline = TRUE
      ),

      bslib::layout_columns(
        col_widths = c(6, 6),

        bslib::card(
          bslib::card_header(
            "Sensitivity: Fertility (Mn)",
            shiny::selectInput("mn_range_val", NULL,
                               choices = c(0.5, 1, 1.5, 2, 3),
                               selected = 1.5,
                               selectize = FALSE)
          ),
          shiny::plotOutput("plot_sens_m")
        ),

        bslib::card(
          bslib::card_header(
            "Sensitivity: Model Family",
            shiny::selectInput("sens_fam_system", NULL,
                               choices = c("UN", "CD", "All"),
                               selectize = FALSE)
          ),
          shiny::plotOutput("plot_sens_fam")
        )
      )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  output$download_template <- shiny::downloadHandler(
    filename = function() "input_template.xlsx",
    content = function(file) {
      writexl::write_xlsx(
        data.frame(
          Age = seq(15, 60, 5),
          Sn = runif(10, 0.1, 1),
          Mn = rep(24, 10)
        ),
        file
      )
    }
  )

  data_reactive <- shiny::reactive({
    shiny::req(input$file_upload)
    ext <- tools::file_ext(input$file_upload$name)
    if (ext == "csv") {
      read.csv(input$file_upload$datapath)
    } else {
      readxl::read_excel(input$file_upload$datapath)
    }
  })

  est_reactive <- shiny::eventReactive(input$run, {
    shiny::req(data_reactive())
    HOME::om_estimate_index(
      method = input$method,
      sex_parent = input$sex,
      age_respondent = data_reactive()$age,
      p_surv = data_reactive()$sn,
      mean_age_parent = data_reactive()$mn,
      surv_date = input$survey_date,
      model_family = input$family
    )
  })

  output$ui_year_slider <- shiny::renderUI({
    shiny::req(est_reactive())
    yrs <- na.omit(est_reactive()$estimates$RefTime)
    shiny::sliderInput("plot_year_range", NULL,
                       min = floor(min(yrs)),
                       max = ceiling(max(yrs)),
                       value = range(yrs),
                       step = 1)
  })

  output$plot_main <- plotly::renderPlotly({
    shiny::req(est_reactive())
    df <- est_reactive()$estimates

    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = RefTime, y = .data[[input$plot_index]])
    ) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::theme_bw()

    plotly::ggplotly(p) |>
      plotly::config(displayModeBar = FALSE)
  })

  output$table_results <- DT::renderDT({
    DT::datatable(est_reactive()$estimates,
                  options = list(dom = "t", paging = FALSE))
  })

  output$download_results <- shiny::downloadHandler(
    filename = function() "HOME_Results.xlsx",
    content = function(file) {
      writexl::write_xlsx(est_reactive()$estimates, file)
    }
  )

  output$plot_linearity <- shiny::renderPlot({
    HOME::om_plot_linearity(est_reactive())
  })

  output$plot_sens_m <- shiny::renderPlot({
    HOME::om_sensitivity(est_reactive(),
                         range_m = seq(-input$mn_range_val,
                                       input$mn_range_val, 0.5))
  })

  output$plot_sens_fam <- shiny::renderPlot({
    HOME::om_sensitivity_family(est_reactive(),
                                type = input$sens_fam_system)
  })
}

shiny::shinyApp(ui, server)
