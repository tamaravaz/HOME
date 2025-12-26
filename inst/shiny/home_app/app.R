library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(readxl)
library(writexl)
library(plotly)
library(HOME)

options(shiny.maxRequestSize = 30*1024^2) # Aumentado para 30MB por segurança

# ==============================================================================
# UI
# ==============================================================================
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "zephyr"),
  title = "HOME Dashboard",

  # CSS: Ajustes finos de espaçamento e alinhamento
  tags$head(
    tags$style(HTML("
      /* Tabela compacta */
      table.dataTable tbody th, table.dataTable tbody td {
        padding: 4px 8px !important; font-size: 0.9rem;
      }
      /* Botão download discreto */
      .btn-discrete {
        padding: 2px 6px; font-size: 0.8rem; color: #6c757d;
        border-color: #dee2e6; background-color: transparent;
      }
      .btn-discrete:hover { background-color: #e9ecef; color: #000; }

      /* Alinhamento dos inputs nos cabeçalhos dos cards */
      .card-header .form-group { margin-bottom: 0 !important; }
      .card-header .selectize-input { padding: 2px 8px; min-height: 30px; line-height: 24px; font-size: 0.9rem; }
      .card-header .selectize-dropdown { font-size: 0.9rem; }

      /* Ajuste da barra de métrica */
      .metric-bar .shiny-options-group { margin-top: 0px; }
      .metric-bar .radio { margin-bottom: 0px !important; margin-top: 0px !important; }

      /* Slider compacto no header */
      .header-slider .irs-bar { border-top: 1px solid #0d6efd; border-bottom: 1px solid #0d6efd; background: #0d6efd; }
      .header-slider .irs-bar-edge { border: 1px solid #0d6efd; background: #0d6efd; }
      .header-slider .irs-single, .header-slider .irs-min, .header-slider .irs-max { font-size: 0.8rem; }
    "))
  ),

  # --- BARRA LATERAL ---
  sidebar = sidebar(
    title = "Setup",
    width = 300,
    fileInput("file_upload", "Upload Data", accept = c(".xlsx", ".csv"), placeholder = "No file..."),
    selectInput("method", "Method", choices = c("Luy (2012)"="luy", "Brass (1973)"="brass", "Timaeus (1992)"="timaeus")),
    selectInput("sex", "Parent Sex", choices = c("Female", "Male")),
    numericInput("survey_date", "Survey Year", value = 2024.75, step = 0.01),
    selectInput("family", "Model Family", choices = c("UN General"="General", "UN Latin"="Latin", "UN Chilean"="Chilean", "UN South Asian"="South_Asian", "UN Far East"="Far_East_Asian", "CD West"="West", "CD North"="North", "CD East"="East", "CD South"="South")),
    hr(),
    actionButton("run", "Estimate", class = "btn-primary w-100"),
    br(), br(),
    downloadButton("download_template", "Get Template", class = "btn-xs btn-link")
  ),

  # --- CONTEÚDO PRINCIPAL ---
  navset_card_underline(

    # TAB 1: ESTIMATES
    nav_panel("Estimates",
              layout_columns(
                col_widths = c(8, 4),

                # CARD 1: GRÁFICO (Com filtros e Fix Y-Axis)
                card(
                  full_screen = TRUE,
                  card_header(
                    class = "d-flex justify-content-between align-items-center",
                    "Trend Analysis",
                    div(class="d-flex gap-3 align-items-center",

                        # Checkbox para fixar escala
                        div(class="d-flex align-items-center",
                            style="margin-bottom: 0px;",
                            checkboxInput("fix_y_scale", "Fix Y-Axis", value = TRUE)
                        ),

                        # Filtro de Ano (Slider UI dinâmico)
                        div(class="header-slider", style="width: 200px;",
                            uiOutput("ui_year_slider")),

                        # Filtro de Index
                        selectInput("plot_index", NULL,
                                    choices = c("30q30"="30q30", "e30"="e30", "45q15"="45q15"),
                                    selected = "30q30", width = "100px", selectize = FALSE)
                    )
                  ),
                  plotlyOutput("plot_main", height = "500px")
                ), # <--- VÍRGULA IMPORTANTE AQUI (Separa Card 1 do Card 2)

                # CARD 2: TABELA
                card(
                  card_header(class = "d-flex justify-content-between align-items-center",
                              "Estimates Table",
                              downloadButton("download_results", label=NULL, icon=icon("download"), class="btn-discrete", title="Download Excel")),
                  DTOutput("table_results", height = "100%")
                )
              )
    ),

    # TAB 2: DIAGNOSTICS
    nav_panel("Diagnostics",

              # 1. Linearity
              card(
                height = "320px",
                card_header("Internal Consistency (Linearity)"),
                plotOutput("plot_linearity", height = "100%")
              ),

              # 2. BARRA DE CONTROLE ULTRA-COMPACTA
              div(
                class = "metric-bar bg-light border rounded d-flex justify-content-center align-items-center py-1 mb-2",
                span("Analysis Metric:", style="font-weight: bold; margin-right: 15px; font-size: 0.9rem;"),
                radioButtons("sens_metric", label = NULL,
                             choices = c("Prob 30q30" = "30q30",
                                         "Prob 45q15" = "45q15",
                                         "Expectancy e30" = "e30"),
                             selected = "30q30",
                             inline = TRUE)
              ),

              # 3. Sensibilidades (Lado a Lado)
              layout_columns(
                col_widths = c(6, 6),

                # Sensibilidade Mn
                card(
                  height = "400px",
                  card_header(
                    class = "d-flex justify-content-between align-items-center",
                    "Sensitivity: Fertility (Mn)",
                    selectInput("mn_range_val", NULL,
                                choices = c("+/- 0.5y"=0.5, "+/- 1.0y"=1.0, "+/- 1.5y"=1.5, "+/- 2.0y"=2.0, "+/- 3.0y"=3.0),
                                selected = 1.5,
                                selectize = FALSE, width = "100px")
                  ),
                  plotOutput("plot_sens_m", height = "100%")
                ),

                # Sensibilidade Família
                card(
                  height = "400px",
                  card_header(
                    class = "d-flex justify-content-between align-items-center",
                    "Sensitivity: Model Family",
                    selectInput("sens_fam_system", NULL,
                                choices=c("vs UN"="UN", "vs CD"="CD", "vs All"="All"),
                                selectize=FALSE, width = "90px")
                  ),
                  plotOutput("plot_sens_fam", height = "100%")
                )
              )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  # --- Template ---
  output$download_template <- downloadHandler(
    filename = function() { "input_template.xlsx" },
    content = function(file) { writexl::write_xlsx(data.frame(Age=seq(15,60,5), Sn=c(0.95772787,0.94418605,0.89402174,0.84395199,0.77974435,0.67717391,0.49225268,0.33670034,0.20071685,0.09517426), Mn=rep(24,10)), file) }
  )

  # --- Read Data ---
  data_reactive <- reactive({
    req(input$file_upload)
    tryCatch({
      ext <- tools::file_ext(input$file_upload$name)
      df <- if(ext == "csv") read.csv(input$file_upload$datapath) else read_excel(input$file_upload$datapath)
      names(df) <- tolower(names(df))
      if(!all(c("age", "sn", "mn") %in% names(df))) stop("Missing columns: age, sn, mn")
      df
    }, error = function(e) { showNotification(e$message, type="error"); NULL })
  })

  # --- Run Estimation ---
  est_reactive <- eventReactive(input$run, {
    req(data_reactive())
    tryCatch({
      HOME::om_estimate_index(
        method = input$method, sex_parent = input$sex, age_respondent = data_reactive()$age,
        p_surv = data_reactive()$sn, mean_age_parent = data_reactive()$mn, surv_date = input$survey_date, model_family = input$family
      )
    }, error = function(e) { showNotification(e$message, type="error"); NULL })
  })

  # --- Theme Helper ---
  my_theme <- theme_bw(base_size = 11) +
    theme(panel.grid.minor = element_blank(), plot.title = element_blank(), axis.text = element_text(color = "black"))

  # --- Dynamic UI for Slider ---
  output$ui_year_slider <- renderUI({
    req(est_reactive())
    raw_df <- est_reactive()$estimates
    valid_years <- na.omit(raw_df$RefTime)
    if(length(valid_years) == 0) return(NULL)

    min_y <- floor(min(valid_years))
    max_y <- ceiling(max(valid_years))

    sliderInput("plot_year_range", NULL, min = min_y, max = max_y, value = c(min_y, max_y),
                sep = "", step = 1, ticks = FALSE)
  })

  # --- Outputs: Main Tab (Plot) ---
  output$plot_main <- renderPlotly({
    req(est_reactive())
    raw_df <- est_reactive()$estimates

    # Validação inicial para evitar erros de 'order' se df estiver vazio
    if (is.null(raw_df) || nrow(raw_df) == 0) return(NULL)
    if (!input$plot_index %in% names(raw_df)) return(NULL)

    # 1. Filtro de NAs e Seleção da Série Completa (para limites globais)
    full_series <- raw_df[!is.na(raw_df$RefTime) & !is.na(raw_df[[input$plot_index]]), ]
    if(nrow(full_series) == 0) return(NULL)

    # 2. Calcular Limites Globais (Global Min/Max) se Fix Axis estiver ativo
    y_limits <- NULL
    if (isTRUE(input$fix_y_scale)) {
      y_vals <- full_series[[input$plot_index]]
      rng <- range(y_vals, na.rm = TRUE)
      margin <- if(diff(rng) == 0) 0.01 else diff(rng) * 0.05
      y_limits <- c(rng[1] - margin, rng[2] + margin)
    }

    # 3. Filtro de Ano (Slider) - Aplicado APÓS calcular os limites
    df <- full_series
    if (!is.null(input$plot_year_range)) {
      df <- df[df$RefTime >= input$plot_year_range[1] & df$RefTime <= input$plot_year_range[2], ]
    }

    # Se o filtro de ano resultar em 0 linhas, não plota
    if(nrow(df) == 0) return(NULL)

    # Labels dinâmicos
    y_label <- switch(input$plot_index, "30q30"="Prob 30q30", "e30"="Life Expectancy (e30)", "45q15"="Prob 45q15")

    # Plot
    p <- ggplot(df, aes(x = RefTime, y = .data[[input$plot_index]], group = 1,
                        text = paste0("<b>Year:</b> ", round(RefTime, 1),
                                      "<br><b>", input$plot_index, ":</b> ", round(.data[[input$plot_index]], 4),
                                      "<br><b>Resp Age:</b> ", RespondentAge))) +
      geom_line(color = "#0d6efd", linewidth = 0.8) +
      geom_point(size = 2, color = "#0d6efd") +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      labs(y = y_label, x = "Reference Year") +
      my_theme

    # Aplica os limites fixos se solicitado
    if (!is.null(y_limits)) {
      p <- p + coord_cartesian(ylim = y_limits)
    }

    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })

  # --- Outputs: Main Tab (Table) ---
  output$table_results <- renderDT({
    req(est_reactive())
    cols_show <- c("RespondentAge", "RefTime", "30q30", "e30", "45q15", "Alpha")
    cols_exist <- intersect(cols_show, names(est_reactive()$estimates))

    datatable(est_reactive()$estimates[, cols_exist, drop=FALSE],
              rownames = FALSE,
              options = list(dom = 't', paging = FALSE, scrollY = "430px", scrollCollapse = TRUE, ordering = FALSE),
              class = "compact cell-border stripe"
    ) %>%
      formatRound("RefTime", 2) %>%
      formatRound("e30", 2) %>%
      formatRound(c("30q30", "45q15", "Alpha"), 4)
  })

  output$download_results <- downloadHandler(
    filename = function() { "HOME_Results.xlsx" },
    content = function(file) { writexl::write_xlsx(est_reactive()$estimates, file) }
  )

  # --- Outputs: Diagnostics Tab ---

  # Linearity
  output$plot_linearity <- renderPlot({
    req(est_reactive())
    suppressWarnings(print(HOME::om_plot_linearity(est_reactive()) + my_theme))
  })

  # Sens M
  output$plot_sens_m <- renderPlot({
    req(est_reactive())
    metric <- input$sens_metric
    range_val <- as.numeric(input$mn_range_val)
    y_lbl <- switch(metric, "30q30"="Prob 30q30", "e30"="Life Expectancy (e30)", "45q15"="Prob 45q15")

    suppressWarnings({
      sens <- HOME::om_sensitivity(est_reactive(), range_m = seq(-range_val, range_val, 0.5))
      print(plot(sens, index = metric) + my_theme + labs(y = y_lbl))
    })
  })

  # Sens Family
  output$plot_sens_fam <- renderPlot({
    req(est_reactive())
    metric <- input$sens_metric
    y_lbl <- switch(metric, "30q30"="Prob 30q30", "e30"="Life Expectancy (e30)", "45q15"="Prob 45q15")

    suppressWarnings({
      sens <- HOME::om_sensitivity_family(est_reactive(), type = input$sens_fam_system)
      print(plot(sens, index = metric) + my_theme + labs(y = y_lbl))
    })
  })
}

shinyApp(ui, server)
