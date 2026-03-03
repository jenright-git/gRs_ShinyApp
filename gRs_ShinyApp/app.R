# install.packages("devtools")
#devtools::install_github("jenright-git/gRs")
library(shiny)
library(bslib)
library(gRs)
library(tidyverse)
library(glue)
library(plotly)
library(thematic)
library(bsicons)
library(openxlsx)

thematic::thematic_on()

options(shiny.maxRequestSize = 30 * 1024^2)

# Define UI for application that draws a histogram
ui <- page_navbar(
  title = "gRs Analysis Tool",
  theme = bs_theme(
    bg = "#ffffff",
    fg = "#00353E",
    primary = "#00353E",
    font_scale = NULL,
    preset = "bootstrap",
    secondary = "#008745",
    success = "#008745",
    ### "body-bg"="#00353A10"
  ),
  # MK results table styling ─────────────────────────────────────────────
  tags$head(tags$style(HTML(
    "
    #mann_kendall_table thead tr:first-child th {
      background-color: #008768 !important;
      color:            #FFFFFF !important;
      font-weight:      bold    !important;
      font-family:      Arial, sans-serif !important;
      border:           1px solid #008768 !important;
    }
    #mann_kendall_table tbody td {
      border:      1px solid #008768 !important;
      font-family: Arial, sans-serif !important;
      font-size:   11px !important;
      text-align:  center !important;
    }
    #mk_table_switch {
      width:        18px !important;
      height:       18px !important;
      cursor:       pointer;
      margin-right: 6px;
    }
    .shiny-input-container:has(#mk_table_switch) label {
      font-size:      13px !important;
      font-weight:    600  !important;
      cursor:         pointer;
      vertical-align: middle;
    }
  "
  ))),
  # #Main Page
  nav_panel(
    "Mann-Kendall",
    page_sidebar(
      sidebar = accordion(
        accordion_panel(
          "Data_Upload",
          shiny::fileInput(
            inputId = "file_input",
            label = "Upload Esdat File",
            accept = ".xlsx"
          ),
          shiny::checkboxInput(
            inputId = "half_lor",
            label = "Use Half LOR?",
            value = FALSE
          )
        ),

        accordion_panel(
          "Mann-Kendall Controls",
          input_task_button(id = "mk_button", label = "Run Trend Analysis"),
          uiOutput(outputId = "analyte_selector"),
          uiOutput(outputId = "location_selector")
        ),

        accordion_panel(
          "Heatmap Controls",
          splitLayout(
            numericInput(
              "mk_trend_label",
              "Label Size",
              value = 6,
              min = 1,
              width = '70%'
            ),
            numericInput(
              "mk_label_width",
              "Label Width",
              value = 20,
              min = 0,
              width = '70%'
            )
          ),
          splitLayout(
            numericInput(
              "mk_x_text",
              "X Label Size",
              12,
              min = 1,
              width = '70%'
            ),
            numericInput(
              "mk_y_text",
              "Y Label Size",
              12,
              min = 1,
              width = '70%'
            )
          ),
          textInput(
            "mk_title",
            label = "Title",
            value = "Mann-Kendall Trend Analysis"
          ),
          splitLayout(
            numericInput(
              "mk_title_size",
              "Title Size",
              value = 10,
              width = '70%'
            ),
            numericInput(
              "mk_legend_text",
              "Legend Text Size",
              value = 10,
              min = 0,
              width = '70%'
            )
          )
        ),
        open = c("Data_Upload", "Mann-Kendall Controls")
      ),
      navset_card_tab(
        nav_panel(
          "Results Table",
          card(
            card_header("Table Options"),
            tags$div(
              style = "display:flex; align-items:center; gap:12px; padding:8px 16px 6px 16px;",
              tags$div(
                style = paste(
                  "display:inline-block;",
                  "border:2px solid #008768;",
                  "border-radius:6px;",
                  "padding:5px 14px;",
                  "background-color:#f0faf6;"
                ),
                tags$p(
                  style = paste(
                    "font-weight:bold;",
                    "font-size:11px;",
                    "color:#008768;",
                    "margin:0 0 4px 0;",
                    "text-transform:uppercase;",
                    "letter-spacing:0.5px;"
                  ),
                  "Table View"
                ),
                checkboxInput(
                  inputId = "mk_table_switch",
                  label = "Show Test Statistics",
                  value = FALSE
                )
              ),
              downloadButton(
                "download_mk_excel",
                label = "Download Excel",
                icon = shiny::icon("file-excel"),
                class = "btn-sm"
              )
            ),
            DT::dataTableOutput("mann_kendall_table"),
            full_screen = TRUE
          )
        ),
        nav_panel(
          "Trend Heatmap",
          card(
            plotlyOutput("mann_kendall_heatmap"),
            full_screen = TRUE
          )
        ),
        nav_panel(
          "Increasing Trends",
          card(layout_sidebar(
            sidebar = sidebar(
              uiOutput(
                outputId = "trend_select"
              ),
              radioButtons(
                inputId = "trend_method",
                label = "Trend Method",
                choiceValues = c("loess", "lm"),
                selected = "loess",
                choiceNames = c("Smoothed", "Linear")
              ),
              open = TRUE
            ),
            plotlyOutput("mk_increasing"),
            full_screen = TRUE
          ))
        )
      )
    )
  ),

  nav_panel(
    "Timeseries",
    page_sidebar(
      sidebar = accordion(
        accordion_panel(
          "Filters",
          open = TRUE,
          uiOutput(outputId = "plotting_analytes"),
          input_task_button("update_plot_locations", "Update Locations"),
          uiOutput(outputId = "plotting_locations"),
          uiOutput(outputId = "plotting_date")
        ),
        accordion_panel(
          "Plotting Controls",
          # ── X-axis date break ──────────────────────────────────────────
          tags$p(
            "X-Axis Date Breaks",
            style = "font-weight:600; font-size:12px; margin:0 0 4px 0;"
          ),
          splitLayout(
            cellWidths = c("45%", "55%"),
            numericInput(
              inputId = "date_break_n",
              label = "Interval",
              value = 3,
              min = 1,
              step = 1,
              width = "100%"
            ),
            selectInput(
              inputId = "date_break_unit",
              label = "Unit",
              choices = c("days", "weeks", "months", "years"),
              selected = "months",
              width = "100%"
            )
          ),
          # ── Date label format ──────────────────────────────────────────
          selectInput(
            inputId = "date_label_radio",
            label = "Date Label Format",
            choices = c(
              "01 Jan 25" = "%d %b %y",
              "Jan 25" = "%b %y",
              "January 25" = "%B %y",
              "Jan 2025" = "%b %Y",
              "January 2025" = "%B %Y",
              "2025" = "%Y",
              "Q1 2025" = "%b %Y",
              "01/01/2025" = "%d/%m/%Y",
              "01-01-2025" = "%d-%m-%Y"
            ),
            selected = "%b %y",
            width = "100%"
          ),
          # ── Label appearance ───────────────────────────────────────────
          splitLayout(
            cellWidths = c("50%", "50%"),
            numericInput(
              "ts_date_size",
              "X Label Size",
              10,
              min = 0,
              width = "100%"
            ),
            numericInput(
              "ts_x_angle",
              "X Label Angle",
              0,
              min = 0,
              max = 360,
              width = "100%"
            )
          ),
          splitLayout(
            cellWidths = c("50%", "50%"),
            numericInput(
              inputId = "ts_y_label_size",
              label = "Y Tick Label Size",
              value = 10,
              min = 6,
              max = 30,
              step = 1,
              width = "100%"
            ),
            numericInput(
              inputId = "ts_y_title_size",
              label = "Y Axis Title Size",
              value = 12,
              min = 6,
              max = 30,
              step = 1,
              width = "100%"
            )
          ),
          # ── Y-axis limits ──────────────────────────────────────────────
          splitLayout(
            cellWidths = c("50%", "50%"),
            numericInput("min_conc", "Min Y", 0, width = "100%"),
            numericInput("max_conc", "Max Y", NA, width = "100%")
          ),
          # ── Line appearance ────────────────────────────────────────────
          tags$p(
            "Line Appearance",
            style = "font-weight:600; font-size:12px; margin:8px 0 4px 0;"
          ),
          checkboxGroupInput(
            inputId = "ts_geom_type",
            label = "Plot Type",
            choices = c(
              "Line" = "line",
              "Smooth" = "smooth",
              "Regression" = "regression"
            ),
            selected = "line",
            inline = TRUE
          ),
          checkboxInput(
            inputId = "ts_show_points",
            label = "Show Points",
            value = TRUE
          ),
          splitLayout(
            cellWidths = c("50%", "50%"),
            numericInput(
              inputId = "ts_linewidth",
              label = "Line Width",
              value = 0.8,
              min = 0.1,
              max = 5,
              step = 0.1,
              width = "100%"
            ),
            numericInput(
              inputId = "ts_pointsize",
              label = "Point Size",
              value = 1.2,
              min = 0.1,
              max = 10,
              step = 0.1,
              width = "100%"
            )
          ),
          selectInput(
            inputId = "ts_colour_theme",
            label = "Colour Theme",
            choices = c(
              "AECOM (Default)" = "aecom",
              "Viridis" = "viridis",
              "Plasma" = "plasma",
              "Brewer Set1" = "Set1",
              "Brewer Set2" = "Set2",
              "Brewer Dark2" = "Dark2",
              "Brewer Paired" = "Paired",
              "Brewer Spectral" = "Spectral"
            ),
            selected = "aecom",
            width = "100%"
          ),
          # ── Legend position ────────────────────────────────────────────
          radioButtons(
            inputId = "ts_legend_pos",
            label = "Legend Position",
            choices = c("Bottom" = "bottom", "Right" = "right"),
            selected = "bottom",
            inline = TRUE
          )
        ),
        accordion_panel(
          "Criteria",
          # ── Criteria 1 ───────────────────────────────────────────────────────
          tags$p(
            "Criteria 1",
            style = "font-weight:600; font-size:12px; margin:0 0 4px 0;"
          ),
          checkboxInput(
            "criteria_check",
            label = "Apply Criteria 1",
            value = FALSE
          ),
          textInput(
            "criteria_label",
            label = "Label",
            placeholder = "Enter label",
            value = NULL
          ),
          splitLayout(
            numericInput(
              "criteria_value",
              label = "Value",
              value = 1,
              min = 0
            ),
            textInput(
              inputId = "criteria_colour",
              label = "Colour",
              value = "Red"
            )
          ),
          # ── Criteria 2 ───────────────────────────────────────────────────────
          tags$hr(style = "margin:8px 0;"),
          tags$p(
            "Criteria 2",
            style = "font-weight:600; font-size:12px; margin:0 0 4px 0;"
          ),
          checkboxInput(
            "criteria2_check",
            label = "Apply Criteria 2",
            value = FALSE
          ),
          textInput(
            "criteria2_label",
            label = "Label",
            placeholder = "Enter label",
            value = NULL
          ),
          splitLayout(
            numericInput(
              "criteria2_value",
              label = "Value",
              value = 1,
              min = 0
            ),
            textInput(
              inputId = "criteria2_colour",
              label = "Colour",
              value = "Blue"
            )
          )
        ),
        accordion_panel(
          "Download Plot",
          tags$p(
            "Image Dimensions",
            style = "font-weight:600; font-size:12px; margin:0 0 4px 0;"
          ),
          splitLayout(
            cellWidths = c("50%", "50%"),
            numericInput(
              inputId = "ts_width_cm",
              label = "Width (cm)",
              value = 30,
              min = 5,
              step = 0.5,
              width = "100%"
            ),
            numericInput(
              inputId = "ts_height_cm",
              label = "Height (cm)",
              value = 14.85,
              min = 5,
              step = 0.5,
              width = "100%"
            )
          ),
          numericInput(
            inputId = "ts_dpi",
            label = "Resolution (DPI)",
            value = 300,
            min = 72,
            max = 600,
            step = 50,
            width = "100%"
          ),
          tags$div(
            style = "margin-top:8px;",
            downloadButton(
              outputId = "download_ts_png",
              label = "Download PNG",
              icon = shiny::icon("image"),
              class = "btn-sm w-100"
            )
          )
        )
      ),

      layout_columns(
        col_widths = c(12, 6, 6),
        card(
          card_header("Timeseries Plot"),
          plotOutput("timeseries_plot"),
          full_screen = TRUE
        ),
        card(
          card_header("Histogram"),
          layout_sidebar(
            sidebar = sidebar(
              numericInput(
                inputId = "bin_selector",
                label = "Select Bin Width",
                value = 0
              ),
              checkboxInput(
                inputId = "facet_check",
                label = "Facet by Location",
                value = FALSE
              ),
              tags$hr(style = "margin:8px 0;"),
              tags$p(
                "Download",
                style = "font-weight:600; font-size:12px; margin:0 0 4px 0;"
              ),
              splitLayout(
                cellWidths = c("50%", "50%"),
                numericInput(
                  "hist_width_cm",
                  "W (cm)",
                  value = 30,
                  min = 5,
                  step = 0.5,
                  width = "100%"
                ),
                numericInput(
                  "hist_height_cm",
                  "H (cm)",
                  value = 14.85,
                  min = 5,
                  step = 0.5,
                  width = "100%"
                )
              ),
              numericInput(
                "hist_dpi",
                "DPI",
                value = 300,
                min = 72,
                max = 600,
                step = 50,
                width = "100%"
              ),
              downloadButton(
                "download_hist_png",
                "Download PNG",
                icon = shiny::icon("image"),
                class = "btn-sm w-100"
              ),
              open = FALSE
            ),
            plotOutput("conc_histogram")
          ),
          full_screen = TRUE
        ),

        card(
          card_header("Boxplot"),
          layout_sidebar(
            sidebar = sidebar(
              tags$p(
                "Download",
                style = "font-weight:600; font-size:12px; margin:0 0 4px 0;"
              ),
              splitLayout(
                cellWidths = c("50%", "50%"),
                numericInput(
                  "boxplot_width_cm",
                  "W (cm)",
                  value = 30,
                  min = 5,
                  step = 0.5,
                  width = "100%"
                ),
                numericInput(
                  "boxplot_height_cm",
                  "H (cm)",
                  value = 14.85,
                  min = 5,
                  step = 0.5,
                  width = "100%"
                )
              ),
              numericInput(
                "boxplot_dpi",
                "DPI",
                value = 300,
                min = 72,
                max = 600,
                step = 50,
                width = "100%"
              ),
              downloadButton(
                "download_boxplot_png",
                "Download PNG",
                icon = shiny::icon("image"),
                class = "btn-sm w-100"
              ),
              open = FALSE
            ),
            plotOutput("conc_boxplot")
          ),
          full_screen = TRUE
        )
      )
    )
  ),

  nav_panel(
    title = "Summary Stats",
    card(
      card_header("Stats"),
      DT::dataTableOutput("stats_table"),
      full_screen = TRUE
    )
  ),

  nav_panel(
    title = "Facet Plot",
    page_sidebar(
      sidebar = accordion(
        accordion_panel(
          "Filters",
          open = TRUE,
          input_task_button("update_facet_locations", "Update Plots"),
          uiOutput(outputId = "facet_analytes"),
          uiOutput(outputId = "facet_locations")
        )
      ),
      card(
        plotlyOutput("facet_plot")
      )
    )
  )
)


# Define server logic required to make visualisations
server <- function(input, output) {
  mk_trend_colors <- c(
    "Increasing" = "#FF0000",
    "Probably Increasing" = "#ff6700",
    "Stable" = "#D9D9D9",
    "No Significant Trend" = "#BFBFBF",
    "Probably Decreasing" = "#92D050",
    "Decreasing" = "#00B050"
  )

  # Shared header / border colour for DT and Excel export
  mk_header_col <- "#008768"

  file_data <- reactive({
    file <- input$file_input

    lor_check <- input$half_lor

    if (!is.null(file) & lor_check) {
      data_processor(file$datapath) %>% half_lor()
    } else if (!is.null(file) & lor_check == FALSE) {
      data_processor(file$datapath)
    }
  })

  mk_results <- eventReactive(input$mk_button, {
    file_data() %>%
      filter(
        chem_name %in% input$analyte_input,
        location_code %in% input$location_input
      ) %>%
      mutate(
        location_code = factor(location_code, levels = input$location_input),
        chem_name = factor(chem_name, levels = input$analyte_input)
      ) %>%
      mann_kendall_test()
  })

  output$mann_kendall_heatmap <- renderPlotly({
    req(mk_results())

    heatmap <- mk_results() %>%
      mann_kendall_heatmap(
        label_text_size = input$mk_trend_label,
        width = input$mk_label_width,
        plot_title = input$mk_title
      ) +
      theme(
        axis.text.y = element_text(size = input$mk_y_text),
        axis.text.x = element_text(size = input$mk_x_text),
        legend.text = element_text(size = input$mk_legend_text),
        title = element_text(size = input$mk_title_size, face = "bold")
      ) +
      labs(fill = "Trend")

    plotly::ggplotly(heatmap)
  })

  output$mann_kendall_table <- DT::renderDataTable({
    req(mk_results())

    if (!input$mk_table_switch) {
      wide_tbl <- mk_results() %>%
        select(location_code, chem_name, trend) %>%
        arrange(location_code, chem_name) %>%
        pivot_wider(names_from = chem_name, values_from = trend)

      analyte_cols <- setdiff(names(wide_tbl), "location_code")

      wide_tbl %>%
        DT::datatable(
          .,
          extensions = "Buttons",
          filter = list(position = "top")
        ) %>%
        DT::formatStyle(
          columns = analyte_cols,
          color = "black",
          backgroundColor = DT::styleEqual(
            names(mk_trend_colors),
            unname(mk_trend_colors),
            default = NULL
          )
        )
    } else {
      mk_results() %>%
        select(-data) %>%
        arrange(location_code) %>%
        mutate_if(is.numeric, signif, 4) %>%
        DT::datatable(
          .,
          extensions = "Buttons",
          filter = list(position = "top")
        ) %>%
        DT::formatStyle(
          columns = "trend",
          color = "black",
          backgroundColor = DT::styleEqual(
            names(mk_trend_colors),
            unname(mk_trend_colors),
            default = NULL
          )
        )
    }
  })

  output$mk_increasing <- renderPlotly({
    req(mk_results())

    increasing_p <- mk_results() %>%
      filter(trend == input$trend_select) %>%
      unnest(data) %>%
      mutate(chem_name = glue('{chem_name} ({output_unit})')) %>%
      ggplot(aes(date, concentration, colour = location_code)) +
      geom_point(alpha = 0.6, size = 1.2) +
      facet_wrap(~chem_name, scales = "free_y") +
      geom_smooth(se = F, method = input$trend_method) +
      theme_light() +
      labs(x = NULL, y = "Concentration", colour = NULL) +
      theme(
        strip.background = element_rect(fill = NA, colour = "black"),
        strip.text = element_text(colour = "black")
      )

    plotly::ggplotly(increasing_p, dynamicTicks = T)
  })

  output$analyte_selector <- renderUI({
    req(file_data())
    selectInput(
      "analyte_input",
      label = "Select and Order Analytes",
      choices = file_data() %>% distinct(chem_name),
      selected = file_data()$chem_name %>% unique(),
      multiple = TRUE
    )
  })

  output$location_selector <- renderUI({
    req(file_data())
    selectInput(
      "location_input",
      label = "Select and Order Locations",
      choices = file_data() %>%
        arrange(chem_group) %>%
        distinct(location_code),
      selected = file_data()$location_code %>% unique(),
      multiple = TRUE
    )
  })

  output$trend_select <- renderUI({
    require(mk_results())

    radioButtons(
      inputId = "trend_select",
      label = "Show Trend",
      choices = c(
        "Increasing",
        "Probably Increasing",
        "Stable",
        "No Significant Trend",
        "Probably Decreasing",
        "Decreasing"
      ),
      selected = "Increasing"
    )
  })

  output$plotting_analytes <- renderUI({
    req(file_data())
    selectInput(
      "plotting_analytes",
      label = "Select Analytes",
      choices = file_data() %>% distinct(chem_name),
      multiple = FALSE
    )
  })

  output$plotting_locations <- renderUI({
    req(file_data())
    selectInput(
      "plotting_locations",
      label = "Select Locations",
      choices = file_data() %>% distinct(location_code),
      selected = file_data()$location_code %>% unique(),
      multiple = TRUE
    )
  })

  output$plotting_date <- renderUI({
    req(file_data())
    shiny::dateRangeInput(
      inputId = "plotting_date",
      label = "Select Start and End Date",
      start = as.Date(min(file_data()$date)),
      end = as.Date(max(file_data()$date)),
      format = "dd-mm-yyyy"
    )
  })

  plotting_data <- eventReactive(input$update_plot_locations, {
    file_data() %>%
      filter(location_code %in% input$plotting_locations)
  })

  # Shared reactive — builds the ggplot2 timeseries object used by both
  # the on-screen render and the PNG download handler.
  # Built directly with ggplot2 so linewidth/point size are fully controllable.
  ts_plot_obj <- reactive({
    req(plotting_data())

    establish_plotting_variables(data = file_data())

    date_range_plot <- input$plotting_date

    df <- plotting_data() %>%
      filter(
        chem_name %in% input$plotting_analytes,
        date >= date_range_plot[1] & date <= date_range_plot[2]
      )

    y_unit <- unique(df$output_unit)
    y_limits <- c(
      input$min_conc,
      if (is.na(input$max_conc)) NA_real_ else input$max_conc
    )
    lw <- input$ts_linewidth
    x_hjust <- if (input$ts_x_angle > 0) 1 else 0.5

    # ── Geom layers (conditional on plot type and points toggle) ─────────────
    geom_layers <- list()
    if ("line" %in% input$ts_geom_type) {
      geom_layers <- c(geom_layers, list(ggplot2::geom_path(linewidth = lw)))
    }
    if ("smooth" %in% input$ts_geom_type) {
      geom_layers <- c(
        geom_layers,
        list(ggplot2::geom_smooth(se = FALSE, linewidth = lw))
      )
    }
    if ("regression" %in% input$ts_geom_type) {
      geom_layers <- c(
        geom_layers,
        list(ggplot2::geom_smooth(method = "lm", se = FALSE, linewidth = lw))
      )
    }
    if (input$ts_show_points) {
      geom_layers <- c(
        geom_layers,
        list(ggplot2::geom_point(size = input$ts_pointsize, alpha = 0.5))
      )
    }

    # ── Base plot ────────────────────────────────────────────────────────────
    plot <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = date, y = concentration, colour = location_code)
    ) +
      geom_layers +
      ggplot2::theme_light() +
      ggplot2::labs(
        x = NULL,
        y = glue("Concentration ({y_unit})"),
        colour = NULL
      ) +
      ggplot2::scale_x_datetime(
        date_breaks = paste(input$date_break_n, input$date_break_unit),
        date_labels = input$date_label_radio,
        limits = as.POSIXct(date_range_plot)
      ) +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        legend.position = input$ts_legend_pos,
        legend.text = ggplot2::element_text(colour = "black"),
        axis.text.x = ggplot2::element_text(
          angle = input$ts_x_angle,
          size = input$ts_date_size,
          hjust = x_hjust,
          colour = "black"
        ),
        axis.text.y = ggplot2::element_text(
          size = input$ts_y_label_size,
          colour = "black"
        ),
        axis.title.y = ggplot2::element_text(
          size = input$ts_y_title_size,
          colour = "black"
        ),
        strip.background = ggplot2::element_rect(fill = NA, colour = "black"),
        strip.text = ggplot2::element_text(colour = "black")
      )

    # ── Colour scale ─────────────────────────────────────────────────────────
    plot <- plot +
      switch(
        input$ts_colour_theme,
        "aecom" = ggplot2::scale_colour_manual(values = location_colours),
        "viridis" = ggplot2::scale_colour_viridis_d(option = "viridis"),
        "plasma" = ggplot2::scale_colour_viridis_d(option = "plasma"),
        ggplot2::scale_colour_brewer(palette = input$ts_colour_theme)
      )

    # ── Y-axis limits ────────────────────────────────────────────────────────
    # Always apply user-specified min/max; NA upper limit lets ggplot2 expand.
    plot <- plot + ggplot2::scale_y_continuous(limits = y_limits)

    # ── Criteria lines ───────────────────────────────────────────────────────
    # Resolve display labels (fall back if the user left the field blank)
    crit1_label <- if (nchar(trimws(input$criteria_label))  > 0) input$criteria_label  else "Criteria 1"
    crit2_label <- if (nchar(trimws(input$criteria2_label)) > 0) input$criteria2_label else "Criteria 2"

    # Map criteria via the LINETYPE aesthetic — a completely separate scale
    # from the location COLOUR scale — so existing location legend entries are
    # never touched.  guide_legend(override.aes) forces each key glyph to
    # display the correct criteria colour even though colour is a fixed param.
    lt_values  <- c()
    lt_colours <- c()

    if (input$criteria_check) {
      plot <- plot +
        ggplot2::geom_hline(
          data = data.frame(yint = input$criteria_value, lbl = crit1_label),
          ggplot2::aes(yintercept = yint, linetype = lbl),
          colour      = input$criteria_colour,
          linewidth   = 0.7,
          show.legend = TRUE
        )
      lt_values[crit1_label] <- "dashed"
      lt_colours             <- c(lt_colours, input$criteria_colour)
    }

    if (input$criteria2_check) {
      plot <- plot +
        ggplot2::geom_hline(
          data = data.frame(yint = input$criteria2_value, lbl = crit2_label),
          ggplot2::aes(yintercept = yint, linetype = lbl),
          colour      = input$criteria2_colour,
          linewidth   = 0.7,
          show.legend = TRUE
        )
      lt_values[crit2_label] <- "dashed"
      lt_colours             <- c(lt_colours, input$criteria2_colour)
    }

    if (input$criteria_check || input$criteria2_check) {
      plot <- plot +
        ggplot2::scale_linetype_manual(
          values = lt_values,
          name   = NULL,
          guide  = ggplot2::guide_legend(
            override.aes = list(colour = lt_colours, linewidth = 0.7)
          )
        )
    }

    plot
  })

  output$timeseries_plot <- renderPlot({
    ts_plot_obj()
  })

  # Download PNG — default size: half portrait A4 (21 × 14.85 cm)
  output$download_ts_png <- downloadHandler(
    filename = function() {
      paste0("timeseries_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggplot2::ggsave(
        filename = file,
        plot = ts_plot_obj(),
        width = input$ts_width_cm,
        height = input$ts_height_cm,
        units = "cm",
        dpi = input$ts_dpi,
        device = "png"
      )
    }
  )

  hist_plot_obj <- reactive({
    req(plotting_data())

    date_range <- input$plotting_date

    hist_data <- plotting_data() %>%
      filter(
        chem_name %in% input$plotting_analytes,
        date >= date_range[1] & date <= date_range[2]
      )

    y_unit   <- unique(hist_data$output_unit)
    x_hjust  <- if (input$ts_x_angle > 0) 1 else 0.5

    black_text <- ggplot2::theme(
      axis.text.x  = ggplot2::element_text(
        colour = "black",
        size   = input$ts_date_size,
        angle  = input$ts_x_angle,
        hjust  = x_hjust
      ),
      axis.text.y  = ggplot2::element_text(
        colour = "black",
        size   = input$ts_y_label_size
      ),
      axis.title.x = ggplot2::element_text(colour = "black"),
      axis.title.y = ggplot2::element_text(
        colour = "black",
        size   = input$ts_y_title_size
      ),
      strip.background = ggplot2::element_rect(fill = NA, colour = "black"),
      strip.text       = ggplot2::element_text(colour = "black")
    )

    if (input$bin_selector != 0) {
      hist_plot <- ggplot2::ggplot(hist_data, ggplot2::aes(concentration)) +
        ggplot2::geom_histogram(
          fill = "steelblue1",
          colour = "black",
          binwidth = input$bin_selector
        ) +
        ggplot2::theme_light() +
        black_text +
        ggplot2::labs(x = glue("Concentration ({y_unit})"), y = "Count")
    } else {
      breaks <- pretty(
        range(hist_data$concentration),
        n = nclass.Sturges(hist_data$concentration),
        min.n = 1
      )
      hist_plot <- ggplot2::ggplot(hist_data, ggplot2::aes(concentration)) +
        ggplot2::geom_histogram(
          fill = "steelblue1",
          colour = "black",
          breaks = breaks
        ) +
        ggplot2::theme_light() +
        black_text +
        ggplot2::labs(x = glue("Concentration ({y_unit})"), y = "Count")
    }

    if (input$facet_check) {
      hist_plot <- hist_plot + ggplot2::facet_wrap(~location_code)
    }

    hist_plot
  })

  output$conc_histogram <- renderPlot({
    hist_plot_obj()
  })

  output$download_hist_png <- downloadHandler(
    filename = function() paste0("histogram_", Sys.Date(), ".png"),
    content = function(file) {
      ggplot2::ggsave(
        filename = file,
        plot = hist_plot_obj(),
        width = input$hist_width_cm,
        height = input$hist_height_cm,
        units = "cm",
        dpi = input$hist_dpi,
        device = "png"
      )
    }
  )

  boxplot_obj <- reactive({
    req(plotting_data())

    establish_plotting_variables(data = file_data())

    date_range <- input$plotting_date

    boxplot_data <- plotting_data() %>%
      filter(
        chem_name %in% input$plotting_analytes,
        date >= date_range[1] & date <= date_range[2]
      ) %>%
      mutate(
        location_code = fct_reorder(
          location_code,
          concentration,
          .fun = median,
          .desc = TRUE
        )
      )

    y_unit  <- unique(boxplot_data$output_unit)
    x_hjust <- if (input$ts_x_angle > 0) 1 else 0.5

    set.seed(1994)
    bplot <- ggplot2::ggplot(
      boxplot_data,
      ggplot2::aes(location_code, concentration, fill = location_code)
    ) +
      ggplot2::geom_jitter(
        shape = 21,
        alpha = 0.6,
        show.legend = FALSE,
        size = 1.2,
        height = 0
      ) +
      ggplot2::geom_boxplot(
        alpha = 0.4,
        outlier.shape = NA,
        show.legend = FALSE
      ) +
      ggplot2::theme_light() +
      ggplot2::labs(x = NULL, y = glue("Concentration ({y_unit})")) +
      ggplot2::theme(
        axis.text.x  = ggplot2::element_text(
          colour = "black",
          size   = input$ts_date_size,
          angle  = input$ts_x_angle,
          hjust  = x_hjust
        ),
        axis.text.y  = ggplot2::element_text(
          colour = "black",
          size   = input$ts_y_label_size
        ),
        axis.title.y = ggplot2::element_text(
          colour = "black",
          size   = input$ts_y_title_size
        ),
        strip.background = ggplot2::element_rect(fill = NA, colour = "black"),
        strip.text       = ggplot2::element_text(colour = "black"),
        legend.position  = input$ts_legend_pos,
        legend.text      = ggplot2::element_text(colour = "black"),
        legend.title     = ggplot2::element_blank()
      )

    # Apply the same colour theme as the timeseries (fill scale for boxplot)
    bplot <- bplot +
      switch(
        input$ts_colour_theme,
        "aecom" = ggplot2::scale_fill_manual(values = location_colours, guide = "none"),
        "viridis" = ggplot2::scale_fill_viridis_d(option = "viridis", guide = "none"),
        "plasma" = ggplot2::scale_fill_viridis_d(option = "plasma", guide = "none"),
        ggplot2::scale_fill_brewer(palette = input$ts_colour_theme, guide = "none")
      )

    # ── Criteria lines with legend entries ───────────────────────────────────
    # Resolve display labels (fall back if the user left the field blank)
    crit1_label <- if (nchar(trimws(input$criteria_label)) > 0) {
      input$criteria_label
    } else {
      "Criteria 1"
    }
    crit2_label <- if (nchar(trimws(input$criteria2_label)) > 0) {
      input$criteria2_label
    } else {
      "Criteria 2"
    }

    # Supply a one-row data.frame per criteria line so that yintercept is a
    # proper aes() mapping. This is the most reliable way to get ggplot2 to
    # generate a legend key whose glyph reflects the dashed linetype and colour.
    if (input$criteria_check) {
      bplot <- bplot +
        ggplot2::geom_hline(
          data = data.frame(yint = input$criteria_value, lbl = crit1_label),
          ggplot2::aes(yintercept = yint, colour = lbl),
          linetype = "dashed",
          linewidth = 0.7,
          show.legend = TRUE
        )
    }
    if (input$criteria2_check) {
      bplot <- bplot +
        ggplot2::geom_hline(
          data = data.frame(yint = input$criteria2_value, lbl = crit2_label),
          ggplot2::aes(yintercept = yint, colour = lbl),
          linetype = "dashed",
          linewidth = 0.7,
          show.legend = TRUE
        )
    }

    # Add a colour scale only for the active criteria lines.
    # scale_fill_* (used by boxes/jitter) and scale_colour_* are independent
    # aesthetics, so there is no conflict between the two scales.
    if (input$criteria_check || input$criteria2_check) {
      crit_colour_scale <- c()
      if (input$criteria_check) {
        crit_colour_scale[crit1_label] <- input$criteria_colour
      }
      if (input$criteria2_check) {
        crit_colour_scale[crit2_label] <- input$criteria2_colour
      }
      bplot <- bplot +
        ggplot2::scale_colour_manual(values = crit_colour_scale, name = NULL)
    }

    bplot
  })

  output$conc_boxplot <- renderPlot({
    boxplot_obj()
  })

  output$download_boxplot_png <- downloadHandler(
    filename = function() paste0("boxplot_", Sys.Date(), ".png"),
    content = function(file) {
      ggplot2::ggsave(
        filename = file,
        plot = boxplot_obj(),
        width = input$boxplot_width_cm,
        height = input$boxplot_height_cm,
        units = "cm",
        dpi = input$boxplot_dpi,
        device = "png"
      )
    }
  )

  output$stats_table <- DT::renderDataTable({
    req(file_data())

    file_data() %>%
      group_by(Location = location_code, Analyte = chem_name) %>%
      summarise(
        n_samples = n(),
        Minimum = min(concentration, na.rm = T),
        Mean = mean(concentration, na.rm = T),
        Maximum = max(concentration, na.rm = -T),
        `5th Percentile` = quantile(concentration, 0.05, na.rm = T),
        `10th Percentile` = quantile(concentration, 0.1, na.rm = T),
        `25th Percentile` = quantile(concentration, 0.25, na.rm = T),
        `50th Percentile` = quantile(concentration, 0.5, na.rm = T),
        `75th Percentile` = quantile(concentration, 0.75, na.rm = T),
        `80th Percentile` = quantile(concentration, 0.80, na.rm = T),
        `85th Percentile` = quantile(concentration, 0.85, na.rm = T),
        `90th Percentile` = quantile(concentration, 0.9, na.rm = T),
        `95th Percentile` = quantile(concentration, 0.95, na.rm = T),
      ) %>%
      mutate_if(is.numeric, signif, 4) %>%
      DT::datatable(
        .,
        extensions = "Buttons",
        filter = list(position = "top"),
        options = list(
          dom = 'Blfrtip',
          buttons = c("copy", "csv", "excel", "pdf", "print"),
          lengthMenu = list(c(-1, 10, 25, 50), c("All", 10, 25, 50))
        )
      )
  })

  facet_data <- eventReactive(input$update_facet_locations, {
    file_data() %>%
      filter(
        location_code %in% input$facet_locations,
        chem_name %in% input$facet_analytes
      )
  })

  output$facet_analytes <- renderUI({
    req(file_data())
    selectInput(
      "facet_analytes",
      label = "Select Analytes",
      choices = file_data() %>% distinct(chem_name),
      selected = file_data()$chem_name %>% unique(), #couldn't be distinct() because not a vector
      multiple = TRUE
    )
  })

  output$facet_locations <- renderUI({
    req(file_data())
    selectInput(
      "facet_locations",
      label = "Select Locations",
      choices = file_data() %>% distinct(location_code),
      selected = file_data()$location_code %>% unique(),
      multiple = TRUE
    )
  })

  output$download_mk_excel <- downloadHandler(
    filename = function() {
      paste0("mk_trends_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(mk_results())

      wb <- openxlsx::createWorkbook()

      # ── Styles ────────────────────────────────────────────────────────────
      header_style <- openxlsx::createStyle(
        fontName = "Arial",
        fontSize = 11,
        fontColour = "#FFFFFF",
        fgFill = mk_header_col,
        halign = "center",
        valign = "center",
        textDecoration = "bold",
        border = "TopBottomLeftRight",
        borderColour = mk_header_col
      )

      data_style <- openxlsx::createStyle(
        fontName = "Arial",
        fontSize = 11,
        fontColour = "#000000",
        halign = "center",
        valign = "center",
        border = "TopBottomLeftRight",
        borderColour = mk_header_col
      )

      # One full style per trend (fill + matching border/font)
      trend_styles <- lapply(mk_trend_colors, function(hex) {
        openxlsx::createStyle(
          fontName = "Arial",
          fontSize = 11,
          fontColour = "#000000",
          fgFill = hex,
          halign = "center",
          valign = "center",
          border = "TopBottomLeftRight",
          borderColour = mk_header_col
        )
      })

      # ── Helper: write header + data + trend fills ─────────────────────────
      format_sheet <- function(wb, sheet_name, data) {
        nr <- nrow(data)
        nc <- ncol(data)

        # Header row
        openxlsx::addStyle(
          wb,
          sheet_name,
          header_style,
          rows = 1,
          cols = seq_len(nc),
          gridExpand = TRUE
        )

        # All data cells with base style
        openxlsx::addStyle(
          wb,
          sheet_name,
          data_style,
          rows = seq(2, nr + 1),
          cols = seq_len(nc),
          gridExpand = TRUE
        )

        # Overwrite trend cells with colour style
        for (col_idx in seq_len(nc)) {
          col_vals <- data[[col_idx]]
          for (trend in names(trend_styles)) {
            matching_rows <- which(col_vals == trend)
            if (length(matching_rows) > 0) {
              openxlsx::addStyle(
                wb,
                sheet_name,
                style = trend_styles[[trend]],
                rows = matching_rows + 1L,
                cols = col_idx,
                stack = FALSE
              )
            }
          }
        }

        # Auto-fit column widths
        openxlsx::setColWidths(
          wb,
          sheet_name,
          cols = seq_len(nc),
          widths = "auto"
        )
      }

      # ── Build sheet(s) ────────────────────────────────────────────────────
      if (!input$mk_table_switch) {
        export_data <- mk_results() %>%
          select(location_code, chem_name, trend) %>%
          arrange(location_code, chem_name) %>%
          pivot_wider(names_from = chem_name, values_from = trend)

        openxlsx::addWorksheet(wb, "Trend Summary")
        openxlsx::writeData(wb, sheet = "Trend Summary", export_data)
        format_sheet(wb, "Trend Summary", export_data)
      } else {
        export_data <- mk_results() %>%
          select(-data) %>%
          arrange(location_code) %>%
          mutate_if(is.numeric, signif, 4)

        openxlsx::addWorksheet(wb, "Test Statistics")
        openxlsx::writeData(wb, sheet = "Test Statistics", export_data)
        format_sheet(wb, "Test Statistics", export_data)
      }

      openxlsx::saveWorkbook(wb, file)
    }
  )

  output$facet_plot <- renderPlotly({
    req(facet_data())

    facet_p <- facet_data() %>%
      mutate(chem_name = glue('{chem_name} ({output_unit})')) %>%
      timeseries_plot() +
      facet_wrap(~chem_name, scales = "free_y") +
      theme_light() +
      theme(strip.background = element_rect(fill = NA, colour = "black")) +
      theme(strip.text = element_text(colour = 'black')) +
      labs(x = NULL, y = "Concentration", colour = "Location") +
      scale_y_continuous(labels = scales::label_number())

    plotly::ggplotly(facet_p, dynamicTicks = TRUE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
