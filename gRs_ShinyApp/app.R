# install.packages("devtools")
#devtools::install_github("jenright-git/gRs")
library(shiny)
library(bslib)
library(gRs)
library(tidyverse)
library(glue)
library(plotly)

# Define UI for application that draws a histogram
ui <- page_navbar(title="gRs Analysis Tool",
                  #Main Page  
                  nav_panel("Mann-Kendall", 
                            layout_sidebar(sidebar = accordion(
                              accordion_panel("Data_Upload", 
                                              shiny::fileInput(inputId = "file_input", 
                                                               label = "Upload Esdat File", 
                                                               accept = ".xlsx"),
                                              shiny::checkboxInput(inputId = "half_lor", 
                                                                   label = "Use Half LOR?", 
                                                                   value = FALSE)
                              ),
                              
                              accordion_panel("Mann-Kendall Controls",
                                              actionButton(inputId = "mk_button", label = "Run Trend Analysis"),
                                              uiOutput(outputId = "analyte_selector"),
                                              uiOutput(outputId = "location_selector")),
                              
                              accordion_panel("Heatmap Controls",
                                              splitLayout(
                                                numericInput("mk_trend_label", "Label Size", value = 6, min = 1, width = '70%'),
                                                numericInput("mk_label_width", "Label Width", value = 20, min = 0, width = '70%')),
                                              splitLayout(
                                                numericInput("mk_x_text", "X Label Size", 12, min = 1, width = '70%'),
                                                numericInput("mk_y_text", "Y Label Size", 12, min = 1, width = '70%')),
                                              textInput("mk_title", label = "Title", value = "Mann-Kendall Trend Analysis"),
                                              splitLayout(
                                                numericInput("mk_title_size", "Title Size", value=10, width = '70%'),
                                                numericInput("mk_legend_text", "Legend Text Size",value=10, min=0, width='70%'))
                              ), open = c("Data_Upload", "Mann-Kendall Controls")),
                              navset_card_tab(
                                nav_panel("Trend Heatmap", 
                                          card(
                                            plotOutput("mann_kendall_heatmap"), 
                                            full_screen = TRUE)),
                                nav_panel("Results Table", card( 
                                  DT::dataTableOutput("mann_kendall_table"),
                                  full_screen = TRUE))
                                , 
                                nav_panel("Increasing Trends",
                                          card(
                                            plotOutput("mk_increasing"), 
                                            full_screen=TRUE)
                                )
                                
                              ))),
                  
                  nav_panel("Timeseries", 
                            layout_sidebar(sidebar = accordion(
                              accordion_panel("Filters", open=TRUE,
                                              uiOutput(outputId = "plotting_analytes"),
                                              actionButton("update_plot_locations", "Update Locations"),
                                              uiOutput(outputId = "plotting_locations"),
                                              uiOutput(outputId = "plotting_date")),
                              accordion_panel("Plotting Controls", 
                                              radioButtons(inputId = "date_breaks_radio", 
                                                           label = "Axis Date Breaks", 
                                                           selected = "3 months", inline = TRUE, 
                                                           choiceNames = list("Week", "Month", "3 Months", "Year", "2 Years", "5 Years"), 
                                                           choiceValues = list("week", "month", "3 months", "year", "2 years", "5 years")),
                                              radioButtons(inputId = "date_label_radio", 
                                                           label = "Axis Date Labels", 
                                                           choices = list("%d %b %y", "%b %y", "%B %Y", "%B %y", "%Y"), 
                                                           selected = "%b %y", inline = TRUE ),
                                              splitLayout(
                                                numericInput("ts_date_size", "Date Size", 10, min = 0, width = '60%'),
                                                numericInput("ts_x_angle", "Date Angle", 0, min=0, max=360, width = '60%')),
                                              splitLayout(
                                                numericInput("min_conc", "Min Y Conc", 0, width='85%'),
                                                numericInput("max_conc", "Max Y Conc", NA,width='85%'))
                                              
                              ),
                              accordion_panel("Criteria", 
                                              checkboxInput("criteria_check", label = "Apply Criteria", value = FALSE),
                                              textInput("criteria_label", label = "Criteria Label", placeholder = "Enter Criteria Label", value = NULL),
                                              splitLayout( 
                                                numericInput("criteria_value", label = "Criteria Value", value = 1, min = 0),
                                                textInput(inputId = "criteria_colour", label = "Colour", value = "Red")
                                              ))
                              
                              
                              
                            ), 
                            
                            
                            layout_columns(col_widths = c(6,6,6,6),
                                           card(card_header("Timeseries Plot"),
                                                plotOutput("timeseries_plot"), full_screen = TRUE),
                                           card(card_header("Plotly Timeseries"), 
                                                plotlyOutput("timeseries_two"), full_screen = TRUE),
                                           card(card_header("Histogram"), 
                                                layout_sidebar(sidebar = 
                                                                 sidebar(numericInput(inputId = "bin_selector", 
                                                                                      label = "Select Bin Width",
                                                                                      value = 0), 
                                                                         checkboxInput(inputId = "facet_check", 
                                                                                       label = "Facet by Location", 
                                                                                       value = FALSE),
                                                                         open = FALSE),
                                                               plotOutput("conc_histogram")
                                                ), full_screen = TRUE),
                                           
                                           card(card_header("Boxplot"),
                                                plotOutput("conc_boxplot"), full_screen = TRUE)
                                           
                                           
                            ))),
                  
                  nav_panel(title = "Summary Stats",
                            card(card_header("Stats"), 
                                 DT::dataTableOutput("stats_table"),
                                 full_screen = TRUE)
                            
                  )
                  
                  
)



# Define server logic required to make visualisations
server <- function(input, output) {
  
  file_data <- reactive({
    
    file <- input$file_input
    
    lor_check <- input$half_lor
    
    if(!is.null(file) & lor_check){data_processor(file$datapath) %>% half_lor()}
    else if(!is.null(file) & lor_check==FALSE){data_processor(file$datapath)}
    else {}
    
    
    
  })
  
  mk_results <- eventReactive(input$mk_button, {
    
    file_data() %>% 
      filter(chem_name %in% input$analyte_input, 
             location_code %in% input$location_input) %>% 
      mann_kendall_test()
    
  })
  
  
  output$mann_kendall_heatmap <- renderPlot({
    req(mk_results())
    
    mk_results() %>% 
      mann_kendall_heatmap(label_text_size = input$mk_trend_label, 
                           width = input$mk_label_width, 
                           plot_title = input$mk_title) + 
      theme(axis.text.y = element_text(size = input$mk_y_text),
            axis.text.x = element_text(size=input$mk_x_text), 
            legend.text = element_text(size=input$mk_legend_text), 
            title = element_text(size=input$mk_title_size, face = "bold"))
  })
  
  output$mann_kendall_table <- DT::renderDataTable({    
    options = list(
      dom = "Blfrtrip", 
      buttons = c("copy", "csv", "excel", "pdf", "print"), 
      lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50), "All")
    )
    req(mk_results())
    
    mk_results() %>% 
      arrange(location_code) %>% 
      mutate_if(is.numeric, signif, 4) %>% 
      DT::datatable(., extensions = "Buttons", 
                    filter = list(position = "top"),
                    options = list(dom = 'Blfrtip', 
                                   buttons = c("copy", "csv", "excel", "pdf", "print"),
                                   lengthMenu = list(c(-1, 10,25,50),
                                                     c("All",10,25,50))))
  })
  
  output$mk_increasing <- renderPlot({
    req(mk_results())
    
    mk_results() %>% 
      filter(trend == "Increasing") %>% 
      left_join(., file_data()) %>% 
      mutate(chem_name = glue('{chem_name} ({output_unit})')) %>% 
      ggplot(aes(date, concentration, colour=location_code))+
      geom_point(alpha=0.6, size=1.2)+
      facet_wrap(~chem_name, scales="free_y")+
      geom_smooth(se=F)+
      theme_light()+
      labs(x=NULL, y="Concentration", colour=NULL)
    
  })
  
  
  output$analyte_selector <- renderUI({
    
    req(file_data())
    selectInput("analyte_input", 
                label = "Select Analytes", 
                choices = file_data() %>% distinct(chem_name),
                selected = file_data()$chem_name %>% unique(), 
                multiple = TRUE)
    
  })
  
  output$location_selector <- renderUI({
    
    req(file_data())
    selectInput("location_input", 
                label = "Select Locations", 
                choices = file_data() %>% distinct(location_code),
                selected = file_data()$location_code %>% unique(), 
                multiple = TRUE)
    
  })
  
  
  output$plotting_analytes <- renderUI({
    
    req(file_data())
    selectInput("plotting_analytes", 
                label = "Select Analytes", 
                choices = file_data() %>% distinct(chem_name),
                multiple = FALSE)
    
  })  
  
  output$plotting_locations <- renderUI({
    
    req(file_data())
    selectInput("plotting_locations", 
                label = "Select Locations", 
                choices = file_data() %>% distinct(location_code),
                selected = file_data()$location_code %>% unique(), 
                multiple = TRUE)
    
  })
  
  output$plotting_date <- renderUI({
    
    req(file_data())
    shiny::dateRangeInput(inputId = "plotting_date", 
                          label = "Select Start and End Date", 
                          start = as.Date(min(file_data()$date)),
                          end = as.Date(max(file_data()$date)), 
                          format="dd-mm-yyyy")
    
    
  })
  
  plotting_data <- eventReactive(input$update_plot_locations, {
    
    file_data() %>% 
      filter(location_code %in% input$plotting_locations)
    
  })

    
  output$timeseries_plot <- renderPlot({
    
    req(plotting_data())
    
    establish_plotting_variables(data = file_data())
    
    date_range_plot <- input$plotting_date
    
    df <- 
      plotting_data() %>% 
      filter(chem_name %in% input$plotting_analytes,
             date >= date_range_plot[1] & date <= date_range_plot[2])
    
    y_unit <- unique(df$units)
    
    plot <- df %>% 
      timeseries_plot(date_size = input$ts_date_size, 
                      x_angle = input$ts_x_angle, 
                      y_unit = y_unit, 
                      dates_range = as.POSIXct(date_range_plot), 
                      date_break = input$date_breaks_radio, 
                      date_label = input$date_label_radio, 
                      ymin = input$min_conc, 
                      ymax = input$max_conc, 
                      y_title_size = 12)
    
    if(input$criteria_check){
      plot <- plot + 
        scale_y_limitval(marker_values = c(input$criteria_value), 
                         marker_labels = input$criteria_label, 
                         marker_colours = c(input$criteria_colour))
    } 
    
    # Not allowing to adust max y scale as the limitval function applies its own scale_y_continuous
    
    plot  # Return the plot object
    
  })
  
  
  output$timeseries_two <- renderPlotly({
    
    req(plotting_data())
    
    date_range <- input$plotting_date
    
    df <- 
      plotting_data() %>% 
      filter(chem_name %in% input$plotting_analytes, 
             date >= date_range[1] & date <= date_range[2])
    
    y_unit <- unique(df$units)
    
    df %>% 
      plot_ly(x=~date, y=~concentration, 
              color=~location_code, 
              colors = location_colours, 
              type = "scatter", 
              mode="lines") %>%
      layout(xaxis = list(title = ""), 
             yaxis = list(title = glue('Concentration ({y_unit})')), legend = list(title=list(text="Location")))
    
  }) 
  
  
  output$conc_histogram <- renderPlot({
    req(plotting_data())

      date_range <- input$plotting_date
      
      hist_data <- plotting_data() %>% 
        filter(chem_name %in% input$plotting_analytes, 
               date >= date_range[1] & date <= date_range[2])
      
      binwidth <- ifelse(input$bin_selector==0, max(hist_data$concentration) / 30, input$bin_selector)
      
      y_unit <- unique(hist_data$units)
      
      hist_plot <- hist_data %>% 
        ggplot(aes(concentration))+
        geom_histogram(fill="steelblue1", colour="black", 
                       binwidth = binwidth)+
        theme_light()+
        labs(x=glue('Concentration ({y_unit})'), y="Count")
      
      if(input$facet_check){
        hist_plot+
          facet_wrap(~location_code)}
      else{
        hist_plot} 
    }
  ) 
  
  
  output$conc_boxplot <- renderPlot({
    req(plotting_data())
    
    date_range <- input$plotting_date
    
    boxplot_data <- 
      plotting_data() %>% 
      filter(chem_name %in% input$plotting_analytes, 
             date >= date_range[1] & date <= date_range[2])
    
    y_unit <- unique(boxplot_data$units)
    
    bplot <- boxplot_data %>% 
      ggplot(aes(location_code, concentration, fill=location_code))+
      geom_boxplot(alpha=0.4, outlier.shape = NA, show.legend = FALSE)+
      geom_jitter(shape=21, alpha=0.4, show.legend = FALSE)+
      theme_light()+
      scale_fill_manual(breaks = waiver(), values = location_colours)+
      labs(x=NULL, y=glue("Concentration ({y_unit})"))+
      scale_y_continuous(limits = c(input$min_conc, input$max_conc))
    
    if(input$criteria_check){
      
      bplot + 
        scale_y_limitval(marker_values = c(input$criteria_value), 
                         marker_labels = input$criteria_label, 
                         marker_colours = c(input$criteria_colour))
      
      
    } else{
      
      bplot
    }    
    
  }) 
  
  
  output$stats_table <- DT::renderDataTable({    
    
    req(file_data())
    
    file_data() %>% 
      group_by(Location=location_code, Analyte=chem_name) %>% 
      summarise(n_samples = n(),
                Minimum = min(concentration, na.rm=T),
                Mean = mean(concentration, na.rm=T),
                Maximum = max(concentration, na.rm=-T),
                `5th Percentile` = quantile(concentration, 0.05, na.rm=T),
                `10th Percentile` = quantile(concentration, 0.1, na.rm=T),
                `25th Percentile` = quantile(concentration, 0.25, na.rm=T),
                `50th Percentile` = quantile(concentration, 0.5, na.rm=T),
                `75th Percentile` = quantile(concentration, 0.75, na.rm=T),
                `90th Percentile` = quantile(concentration, 0.9, na.rm=T),
                `95th Percentile` = quantile(concentration, 0.95, na.rm=T),
      ) %>% 
      mutate_if(is.numeric, signif, 4) %>% 
      DT::datatable(., extensions = "Buttons", 
                    filter = list(position = "top"),
                    options = list(dom = 'Blfrtip', 
                                   buttons = c("copy", "csv", "excel", "pdf", "print"),
                                   lengthMenu = list(c(-1, 10,25,50),
                                                     c("All",10,25,50))))
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
