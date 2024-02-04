# install.packages("devtools")
#devtools::install_github("jenright-git/gRs")
library(shiny)
library(bslib)
library(gRs)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- page_navbar(title="Mann Kendall Analysis and Timeseries Plots",
                  sidebar = sidebar(
                    accordion(accordion_panel("Data_Upload", 
                                              shiny::fileInput(inputId = "file_input", 
                                                               label = "Upload Esdat File", 
                                                               accept = ".xlsx"),
                    ),
                    accordion_panel("Mann-Kendall Controls", 
                                    uiOutput(outputId = "analyte_selector"),
                                    uiOutput(outputId = "location_selector")
                    ),
                    
                    accordion_panel("Plotting Controls", open=TRUE,
                                    uiOutput(outputId = "plotting_analytes"),
                                    uiOutput(outputId = "plotting_locations"),
                                    uiOutput(outputId = "plotting_date")
                                    
                    ), 
                    open = c("Data_Upload", "Mann-Kendall Controls", "Plotting Controls"))
                    
                    ),
                  #Main Page  
                  nav_spacer(),
                  nav_panel("Mann-Kendall", 
                            navset_card_tab(
                              nav_panel("Trend Heatmap", 
                                        card(
                                          plotOutput("mann_kendall_heatmap"), 
                                          full_screen = TRUE)),
                              nav_panel("Results Table", card( 
                                DT::dataTableOutput("mann_kendall_table"),
                                full_screen = TRUE))
                            )),
                  
                  nav_panel("Timeseries", 
                            layout_columns(col_widths = c(12,6,6),
                                           card(card_header("Timeseries Plot"),
                                                plotOutput("timeseries_plot"), full_screen = TRUE
                                           ),
                                           card(card_header("Histogram"), 
                                                plotOutput("conc_histogram"), full_screen = TRUE
                                           ),
                                           
                                           card(card_header("Boxplot"),
                                                plotOutput("conc_boxplot"), full_screen = TRUE)
                                           
                                           
                            ))
                  
                  
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  #bslib::bs_themer()
  
  file_data <- reactive({
    
    file <- input$file_input
    if(!is.null(file)){data_processor(file$datapath)}
    
  })
  
  
  output$mann_kendall_heatmap <- renderPlot({
    req(file_data())
    
    file_data() %>% 
      filter(analyte %in% input$analyte_input, 
             location %in% input$location_input) %>% 
      mann_kendall_test() %>% 
      mann_kendall_heatmap(label_text_size = 6)
  })
  
  output$mann_kendall_table <- DT::renderDataTable({    
    options = list(
      dom = "Blfrtrip", 
      buttons = c("copy", "csv", "excel", "pdf", "print"), 
      lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50), "All")
    )
    req(file_data())
    
    file_data() %>% 
      filter(analyte %in% input$analyte_input, 
             location %in% input$location_input) %>% 
      arrange(location) %>% 
      mann_kendall_test() %>%
      mutate_if(is.numeric, signif, 4) %>% 
      DT::datatable(., extensions = "Buttons", 
                    filter = list(position = "top"),
                    options = list(dom = 'Blfrtip', 
                                   buttons = c("copy", "csv", "excel", "pdf", "print"),
                                   lengthMenu = list(c(-1, 10,25,50),
                                                     c("All",10,25,50))))
  })
  
  
  
  output$analyte_selector <- renderUI({
    
    req(file_data())
    selectInput("analyte_input", 
                label = "Select Analytes", 
                choices = file_data() %>% distinct(analyte),
                selected = file_data()$analyte %>% unique(), 
                multiple = TRUE)
    
  })
  
  output$location_selector <- renderUI({
    
    req(file_data())
    selectInput("location_input", 
                label = "Select Locations", 
                choices = file_data() %>% distinct(location),
                selected = file_data()$location %>% unique(), 
                multiple = TRUE)
    
  })
  
  
  output$plotting_analytes <- renderUI({
    
    req(file_data())
    selectInput("plotting_analytes", 
                label = "Select Analytes", 
                choices = file_data() %>% distinct(analyte),
                multiple = FALSE)
    
  })  
  
  output$plotting_locations <- renderUI({
    
    req(file_data())
    selectInput("plotting_locations", 
                label = "Select Locations", 
                choices = file_data() %>% distinct(location),
                selected = file_data()$location %>% unique(), 
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
  
  
  output$timeseries_plot <- renderPlot({
    
    req(file_data())
    if(!is.null(file_data())){
      date_range <- input$plotting_date
      
      df <- 
        file_data() %>% 
        filter(analyte %in% input$plotting_analytes, 
               location %in% input$plotting_locations,
               date >= date_range[1] & date <= date_range[2])
      
      y_unit <- unique(df$units)
      
      df %>% 
        timeseries_plot(y_unit = y_unit)+
        theme_light()
    }
  })
  
  output$conc_histogram <- renderPlot({
    req(file_data())
    if(!is.null(file_data())){
      date_range <- input$plotting_date
      
      
      file_data() %>% 
        filter(analyte %in% input$plotting_analytes, 
               location %in% input$plotting_locations,
               date >= date_range[1] & date <= date_range[2]) %>% 
        ggplot(aes(concentration))+
        geom_histogram(fill="steelblue1", colour="black", binwidth = input$bin_selector)+
        #facet_wrap(as.formula(paste0("~",input$plotting_locations, sep="")), scales="free_x") +
        theme_light()+
        labs(x="Concentration", y="Count")
    }
  }) 

  
  output$conc_boxplot <- renderPlot({
    req(file_data())
    
    date_range <- input$plotting_date
    
    file_data() %>% 
      filter(analyte %in% input$plotting_analytes, 
             location %in% input$plotting_locations,
             date >= date_range[1] & date <= date_range[2]) %>% 
      ggplot(aes(location, concentration, fill=location))+
      geom_boxplot(alpha=0.4, outlier.shape = NA)+
      geom_jitter(shape=21, alpha=0.4)+
      theme_light()+
      scale_fill_manual(breaks = waiver(), values = location_colours)+
      labs(x=NULL, y="Concentration")
    
    
  }) 
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
