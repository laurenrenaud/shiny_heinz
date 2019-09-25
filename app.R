# import libraries
library(shiny)
library(dplyr)
library(magrittr)
library(ggplot2)
library(readr)
library(ggmap)
library(maps)
library(RColorBrewer)
# library(leaflet)
library(scales)
library(plotly)
options(scipen = 9)

# load the data before starting up the application
capital.projects <- read_csv("data/capital_projects.csv") 

# remove erroneous lat / long
capital.projects %<>% 
  dplyr::filter(latitude != 0, longitude != 0) %>%
  dplyr::mutate(proj_year = format(start_date, "%Y")) 

# extract parameters for user input
status_options <- unique(capital.projects$status)
district_options <- sort(unique(capital.projects$council_district))
year_options <- unique(capital.projects$proj_year)

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Pittsburgh Capital Projects"),
  
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of observations to generate
      sliderInput("amount_input",
                  label = "Budgeted Amount",
                  min = 2500,  max = 7500000,
                  step = 500,
                  value = c(2500, 7500000)),
      
      # br() element to introduce extra vertical spacing 
      br(),
      
      # Input: Select the random distribution type 
      selectInput("status_input",
                  label="Status",
                  choices=status_options,
                  multiple=TRUE),
      
      # Input: Select the random distribution type 
      selectInput("district_input",
                  label="Council Districts",
                  choices=district_options,
                  multiple=TRUE),
      
      # Input: Select the random distribution type 
      selectInput("year_input",
                  label="Project Year",
                  choices=year_options,
                  multiple=TRUE)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("district_year_status_plot")),
                  tabPanel("Mapped", plotOutput("mapped")),
                  tabPanel("Table", tableOutput("table"))
      )
      
    )
  )
)

# Define server logic  ----
server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  projectsData <- reactive({
    
    proj.data <- dplyr::filter(capital.projects,
                               budgeted_amount >= input$amount_input[1],
                               budgeted_amount <= input$amount_input[2]
    )
    
    # on these filters below, only filtering if a filter is selected
    
    # check if we have specified status
    if(length(input$status_input) > 0){
      proj.data <- proj.data %>%
        dplyr::filter(
          status %in% input$status_input
        )
    }
    
    # check if we have specified district
    if(length(input$district_input) > 0){
      proj.data <- proj.data %>%
        dplyr::filter(
          council_district %in% input$district_input
        )
    }

    # check if we have specified year
    if(length(input$year_input) > 0){
      proj.data <- proj.data %>%
        dplyr::filter(
          proj_year %in% input$year_input
        )
    }
    
    return(proj.data)
    
  })
  
  
  projectsSummarized <- reactive({
    
    cap.proj.year.dist.status <- projectsData() %>%
      dplyr::group_by(proj_year, council_district, status) %>%
      dplyr::summarise(amount = sum(budgeted_amount, na.rm = T))
    
    return(cap.proj.year.dist.status)
    
  })
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$district_year_status_plot <- renderPlot({
    projectsSummarized() %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(council_district)) %>%
      dplyr::mutate(status = ordered(status, c("Canceled", "Planned", "In Progress", "Completed")),
                    council_district = paste0("District ", council_district)) %>%
      ggplot(aes(x = proj_year, y = amount, fill = status)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_color_brewer(palette = "Paired") + 
      scale_y_continuous(labels = scales::dollar) +
      #coord_flip() +
      theme_light() +
      facet_wrap("council_district", ncol = 3) +
      labs(x = "Project Year", y = "Budgeted Amount", fill = "",
           title = "Capital Project Budgets by District, Year, Status") 
  })
  
  
  output$mapped <- renderPlot({
    
    map_box <- make_bbox(lat = latitude, lon = longitude, 
                         data = capital.projects)
    base_map <- get_map(location = map_box, source = "google", color = "bw")
    # Google's Terms of Service: https://cloud.google.com/maps-platform/terms/.
    # Please cite ggmap if you use it! See citation("ggmap") for details.
    
    ggmap(base_map) +
      geom_jitter(data = projectsData(), 
                  aes(x = longitude, y = latitude, color = status, size = budgeted_amount),
                  alpha = 0.7) +
      scale_color_brewer(palette = "Paired") + 
      theme_void() +
      labs(x = "", y = "", title = "Locations", 
           color = "Status", size = "Budgeted\nAmount")
  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(projectsSummarized())
  })
  
  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    projectsSummarized()
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)

