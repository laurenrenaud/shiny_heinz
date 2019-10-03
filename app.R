# import libraries
library(shiny)
library(dplyr)
library(magrittr)
library(ggplot2)
library(readr)
library(ggmap)
library(maps)
library(RColorBrewer)
library(scales)
library(DT)
options(scipen = 9)

# load the data before starting up the application
# source: https://data.wprdc.org/dataset/capital-projects
capital.projects <- read_csv("data/capital_projects.csv") 

# remove erroneous lat / long
capital.projects %<>% 
  dplyr::filter(latitude != 0, longitude != 0) %>%
  dplyr::mutate(proj_year = format(start_date, "%Y")) 

# extract parameters for user input
status_options <- unique(capital.projects$status)
district_options <- sort(unique(capital.projects$council_district))
year_options <- unique(capital.projects$proj_year)

# Define UI ----
#' We're going to build a user interface side -- what the user will view
#' and interact with and input selections
#' and server side that will render calculations and plots
ui <- fluidPage(
  
  # App title ----
  titlePanel("Pittsburgh Capital Projects"),
  
  # we're going to use a simple layout with user input 
  # in a left column and plots and output on the right
  # Other layout templates: https://shiny.rstudio.com/gallery/
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider to filter based on project budget
      sliderInput("amount_input",
                  label = "Budgeted Amount",
                  min = 2500,  max = 7500000,
                  step = 500,
                  value = c(2500, 7500000)),
      
      # br() element to introduce extra vertical spacing 
      br(),
      
      # User selects status from drop down menu 
      selectInput("status_input",
                  label="Status",
                  choices=status_options,
                  # multiple = TRUE means user can select
                  # more than one
                  # switch to false to be single option dropdown
                  multiple=TRUE),
      
      
      selectInput("district_input",
                  label="Council Districts",
                  choices=district_options,
                  multiple=TRUE),
      
      
      selectInput("year_input",
                  label="Project Year",
                  choices=year_options,
                  multiple=TRUE)
      
    ),
    
    # Main panel for displaying outputs ----
    #' This panel will display outputs based on user filters
    #' and our server side logic
    mainPanel(
      
      h4(textOutput("total_projects")),
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("district_year_status_plot")),
                  tabPanel("Mapped", plotOutput("mapped")),
                  tabPanel("Table", DT::dataTableOutput("table"))
      )
      
    )
  )
)

# Define server logic  ----
server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  # Generating a reactive function that we're going to use like a dataframe
  projectsData <- reactive({
    
    proj.data <- dplyr::filter(capital.projects,
                               budgeted_amount >= input$amount_input[1],
                               budgeted_amount <= input$amount_input[2]
    )
    
    # on these filters below, only filtering if a filter is selected --
    # check that there is any input in that user selection, otherwise
    # do not apply filter
    
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
      dplyr::summarise(amount = round(sum(budgeted_amount, na.rm = T), 0))
    
    return(cap.proj.year.dist.status)
    
  })
  
  # any time you want to be sending text over to the user interface
  # going to use renderText function
  output$total_projects <- renderText({
    # Count the total number of projects we are exploring
    # and return some text in the form "Exploring x crashes"
    # note that we're calling the projectsData() function here
    proj.count <- format(nrow(projectsData()), big.mark=",")
    paste("Exploring", proj.count, "Projects", sep=" ")
  })
  
  # Generate a plot of the data ----
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
      facet_wrap("council_district", ncol = 3, scales = "free_y") +
      labs(x = "Project Year", y = "Budgeted Amount", fill = "",
           title = "Capital Project Budgets by District, Year, Status") 
  })
  
  
  output$mapped <- renderPlot({
    
    # Generate the lat/lon rectangle that will contain
    # the given lat and lon from the projects
    map_box <- make_bbox(lat = latitude, lon = longitude, 
                         data = capital.projects)
    # get base map based on those parameters
    base_map <- get_map(location = map_box, source = "google", color = "bw")
    # Google's Terms of Service: https://cloud.google.com/maps-platform/terms/.
    # Please cite ggmap if you use it! See citation("ggmap") for details.
    
    ggmap(base_map) +
      geom_jitter(data = projectsData(), 
                  aes(x = longitude, y = latitude, 
                      color = status, size = budgeted_amount),
                  alpha = 0.7) +
      scale_color_brewer(palette = "Paired") +
      scale_size(range = c(4, 12)) +
      theme_void() +
      labs(x = "", y = "", title = "Locations", 
           color = "Status", size = "Budgeted\nAmount")
  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(projectsSummarized())
  })
  
  # Generate a searchable, sortable table view of the data ----
  # more info on Shiny + datatables: https://shiny.rstudio.com/articles/datatables.html
  output$table <- DT::renderDataTable(DT::datatable({
    projectsSummarized() %>%
      dplyr::rename(
        `Project Year` = proj_year,
        `Council District` = council_district,
        Status = status,
        Amount = amount
      )
  }))
  
}

# Create Shiny app ----
shinyApp(ui, server)

