library(shiny)
library(dplyr)
library(tidyverse)
library(tidycensus)
library(vroom)
library(janitor)

census_years <- list("2000", "2010")
acs1_years <- list("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012",
               "2013", "2014", "2015", "2016", "2017", "2018", "2019")
acs3_years <- list("2007", "2008", "2009", "2010", "2011", "2012",
                   "2013")
acs5_years <- list("2009", "2010", "2011", "2012",
                   "2013", "2014", "2015", "2016", "2017", "2018", "2019")
geography <- c("US", "State", "County", "Tract")
counties <- read.csv("./ref/us_counties.csv", header = TRUE, sep=",")
acs_types <- c("acs1", "acs3", "acs5")



ui <- fluidPage(
  titlePanel(
    title = (div(img(src = "logo.png", height = 100), "Spatial Injector - Census Data Aggregator for GIS",
                 windowTitle="Spatial Injector"))
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "data_type",
        label = "Census Data Type",
        choices = list("Decennial Census", "American Community Survey"),
        selected = c("Decennial Census")
      ),
      
      selectInput(
        "year",
        label = "Year",
        choices = census_years,
        selected = c("2010")
      ),
      
      conditionalPanel(
        condition = "input.data_type == 'American Community Survey'",
        selectInput(
          "estimate_type",
          label = "ACS Estimate Type",
          choices = list("1-Year", "3-Year", "5-Year"),
          selected = c("1-Year")
        )
      ),
      
      selectInput(
        "geography",
        label = "Geography",
        choices = geography,
        selected = c("State")
      ),
      
      conditionalPanel(
        condition = "input.geography == 'County' | input.geography == 'Tract'" ,
        selectInput(
          "state",
          label = "State",
          choices = state.abb,
          selected = c(state.abb[1])
        )
      ),
      
      conditionalPanel(
        condition = "input.geography == 'County' | input.geography == 'Tract'",
        uiOutput("countySelect")
      ),
      
      selectizeInput(
        "table",
        label = "Data Table",
        choices = NULL,
        selected = c("test"),
        ),
      
      fluidRow(
        actionButton("findtable","Search for Tables", icon = icon("search"), class="btn btn-success"),
        actionButton("inject","Inject Data", icon = icon("syringe"), class="btn btn-success")
      )

      
    ),
    
    mainPanel(
      # tableOutput("tabletest"),
      textOutput("hello")
    )
  )

  
)

server <- function(input, output, session) {
  
  
  findacsyr <- function(estimate) {
    acs_t <- switch(estimate, "1-Year" = acs_types[1], "3-Year" = acs_types[2],
                    "5-Year" = acs_types[3]) 
    return(acs_t)
  }
  
  observe( 
    {
    
      dtype <- input$data_type
      
      if(dtype == "American Community Survey") 
        {
        
        if(findacsyr(input$estimate_type) == "acs1") {
          updateSelectInput(session,'year', choices=acs1_years, selected = c("2005"))
          # updateSelectizeInput(session, 'table', choices = load_variables(input$year, "acs1"), server = TRUE)
        }
        
        else if (findacsyr(input$estimate_type) == "acs3") {
          updateSelectInput(session,'year', choices=acs3_years, selected = c("2007"))
          # updateSelectizeInput(session, 'table', choices = load_variables(input$year, "acs3", cache = TRUE), server = TRUE)
        }
        
        else {
          updateSelectInput(session,'year', choices=acs5_years, selected = c("2009"))
          # updateSelectInput(session, 'table', choices = load_variables(input$year, "acs5", cache = TRUE), server = TRUE)
        }

        
        
       }
      
      else {
        updateSelectInput(session,'year', choices=census_years, selected = c("2010"))
        # updateSelectizeInput(session, 'table', choices = load_variables(input$year, "sf1", cache = TRUE), server = TRUE)
      }
    
    
  })
  

  output$countySelect <- renderUI({
    selectInput('county', 
                'County', 
                choices = filter(counties, STATE == input$state)[4]
    )
  })
  

  output$state_val <- renderText(input$state)
  
  output$hello <- eventReactive(input$findtable, {
    # updateSelectInput(session, 'table',
    #                      choices = load_variables(input$year, findacsyr(input$estimate_type), cache = TRUE)
    
    if (input$data_type == "American Community Survey") {
    updateSelectizeInput(session, 'table', choices = load_variables(input$year, findacsyr(input$estimate_type), cache = TRUE))
    }
    else {
      updateSelectizeInput(session, 'table', choices = load_variables(input$year, "sf1", cache = TRUE))
    }
    
    return("hello")
  })
  
  f1 <- eventReactive(input$inject, {
    if(input$data_type == "Decennial Census")
    {
      get_decennial(
        geography = 
      )
    }
  })
  
  # output$tabletest <- renderTable(load_variables(input$year, "sf1", cache = TRUE))
  
  
}

shinyApp(ui, server)