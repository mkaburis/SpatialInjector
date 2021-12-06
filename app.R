library(shiny)
library(shinyjs)
library(dplyr)
library(tidyverse)
library(tidycensus)
library(vroom)
library(janitor)

census_api_key("5967e6e9042cd59877c891173cc8035c69a88162", install = TRUE, overwrite = TRUE)

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
  shinyjs::useShinyjs(),
  titlePanel(
    title = (div(img(src = "logo.png", height = 100), "Spatial Injector - Census Data Aggregator for GIS",
                 windowTitle="Spatial Injector"))
  ),
  
  fluidRow(
    column(4,
      selectInput(
        "data_type",
        label = "Census Data Type",
        choices = list("Decennial Census", "American Community Survey"),
        selected = c("Decennial Census"),
        width = "100%"
      ),
      
      selectInput(
        "year",
        label = "Year",
        choices = census_years,
        selected = c("2010"),
        width = "100%"
      ),
      
      conditionalPanel(
        condition = "input.data_type == 'American Community Survey'",
        selectInput(
          "estimate_type",
          label = "ACS Estimate Type",
          choices = list("1-Year", "3-Year", "5-Year"),
          selected = c("1-Year"),
          width = "100%"
        )
      ),
      
      selectInput(
        "geography",
        label = "Geography",
        choices = geography,
        selected = c("State"),
        width = "100%"
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
        width = "100%"
        ),
      
      fluidRow(
        
        column(12,
               
               splitLayout(cellWidths = c("50%", "50%"),
                           actionButton("findtable","Search for Tables", icon = icon("search"), class="btn btn-success", width = "95%"),
                           actionButton("inject","Inject Data", icon = icon("syringe"), class="btn btn-danger", width = "95%")
                           )
      )
      
    )),
    
    
    column(8,
      textOutput("hello"),
      textOutput("fx"),
      dataTableOutput("tabletest")
    )
  ),
  
  fluidRow(
    align = "center",
    br(),
    column(6,
           disabled(downloadButton("dl_csv", "Download CSV", width = "98%"))
           ),
    column(6,
           disabled(downloadButton("dl_shapefile", "Download Shapefile", width = "98%"))
           )
    )
  )

server <- function(input, output, session) {
  
  rvals <- reactiveValues(
    csv=NULL,
    x=NULL
  )
  
  findacsyr <- function(estimate) {
    acs_t <- switch(estimate, "1-Year" = acs_types[1], "3-Year" = acs_types[2],
                    "5-Year" = acs_types[3]) 
    return(acs_t)
  }
  
  cleancounty <- function(countyraw) {
    countyname <- unlist(strsplit(input$county, " "))
    countyname <- countyname[!countyname %in% "County"]
    modcounty <- paste(countyname, collapse = " ")
    return(modcounty)
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
      
      acstables <- load_variables(input$year, findacsyr(input$estimate_type), cache = TRUE) %>% 
        group_by(concept) %>% 
        summarize(tablem = paste(name, collapse=' '))
      updateSelectizeInput(session, 'table', choices = acstables[1])
    }
    else {
      
      censustables <- load_variables(input$year, "sf1", cache = TRUE) %>% 
        group_by(concept) %>% 
        summarize(tablem = paste(name, collapse=' '))
      updateSelectizeInput(session, 'table', choices = censustables[1])
    }
    
    return(input$table[2])
  })
  
  observeEvent(input$inject, {
    if(input$data_type == "American Community Survey")
    {
      acstables <- load_variables(input$year, findacsyr(input$estimate_type), cache = TRUE) %>% 
        group_by(concept) %>% 
        summarize(tablem = paste(name, collapse=' '))
      
      censusrow <- acstables$tablem[which(acstables$concept == input$table)]
      
      censusvars <- strsplit(censusrow, " ")
      
      if (input$geography == "US" || input$geography == "State")
      {
      output$tabletest <- renderDataTable(get_acs(
        geography = tolower(input$geography),
        variables = c(censusvars[[1]]),
        state = input$state,
        year = input$year), options=list(pageLength = 12, width = "100%"))
      
        return(ttable)
      }
      
      else{
        output$tabletest <- renderDataTable(get_acs(
          geography = tolower(input$geography),
          variables = c(censusvars[[1]]),
          state = input$state,
          county = cleancounty(input$county),
          year = input$year), options=list(pageLength = 12, width = "100%"))
      }
      
    }
    else {
      
      acstables <- load_variables(input$year, "sf1", cache = TRUE) %>% 
        group_by(concept) %>% 
        summarize(tablem = paste(name, collapse=' '))
      
      censusrow <- acstables$tablem[which(acstables$concept == input$table)]
      
      censusvars <- strsplit(censusrow, " ")
     
      if (input$geography == "US" ||input$geography == "State")
      { 
       outputtable <- get_decennial(
         geography = tolower(input$geography),
         variables = c(censusvars[[1]]),
         year = input$year)
      
        output$tabletest <- renderDataTable(outputtable, options = list(pageLength = 12, width = "100%"))
        
        rvals$csv <-outputtable
      }
      
      else {
        outputtable <- get_decennial(
          geography = tolower(input$geography),
          variables = c(censusvars[[1]]),
          state = input$state,
          county = cleancounty(input$county),
          year = input$year)
        
        output$tabletest <- renderDataTable(outputtable, options=list(pageLength = 12, width = "100%"))
        rvals$csv <-outputtable
      }
      
    }
    
    observeEvent(input$csv, {
      
    })
    
    shinyjs::enable("dl_csv")
    shinyjs::enable("dl_shapefile")
    

  })
  
  output$dl_csv <- downloadHandler(
    filename = function() {
      paste("data.csv", sep=" ")
    },
    content = function(file) {
      write.csv(rvals$csv, file)
    }
  )
  
  
  # output$tabletest <- renderTable(load_variables(input$year, "sf1", cache = TRUE))
}

shinyApp(ui, server)