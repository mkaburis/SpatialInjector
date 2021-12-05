library(shiny)

census_years <- list("2000", "2010")
acs_years <- list("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012",
               "2013", "2014", "2015", "2016", "2017", "2018", "2019")
geography <- c("us", "state", "county", "tract")
counties <- read.csv("./ref/us_counties.csv", header = TRUE, sep=",")

ui <- fluidPage(
  titlePanel(
    title = (div(img(src = "logo.png", height = 80), "Spatial Injector - Census Data Aggregator for GIS",
                 windowTitle="Spatial Injector"))
  ),
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
      selected = c("5-Year")
    )
  ),
  
  selectInput(
    "geography",
    label = "Geography",
    choices = geography,
    selected = c("State")
  ),
  
  conditionalPanel(
    condition = "input.geography == 'County'",
    selectInput(
      "state",
      label = "State",
      choices = state.name,
      selected = c(state.name[1])
    )
  )
  
)

server <- function(input, output, session) {
  
  observe( {
    x = input$data_type
    
    if (x == "American Community Survey") {
      updateSelectInput(session,'year', choices=acs_years)
    }
  })
  
}

shinyApp(ui, server)