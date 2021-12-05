# TidyCensus Sandbox
# Mihail Kaburis



# Install required R packages if not installed on user's computer
packages <- c("tidycensus", "tidyverse","janitor","usethis")
install.packages(setdiff(packages, rownames(installed.packages())))

# Ensure that you have a Census API Key in your .Renviron file
# Uncomment the following line of code to create a .Renviron file
# usethis::edit_r_environ()

# Add the following line of code to the .Renviron folder:
# CENSUS_API_KEY="5967e6e9042cd59877c891173cc8035c69a88162"

library(tidycensus)
library(tidyverse)
library(janitor)
library(deplyrr)

census_years <- c("2000", "2010", "2020")
acs_years <- c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012",
"2013", "2014", "2015", "2016", "2017", "2018", "2019")

acs_types <- c("acs1", "acs3", "acs5")
census_summary_files <- c("sf1", "sf2")

# Grab variable list for Census

census_variable_list1 <- load_variables(census_years[2], "sf1", cache = TRUE)
census_variable_list2 <- load_variables(census_years[2], "sf1", cache = TRUE) %>% 
  group_by(concept) %>% 
  summarize(tablem = paste(name, collapse=' '))


census_varaible_list$mod_label <- (str_split(census_variable_list$label, "!!"))
census_variable_list$mod_label <- str_to_title(census_variable_list$concept)

nj <- get_decennial(
  geography = "tract",
  variables = c(poptotal = "P001001"),
  state = "NJ",
  county = "Middlesex",
  year = 2010,
  geometry = TRUE
)
nj %>% 
  ggplot(aes(fill = value)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") 
st_write(nj, "nj.shp")

write.csv(nj, "test.csv")

# Grab variable list for ACS
