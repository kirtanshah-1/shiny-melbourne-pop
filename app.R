library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(sf)
library(plotly)

# Data loading and processing
my_sf_vic <- st_read("shapes/VMADMIN/POSTCODE_POLYGON.shp")

my_sf_melb <- st_read("shapes/MELB_POSTCODE_POLYGON/MELB_POSTCODE_POLYGON.shp")

density <- read_csv("data/density_slope.csv")
density$POA <- as.character(density$POA)
city_pop <- read.csv("data/largest-cities-by-population-2025.csv")

key_cities <- c("Melbourne", "London", "Mumbai", "Tokyo", "New York City", "Los Angeles", "Singapore")

top_density <- density %>%
  arrange(desc(SLOPE)) %>%
  top_n(15)

population <- read.csv("data/merged.csv")

population <- population %>%
  filter(population$POA %in% top_density$POA)

long_data <- pivot_longer(population, cols = starts_with("Population"), names_to = "Year", values_to = "Population")
long_data$Year <- sub("Population..", "", long_data$Year)
long_data$Year <- sub("\\.", "", long_data$Year)

city_pop <- city_pop %>%
  filter(city_pop$rank < 150)

my_sf_vic_merged <- inner_join(my_sf_vic, density, by=c("POSTCODE"="POA"))
my_sf_melb_merged <- inner_join(my_sf_melb, density, by=c("POSTCODE"="POA"))

create_p1 <- function(data, x, y, size) {
  ggplot(data, aes(x=population, y=growthRate, size=population,
                   text=paste("City:", `city`, "<br>",
                              "Country:", `country`, "<br>",
                              "Population:", prettyNum(`population`, big.mark = ",", scientific=FALSE), "<br>",
                              "Rate of Growth:", `growthRate`, "<br>"),
                   alpha=0.5)) + 
    geom_point(aes(colour = city %in% key_cities)) +
    geom_text(aes(label=ifelse(city %in% key_cities, as.character(city),'')),
              hjust=0.5, vjust=-1,
              size=4) +
    labs(x="Population", y="Rate of Population Growth") +
    scale_x_log10(labels = scales::comma) +
    ggtitle("Populations of cities around the world by their rate of growth") +
    scale_colour_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
    theme(legend.position = "none")
}

create_p2 <- function(data, x, y, group) {
  ggplot(data=long_data, aes(x=Year, y=Population, group=POA, 
                        colour = as.factor(POA), 
                        text = paste("Postal Area:", `POA`, "<br>",
                                     "Population:", `Population`, "<br>",
                                     "Year:", `Year`))) + 
    geom_line() + 
    geom_point() +
    labs(title = "Population changes of Top 15 POAs from 2006 to 2021",
         x = "Year", y = "Population", colour = "POA") +
    theme_minimal() +
    scale_y_log10() +
    annotate("text", x = "2016", y = 239,
             label = "Derrimut added to 3026 POA in 2018",
             colour = "darkgrey",
             size = 3, vjust = 5) +
    theme(legend.position = "none")
}

create_map1 <- function(data, fill_variable, year) {
  ggplot(data) +
    geom_sf(aes(
      fill = !!sym(fill_variable),
      text = sprintf("Postcode: %s<br>Density: %d", POSTCODE, round(!!sym(fill_variable), 0))
    ), linewidth = 0.1) +
    theme_void() +
    scale_fill_gradient(low = "blue", high = "red", name = HTML("POA Density<br>(people per km^2)")) +
    ggtitle(paste("Population Density by Postal Area in", year))
}

create_map2 <- function(data, fill_variable) {
  ggplot(data) +
    geom_sf(aes(
      fill = !!sym(fill_variable),
      text = sprintf("Postcode: %s<br>Expected increase in density (people/km^2): %d", POSTCODE, round(!!sym(fill_variable), 0))
    ), linewidth = 0.1) +
    theme_void() +
    scale_fill_gradient(low = "blue", high = "red", name = HTML("Rate of Change in<br>POA Density<br>(people per km^2 per year)")) +
    ggtitle("Projected Population Density Changes for 2026")
}

ui <- dashboardPage(
  dashboardHeader(title = "Population Density in Melbourne Over Time", titleWidth = 455),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Melbourne in context", tabName = "beginning", selected = TRUE),
      menuItem("Melbourne through the years", tabName = "middle"),
      menuItem("Melbourne into the future", tabName = "end")
    )

  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "beginning",
              fluidRow(
                box("Melbourne is a city strategically placed at the heart of the Port Phillip Bay. It is a hub for shipping, commerce, education and culture, much like many other metropolises across the world, like New York City, London, Paris, Mumbai, Tokyo, and Buenos Aires. Melbourne today is placed at a fork in the path, with one tine leading Melbourne to continue sprawling like its counterparts in the western United States with labyrinthine neighbourhoods marring the land, displacing native flora and fauna in favour of asphalt and concrete streets lined with cookie-cutter homes, and the other leading Melbourne to densify, catering to a growing population that will match the likes of its contemporaries over time.",
                    width = 500),
                box(plotlyOutput("p1"), width = 500, "(World Population Review, 2025)")
              )
      ),
      
      tabItem(tabName = "middle",
              fluidRow(
                box(plotlyOutput("densityMap"), "(Victorian Government, 2025), (Australian Bureau of Statistics, 2006), (Australian Bureau of Statistics, 2011), (Australian Bureau of Statistics, 2016), (Australian Bureau of Statistics, 2021)"),
                box(plotlyOutput("p2"), "(Victorian Government, 2025), (Australian Bureau of Statistics, 2006), (Australian Bureau of Statistics, 2011), (Australian Bureau of Statistics, 2016), (Australian Bureau of Statistics, 2021)"),
                box(sliderInput("year", "Select Year:",
                                min = 2006, max = 2021,
                                value = 2006,
                                step = 5,
                                sep = "")),
                box("Through the 21st Century, Melbourne at large has maintained a relatively low population density, only really concentrating in the Postal Areas (POAs) in the City Centre. And this has stayed relatively constant through the time period observed.")
              )
      ),
      
      tabItem(tabName = "end",
              box(plotlyOutput("densityProjection"), "(Victorian Government, 2025), (Australian Bureau of Statistics, 2006), (Australian Bureau of Statistics, 2011), (Australian Bureau of Statistics, 2016), (Australian Bureau of Statistics, 2021)"),
              box("Based on the used data, a linear regression was conducted to determine the approximate rate at which the population density of each POA increased. Densification trends are well-established in and around the City Centre, as the city's north and west develop. This hints that there too will be an increased rate of densification."),
              box(
                HTML("
                <strong>References</strong><br>
                Australian Bureau of Statistics. (2006). TableBuilder. Retrieved from Australian Bureau of Statistics: https://tablebuilder.abs.gov.au/webapi/jsf/login.xhtml<br>
                Australian Bureau of Statistics. (2011). TableBuilder. Retrieved from Australian Bureau of Statistics: https://tablebuilder.abs.gov.au/webapi/jsf/login.xhtml<br>
                Australian Bureau of Statistics. (2016). TableBuilder. Retrieved from Australian Bureau of Statistics: https://tablebuilder.abs.gov.au/webapi/jsf/login.xhtml<br>
                Australian Bureau of Statistics. (2021). TableBuilder. Retrieved from Australian Bureau of Statistics: https://tablebuilder.abs.gov.au/webapi/jsf/login.xhtml<br>
                Victorian Government. (2025, May 18). Vicmap Admin - Postcode Polygon. Retrieved from DataVic: https://discover.data.vic.gov.au/dataset/vicmap-admin-postcode-polygon<br>
                World Population Review. (2025). Largest Cities by Population 2025. Retrieved from World Population Review: https://worldpopulationreview.com/cities
                     ")
                )
              )
      ),
    selected = "beginning"
  )
)

server <- function(input, output, session) {
  
  output$p1 <- renderPlotly({
    map_plot <- create_p1(data=city_pop, x=population, y=growthRate, size=population)
    ggplotly(map_plot, tooltip = "text")
  })
  
  output$p2 <- renderPlotly({
    map_plot <- create_p2(data=long_data, x=Year, y=Population, group=POA)
    ggplotly(map_plot, tooltip="text")
  })
  
  output$densityMap <- renderPlotly({
    # Determine the fill variable based on the selected year
    fill_variable <- switch(as.character(input$year),
                            "2006" = "DENSITY_2006",
                            "2011" = "DENSITY_2011",
                            "2016" = "DENSITY_2016",
                            "2021" = "DENSITY_2021")
    
    # Create map for the selected year
    map_plot <- create_map1(my_sf_melb_merged, fill_variable, input$year)
    ggplotly(map_plot, tooltip = "text")
  })
  
  output$densityProjection <- renderPlotly({
    fill_variable <- "SLOPE"
    map_plot <- create_map2(my_sf_melb_merged, fill_variable)
    ggplotly(map_plot, tooltip = "text")
  })
}

shinyApp(ui, server)