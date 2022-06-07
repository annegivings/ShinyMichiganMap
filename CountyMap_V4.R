#https://rstudio.github.io/shinydashboard/structure.html
library(shinydashboard)
library(shinycssloaders)
library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)
library(DT)
library(leaflet)
library(rgdal)
library(janitor)

setwd("~/R")
getwd()

# Input data: shapefile and csv
mich2 <- readOGR("~/R", "Counties_(v17a)")
eitc <- read.csv("TY2020_CreditsbyCounty.csv")

# Clean up EITC data
keeps <- c("X", "TY.2020..6..Rate", "X.1", "X.2", "X.3", "X.4")
eitc <- eitc[keeps]
eitc <- eitc %>%
  row_to_names(row_number=1)
eitc <- eitc %>% rename(
  NAME = County
)
eitc[eitc=="Shiawasee"] <- "Shiawassee"
eitc[eitc=="Wayne (excluding Detroit)"] <- "Wayne"
eitc <- eitc[!(is.na(eitc$Amount) | eitc$Amount==""),]
# Merge shapefile and EITC data
mich2 <- merge(mich2, eitc, by = 'NAME')


# Create dataframe combining shapefile and eitc data
michigan <- as.data.frame(mich2)
eitc$average <- eitc$`Avg. EITC`
eitc[]<-lapply(eitc, gsub, pattern="$", fixed=TRUE, replacement="")
eitc[]<-lapply(eitc, gsub, pattern="%", fixed=TRUE, replacement="")
eitc$takeup <- eitc$`Takeup rate`
keepit <- c("NAME", "average", "takeup")
eitc <- eitc[keepit]
michigan <- merge(michigan, eitc, by='NAME')
michigan$average <- as.numeric(michigan$average)
michigan$takeup <- as.numeric(michigan$takeup)
summary(michigan$average)
summary(michigan$takeup)
michigan['highlight'] <- 0
# Set in increasing value of average - DOES NOT WORK
michigan <- michigan[order(michigan$average),]


ui <- dashboardPage(
  dashboardHeader(title = "EITC dashboard"),
  dashboardSidebar(
    disable=TRUE
  ),
  dashboardBody(
    fluidRow(
      titlePanel(htmlOutput("countyname")),
      column(width = 4, 
               fluidRow(valueBoxOutput("takeup", width=12)),
               fluidRow(valueBoxOutput("avgeitc", width=12)),
               fluidRow(valueBoxOutput("economy", width=12))
             ),
      column(width = 8,
             box(
               leafletOutput("map")
             )
             )
    )
)
)

server <- function(input, output, session) {
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click 
    item <- click$id
    print(item)
    output$text <- renderText({click$id})
    output$countyname <- renderUI({click$id})
    # Highlight selected county
    lat <- click$lat
    lng <- click$lng
    coords <- as.data.frame(cbind(lng, lat))
    point <- SpatialPoints(coords)
    proj4string(point) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    selected <- mich2[point,]
    proj4string(selected) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    proxy <- leafletProxy("map")
    if(click$id == "Selected"){
      proxy %>% removeShape(layerId = "Selected")
    } else {
      proxy %>% addPolygons(data = selected, 
                            fillColor = "navy",
                            fillOpacity = 1, 
                            weight = 3, 
                            stroke = T,
                            layerId = "Selected")
    } 
    # Create dataframe with only one item
    county <- filter(michigan, NAME==click$id)
    output$takeup <- renderValueBox({
      valueBox(
        paste0(county$takeup, "%"), "Takeup Rate", icon = icon("home", lib = "glyphicon"),
        color = "purple", width = 12
      )
    })
    output$avgeitc <- renderValueBox({
      valueBox(
        paste0("$", county$average), "Average EITC", icon = icon("usd", lib = "glyphicon"),
        color = "yellow", width = 12
      )
    })
    
    output$economy <- renderValueBox({
      valueBox(
        paste0(county$Amount), "Returned to the local economy", icon = icon("home", lib = "glyphicon"),
        color = "navy", width = 12
      )
    })
    })
  
  output$plot <- renderPlot({
    a <- ggplot(data=michigan, aes(x=reorder(NAME, average), y=average), 
                fill = "Specific") +
      geom_bar(stat="identity", width = .75)
    a
  })
  
  output$map <- renderLeaflet({
    leaflet(mich2)%>% 
      setView(lng=-85, lat=45, zoom=6) %>%
      addPolygons(layerId=~NAME, weight = 1, smoothFactor=.5, 
                  opacity=1, 
                  color="#444444", fillOpacity = .5,
                  highlightOptions = highlightOptions(color="white",
                                    weight=2, bringToFront = TRUE))
  }) 
  output$economy <- renderValueBox({
    valueBox(paste0("$"), "Returned to the local economy", icon = icon("home", lib = "glyphicon"),
      color = "navy", width = 12
    )
  })
  output$takeup <- renderValueBox({
    valueBox(
      paste0("%"), "Takeup Rate", icon = icon("level-up", lib = "glyphicon"),
      color = "purple", width = 12
    )
  })
  output$avgeitc <- renderValueBox({
    valueBox(
      paste0("$"), "Average EITC", icon = icon("usd", lib = "glyphicon"),
      color = "yellow", width = 12
    )
  })
  
}

shinyApp(ui, server)
