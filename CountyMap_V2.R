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
keepit <- c("NAME", "average")
eitc <- eitc[keepit]
michigan <- merge(michigan, eitc, by='NAME')
michigan$average <- as.numeric(michigan$average)
summary(michigan$average)
michigan['highlight'] <- 0
# Set in increasing value of average - DOES NOT WORK
michigan <- michigan[order(michigan$average),]


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      style = "height: 700px;",
      "County:",
      textOutput("text"),
      plotOutput("plot", height = "100%")
    ),
  mainPanel(
    leafletOutput("map")
    )
  )
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet(mich2)%>% 
      addPolygons(layerId=~NAME, weight = 1, smoothFactor=.5, 
                  opacity=1, 
                  color="#444444", fillOpacity = .5,
                  highlightOptions = highlightOptions(color="white",
                                      weight=2, bringToFront = TRUE))
  }) 
  output$plot <- renderPlot({
    a <- ggplot(data=michigan, aes(x=NAME, y=average), fill = "Specific") +
      geom_bar(stat="identity", width = .75) + 
      coord_flip() 
    #LABEL:
      #geom_text(aes(label=`Avg. EITC`), vjust=-0.2)
    a
  })
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click 
    item <- click$id
    print(item)
    output$text <- renderText({click$id})
    # Create dataframe with only one item
    county <- filter(michigan, NAME==click$id)
    # Try to highlight one bar (NOT SUCCESSFUL)
    filtered <- reactive({
      michigan$Specific <- ifelse((michigan$NAME == click$id), 1,0)
      return(michigan)
    })
  })
}
shinyApp(ui = ui, server = server)