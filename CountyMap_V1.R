#https://www.paulamoraga.com/book-geospatial/sec-shinyexample.html
install.packages("leaflet")
install.packages("rgdal")
install.packages("shinycssloaders")
install.packages("readxl")
install.packages("DT")
library(shinycssloaders)
library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)
library(DT)
library(leaflet)
library(rgdal)


mich2 <- readOGR("~/R", "Counties_(v17a)")
summary(mich2)
eitc <- read.csv("TY2020_CreditsbyCounty.csv")
#View(mich2)
View(eitc)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      #DTOutput(outputID = "table")
    ),
    mainPanel(
      leaflet(mich2) %>% 
        addPolygons(weight = 1, smoothFactor=.5, opacity=1, 
            color="#444444", fillOpacity = .5,
            highlightOptions = highlightOptions(color="white", 
            weight=2, bringToFront = TRUE))
    )
  )
)

server <- function(input, output){
  #observe({
  #  event <- input$mich2_shape_click
  #  if(is.null(event))
  #    return()
  #  print(event)
  #})
  #output$table <- renderDT(data)
}

shinyApp(ui = ui, server = server)