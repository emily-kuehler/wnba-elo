library(shiny)
counties <- readRDS("data/counties.rds")
source("helpers.R")
library(maps)
library(mapproj)


# Define UI ---------------------------------------------------------------


ui <- fluidPage(
  
  titlePanel("censusVis"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Create demographic maps with information from the 2010 US Census."),
      
      selectInput("var", 
                  h3("Choose a variable to display"),
                  choices = list("Percent White",
                                 "Percent Black",
                                 "Percent Hispanic",
                                 "Percent Asian"),
                  selected = "Percent White"),
      sliderInput("range",
                  h3("Range of interest:"),
                  min = 0,
                  max = 100,
                  value = c(0,100))
      
    ),
    
    mainPanel(
      plotOutput("map")
    )
    
  )

)


# Define server logic -----------------------------------------------------

server <- function(input, output) {
  
  output$map <- renderPlot({
    
    
    data <- switch(input$var,
                   "Percent White" = counties$white,
                   "Percent Black" = counties$black,
                   "Percent Hispanic" = counties$hispanic,
                   "Percent Asian" = counties$asian)
    
    color <- switch(input$var,
                   "Percent White" = "darkgreen",
                   "Percent Black" = "black",
                   "Percent Hispanic" = "darkorange",
                   "Percent Asian" = "darkviolet")
    
    legend <- switch(input$var,
                    "Percent White" = "% White",
                    "Percent Black" = "% Black",
                    "Percent Hispanic" = "% Hispanic",
                    "Percent Asian" = "% Asian")
    
    
    
    percent_map(var = data,
                color = color,
                legend.title = legend,
                max = input$range[[2]],
                min = input$range[[1]])
  })
  
}

# Run the app -------------------------------------------------------------

shinyApp(ui = ui, server = server)