library(shiny)

source("~/Desktop/github/wnba-elo/helper-functions.R")
source("app-helpers.R")


# load data ---------------------------------------------------------------

my_con <- connect_to_aws_db()

elo_val_df <- DBI::dbGetQuery(my_con, "SELECT * FROM wnba_elo_vals")

wnba_team_colors <- DBI::dbGetQuery(my_con, "SELECT * FROM wnba_team_colors")

elo_val_df <- elo_val_df %>% 
  inner_join(wnba_team_colors, by = c("team_club_code" = "club_code"))

club_codes <- sort(unique(elo_val_df$team_club_code))


# ui side -----------------------------------------------------------------

ui <- fluidPage(
  
  navbarPage("WNBA App",
             tabPanel(
               "Elo Component",
               tags$head(
                 tags$link(rel = "stylesheet", type = "text/css", href = "style-sheet.css")
               ),
               
               titlePanel("WNBA ELO Values"),
               
               sidebarLayout(
                 sidebarPanel(
                   #helpText("Show Team ELO Values Over Time"),
                   checkboxGroupInput("teams",
                                      label = "Select Teams: ",
                                      choices = club_codes,
                                      selected = "ATL"
                   )
                 ),
                 
                 mainPanel(
                   plotOutput("eloLinePlot")
                 )
               )
             ),
             tabPanel(
               "Playoff Predictions"
             )),
  
  
)



# server side -------------------------------------------------------------

server <- function(input, output) {
  
  output$eloLinePlot <- renderPlot({
    
    plot_elo_values(selected_teams = input$teams,
                    elo_vals = elo_val_df,
                    team_color_df = wnba_team_colors)
    
  })
  
}

shinyApp(ui, server)