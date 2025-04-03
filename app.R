#This app show the flow plot and the current river crossing status
#It checks for data updates every 30 minutes

#Load any required libraries
list.of.packages <- c("shiny","shinydashboard","plotly","fresh","shinycssloaders")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages,repos='https://cloud.r-project.org')

librariesToLoad <- list.of.packages[!(list.of.packages %in% (.packages()))]
if(length(librariesToLoad)) sapply(librariesToLoad, library, character.only = TRUE)

source("AppFunctions.R")
HopeStageData <- reactive({
  invalidateLater(180000)
  GetHopeRiverAtGlynnWyeStage(Period="2_Days")
})

#Substitute in my custom colours for the named colours used by shinydashboard
mytheme <- create_theme(
  adminlte_color(
    green = 'rgba(87, 196, 173, 1)',
    orange ='rgba(237, 162, 71, 1)',
    red = 'rgba(219, 67, 37, 1)'
  ),
  adminlte_global(
    content_bg = "white"
  )
  
)

# Define UI that shows current coloured crossing status message
# and the latest stage plot from the Hope River

body <- dashboardBody(
  #Make sure my customised theme is used
  use_theme(mytheme),
  
  #Set out the app
  fluidPage(
    fluidRow(infoBoxOutput("StatusBox",width=12) %>% withSpinner()),
    fluidRow(
     div(class = "col-sm-12 col-md-8 col-lg-6",
     box(plotlyOutput("RiverPlot") %>% withSpinner(),
                width='100%',
                title= "River depth downstream",
                collapsible = TRUE,
                collapsed = TRUE))
    
  )
)
)


# Define server logic required to draw a plot
server <- function(input, output, session) {

  # set box attributes based on status
  box_status <- reactive({
    if (CrossingDanger(HopeStageData()) == "Danger") {
      c(title="Danger",
        message="Do not cross the Boyle",
        color='red',
        icon="thumbs-down")
    } else if (CrossingDanger(HopeStageData()) == "Caution"){
      c(title="Caution",
        message="Do not cross the Boyle from the Doubtful carpark, instead cross the Boyle north of the bluffs where the Boyle is braided, then use the Tui track and a marked route to crossing the Doubtful River where it too is braided",
        color='orange',
        icon="triangle-exclamation")
    } else {
      c(title="OK",
        message="You can probably cross the Boyle with care directly from the Doubtful carpark",
        color='green',
        icon="thumbs-up")
    }
  })
  
  output$StatusBox <- renderInfoBox({
    infoBox(
      box_status()['title'],box_status()['message'],icon=icon(box_status()['icon']),color=box_status()['color'], fill=TRUE
    )
  })
  output$RiverPlot <- renderPlotly({
    #reactive({
      invalidateLater(600000)
      GetHopeRiverAtGlynnWyeStage(Period="2_Weeks") %>%
        HopeRiverStageGraph()
    #})
    #HopeRiverStageGraph(HopeStageData()) 
  })
}

# Run the application 
shinyApp(ui = dashboardPage(dashboardHeader(disable=TRUE),dashboardSidebar(disable=TRUE),body=body), server)
#bslib::run_with_themer(shinyApp(ui = dashboardPage(dashboardHeader(disable=TRUE),dashboardSidebar(disable=TRUE),body=body), server))
