library('shiny')
source('Riskmodel_Alpha.R')


ui <- fluidPage(
  title="Basic model for HCT Risk - Alpha Version",
  # App title ----
  titlePanel("Basic model for HCT Risk - Alpha Version"),
  # Sidebar layout with input and output definitions ----
  # Simulations = 1000, Participants=15, Age_Range='20 to 29'
  column(width=4,
         # Input: Slider for the number of bins ----
         sliderInput(inputId = "Simulations",
                     label = "Number of Simulations to Run:",
                     min = 100,
                     max = 10000,
                     value = 1000,
                     round=TRUE),
         sliderInput(inputId = "Participants",
                     label = "People in Trial:",
                     min = 10,
                     max = 100,
                     value = 15,
                     round=TRUE),
         radioButtons(inputId = "Gender",
                      label = "Gender of Participants:",
                      choiceNames=list("Male", "Female", "Both (Equal number)"),
                      choiceValues=c("m","f","b"),
                      selected="b"
                      ),
         radioButtons(inputId = "Age_Range",
                      label = "Ages of Participants:",
                      choices = c('20 to 29', '20 to 39'),
                      selected = '20 to 29'
                      )
         ),
  # Main panel for displaying outputs ----
  column(12,
         "Based on the assumption that the population-level IFR and risk of serious cases are no higher for the study-population than reported in Salje et al. 2020, we can be 95% confident that the risk of a death during the study is below:", textOutput("deaths"), "and that the risk of any hospitalizations / serious cases during the study is below:", textOutput("hospitalizations")
         
  )
)

server <- function(input, output) {
  Summary_Simulate_Studyrisks
  output$deaths <- renderText(Simulate_StudyRisks(Simulations = input$Simulations, Participants=input$Participants, Age_Range=input$Age_Range, gender=input$Gender)[1])
  output$hospitalizations <- renderText(Simulate_StudyRisks(Simulations = input$Simulations, Participants=input$Participants, Age_Range=input$Age_Range, gender=input$Gender)[2])
}


#Run!
shinyApp(ui, server)