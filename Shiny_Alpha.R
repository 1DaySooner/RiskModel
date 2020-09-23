library('shiny')
source('Riskmodel_Alpha.R')

Micromortify<- function(Micromorts){
  mm_names = c("Driving from NYC to Los Angeles in a motorcycle", "Living Kidney Donation, (https://cjasn.asnjournals.org/content/14/4/597)", "Woman giving birth", "Woman giving birth twice", "Serving in the army Afganistan in 2009 for one week","Serving in the army Afganistan in 2009 for two weeks","Driving from NYC to Los Angeles and back in a motorcycle","Trucking for 1 year", "Logging for 1 year")
  mm_values = c(465, 300, 120,240,336,672,930,280,737)
  closest_answer = which(abs(mm_values-Micromorts)==min(abs(mm_values-Micromorts)))
  output_list = list(name=mm_names[closest_answer], num=mm_values[closest_answer])
}


# Adapted from Stack overflow user sebkopf, https://stackoverflow.com/questions/30502870/shiny-slider-on-logarithmic-scale
JS.logify <-
  "
// function to logify a sliderInput
function logifySlider (sliderId, sci = false) {
  if (sci) {
    // scientific style
    $('#'+sliderId).data('ionRangeSlider').update({
      'prettify': function (num) { return ('10<sup>'+num+'</sup>'); }
    })
  } else {
    // regular number style
    $('#'+sliderId).data('ionRangeSlider').update({
      'prettify': function (num) { return Math.round((Math.pow(10, num))); }
    })
  }
}"

# call logifySlider for each relevant sliderInput
JS.onload <-
  "
// execute upon document loading
$(document).ready(function() {
  // wait a few ms to allow other scripts to execute
  setTimeout(function() {
    // include call for each slider
    logifySlider('Simulations', sci = false)
  }, 5)})
"


ui <- fluidPage(
  tags$head(HTML("<style>
  div.shiny-text-output{
  	display: inline;
    padding: 0px;
    }</style>")
  ),
  #tags$head(tags$script(HTML(JS.logify))),
  #tags$head(tags$script(HTML(JS.onload))),
  title="Basic model for HCT Risk - Alpha Version",
  # App title ----
  titlePanel("Basic model for HCT Risk - Alpha Version"),
  # Sidebar layout with input and output definitions ----
  # Simulations = 1000, Participants=15, Age_Range='20 to 29'
  column(width=4,
         # Input: Slider for the number of bins ----
         #sliderInput(inputId = "Simulations",
         #             label = "Number of Simulations to Run:",
         #             min = 4,
         #             max = 7,
         #             value = 4,
         #             step=0.05),
         # Input: Slider for therapy ----
         sliderInput(inputId = "Therapy",
                     label = "Potentially Available Treatment (Percentage reduction in serious cases / deaths)",
                     min = 0,
                     max = .9,
                     value = 0,
                     step=0.05),
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
         h3("Model Outcomes"),
         "Based on the assumption that the population-level IFR and risk of serious cases are no higher for the study-population than reported in Salje et al. 2020, we can be 95% confident that the risk of a death during the study is below:", textOutput("deaths95"), "% (expected: ", textOutput("deaths_mean"), "%) and that the risk of any hospitalizations or serious cases during the study is below: ", textOutput("hospitalizations95"), "% (expected: ", textOutput("hospitalizations_mean"),"%)"
  ),
  column(12,"The expected risk to an individual is a ", textOutput("hospitalization_indiv"), "% chance of hospitalization, and a ", textOutput("death_indiv"), "% chance of death."),
  column(12,"Micromorts are a standard measure of risk of death. For example, driving a car on a trip for 250 miles has a 1-in-1-million risk of death, or one micromort. The risk of participating in the study involves ", textOutput("micromorts_indiv"), " micromorts, which is comparable to", textOutput("mm_name"), ", which involves ", textOutput("mm_num"),  " micromorts of risk."),
  #Link to NYtimes on micromorts from COVID-19: https://www.nytimes.com/2020/05/22/well/live/putting-the-risk-of-covid-19-in-perspective.html
  # Driving from NYC to Los Angeles in a motorcycle, 465 Micromorts (there and back, 930 Micromorts)
  # Living Kidney Donation: 300 micromorts (https://cjasn.asnjournals.org/content/14/4/597)
  # Giving birth to N children (120 micromorts each.)
  # Flying a mission for UK Bomber Command in WW2: 25,000 micromorts
  # Serving in the army in Afghanistan in 
  
  
  withTags({
    div(class="bodytext", checked=NA,
        h3("Methodology"),
        p("The methodology for this alpha version of the risk model is to use the suite of modeling analyses and data from Salje et al. 2020 to simulate a number of infections chosen, for the study size, above. The simulation is stochastic, so larger samples will be more accurate, but require longer to run. This does not account for the lower risk due to screening out comorbidities, or any risks that differ between intentional exposure trials and natural exposure. It also assumes no rescue therapy is applied, and does not include the greater clinical success of current COVID-19 treatment compared to the treatment available between March and the beginning of May, the time period covered by the Salje et al. data."),
        p("Note that this model is appropriate for considering a dosage trial. The size of such a trial may vary, but one approach would be to expose 5 volunteers each to 3 increasing titers of COVID-19, stopping if the majority or all volunteers for a given exposure level develop clinical disease. Following this, an additional 10-20 volunteers would be given the appropriate titer of COVID-19, to validate the initial result."),
        p("Salje, H., Kiem, C. T., Lefrancq, N., Courtejoie, N., Bosetti, P., Paireau, J., et al. & Le Strat, Y. (2020). Estimating the burden of SARS-CoV-2 in France. Science."))}
        )
  
)

Basicformatter<- function(Number, Mode='Pct'){
  if (Mode=='Pct'){
    return(paste0(prettyNum(Number*100, digits=3, format = "g", drop0trailing=TRUE),"%"))
  } else {
    # I want a heuristic way to generate an odds ratio that's "pretty" and fairly accurate.
  }
  
}



server <- function(input, output) {
  outcome_95 <- reactive(Simulate_StudyRisks(Participants=input$Participants, Age_Range=input$Age_Range, gender=input$Gender,Therapy=input$Therapy))
  outcome_mean <- reactive(Simulate_StudyRisks( Participants=input$Participants, Age_Range=input$Age_Range, gender=input$Gender, Therapy=input$Therapy, qtile=c(0.5)))
  micromorts <- reactive(round(IndivRiskPull(Age_Range=input$Age_Range, gender=input$Gender, outcome='death', Therapy=input$Therapy, Pctile="50%")*1000000))
  mm_ify = reactive(Micromortify(micromorts()))
  
  # output$deaths99 <- renderText(Basicformatter(Simulate_StudyRisks(Simulations = input$Simulations, Participants=input$Participants, Age_Range=input$Age_Range, gender=input$Gender,Therapy=input$Therapy,qtile=c(0.99))[1]))
  # output$hospitalizations99 <- renderText(Basicformatter(Simulate_StudyRisks(Participants=input$Participants, Age_Range=input$Age_Range, gender=input$Gender, Therapy=input$Therapy,qtile=c(0.99))[2]))
  
  output$deaths95 <- renderText(Basicformatter(outcome_95()[1]))
    output$deaths_mean <- renderText(Basicformatter(outcome_mean()[1]))
  output$hospitalizations95 <- renderText(Basicformatter(outcome_95()[2]))
    output$hospitalizations_mean <- renderText(Basicformatter(outcome_mean()[2]))
    
  output$hospitalization_indiv <- renderText(Basicformatter(IndivRiskPull(Age_Range=input$Age_Range, gender=input$Gender, outcome='hosp', Therapy=input$Therapy, Pctile="50%")))
  output$death_indiv <- renderText(Basicformatter(IndivRiskPull(Age_Range=input$Age_Range, gender=input$Gender, outcome='death', Therapy=input$Therapy, Pctile="50%")))

  output$micromorts_indiv <- renderText(micromorts())
  output$mm_name <- renderText(mm_ify()$name)
  output$mm_num <- renderText(mm_ify()$num)
}

#Run!
shinyApp(ui, server)
