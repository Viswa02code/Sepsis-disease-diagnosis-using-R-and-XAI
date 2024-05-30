library(shiny)
library(shinydashboard)

explain_recommendation <- function(question, symptoms, history_of_sepsis, plasma_glucose_level, blood_pressure, blood_work_results) {
  if (grepl("exercise", tolower(question))) {
    return("Staying active through regular exercise can improve overall health and well-being.")
  } else if (symptoms == "Fever" && blood_pressure > 90) {
    return("Consider seeking immediate medical attention for fever and high blood pressure.")
  } else if (symptoms == "Fatigue" && plasma_glucose_level < 70) {
    return("Consider seeking immediate medical attention for fatigue and low blood glucose level.")
  } else if (symptoms == "Difficulty Breathing" && blood_work_results[1] > 100) {
    return("Consider seeking immediate medical attention for difficulty breathing and high blood work result-1.")
  } else if (history_of_sepsis) {
    return("I recommend maintaining a healthy lifestyle with a balanced diet, regular exercise, and sufficient sleep. Additionally, consider seeking medical attention if you have a history of sepsis.")
  } else {
    return("I recommend maintaining a healthy lifestyle with a balanced diet, regular exercise, and sufficient sleep.")
  }
}

ui <- fluidPage(
  titlePanel("Sepsis Diagnosis"),
  sidebarLayout(
    mainPanel(
      numericInput("user_age", "Age", value = 30),
      numericInput("user_plasma_glucose_level", "Plasma glucose Level", value = 100),
      numericInput("user_blood_pressure", "Blood Pressure (mm Hg)", value = 120),
      numericInput("user_blood_work_result1", "Blood Work Result-1 (mu U/ml)", value = 100),
      numericInput("user_blood_work_result2", "Blood Work Result-2 (mm)", value = 10),
      numericInput("user_blood_work_result3", "Blood Work Result-3 (mu U/ml)", value = 100),
      numericInput("user_blood_work_result4", "Blood Work Result-4 (mu U/ml)", value = 100),
      numericInput("user_body_mass_index", "Body mass index", value = 20),
      uiOutput("symptoms_ui"),
      checkboxInput("history_of_sepsis", "Do you have a history of sepsis?"),
      conditionalPanel(condition = "input.history_of_sepsis == true",
                       checkboxInput("insurance", "Do you have Insurance?")),
      actionButton("submit_button", "Submit")
    ),
    mainPanel(
      verbatimTextOutput("bot_response")
    )
  )
)

server <- function(input, output) {
  output$symptoms_ui <- renderUI({
    selectInput("symptoms", "Select Symptoms", choices = c("Fever", "Fatigue", "Difficulty Breathing"), selected = "Fever")
  })
  
  output$bot_response <- renderPrint({
    if (input$submit_button > 0) {
      user_question <- paste("Age:", input$user_age, "Plasma glucose Level:", input$user_plasma_glucose_level, "Blood Pressure (mm Hg):", input$user_blood_pressure, "Blood Work Result-1 (mu U/ml):", input$user_blood_work_result1, "Blood Work Result-2 (mm):", input$user_blood_work_result2, "Blood Work Result-3 (mu U/ml):", input$user_blood_work_result3, "Blood Work Result-4 (mu U/ml):", input$user_blood_work_result4, "Body mass index:", input$user_body_mass_index, sep = " ")
      
      recommendation <- explain_recommendation(user_question, input$symptoms, input$history_of_sepsis, input$user_plasma_glucose_level, input$user_blood_pressure, c(input$user_blood_work_result1, input$user_blood_work_result2, input$user_blood_work_result3, input$user_blood_work_result4))
      
      recommendation 
    }
  })
}
shinyApp(ui, server)


