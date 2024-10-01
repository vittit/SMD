#


library(shiny)
library(bslib)
library(MBESS)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Standard Mean Deviation and Confidence Interval Calculator"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
      column(3,
       numericInput("Group_1_Mean", "Mean (Group1)", value = 0, min = 0.1, max = 1000),
       numericInput("Group_1_SD", "SD (Group1)", value = 0, min = 0.1, max = 1000),
       numericInput("Group_1_N", "N (Group1)", value = 0, min = 0.1, max = 1000),
      ),
      column(3,
    numericInput("Group_2_Mean", "Mean (Group2)", value = 0, min = 0.1, max = 1000),
    numericInput("Group_2_SD", "SD (Group2)", value = 0, min = 0.1, max = 1000),
    numericInput("Group_2_N", "N (Group2)", value = 0, min = 0.1, max = 1000),
      ),
      column(2,
    numericInput("CI", "Confidence Interval (%)", value = 95),
      ),

      column(12,
    #Create button for calculating 
    actionButton("Calculate", "Go"),
      ),
    
    column(12, 
    #Output 
    textOutput("SMD"),
    textOutput("Confidence_Interval")
    ),
  )
)

# Server
server <- function(input, output) {
  
  #Calculate the Standard Mean Difference using smd() from MBESS
  observeEvent(input$Calculate, {
    Standard_Mean_Difference <- smd(
      Mean.1 = input$Group_1_Mean,
      Mean.2 = input$Group_2_Mean,
      s.1 = input$Group_1_SD,
      s.2 = input$Group_2_SD,
      n.1 = input$Group_1_N,
      n.2 = input$Group_2_N
    )
    
    #Calculate Confidence Intervals 
    Confidence_Intervals <- ci.smd(
      smd = Standard_Mean_Difference,
      n.1 = input$Group_1_N,
      n.2 = input$Group_2_N,
      conf.level = input$CI / 100)
    
    #Output
    output$SMD <- renderText({
    paste("Standard Mean Difference: ", round(Standard_Mean_Difference, 2))
  })

  #Output
    output$Confidence_Interval <- renderText({
    paste("Confidence Interval: [", round(Confidence_Intervals$Lower.Conf.Limit.smd, 3),
          ",", round(Confidence_Intervals$Upper.Conf.Limit.smd, 3), "]")
   })
})

}

# Run the application 
shinyApp(ui = ui, server = server)
