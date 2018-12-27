library(shiny)

shinyUI(fluidPage(
      titlePanel("Word Prediction Application"),
      sidebarLayout(
            sidebarPanel(
                  textInput("usertext",
<<<<<<< HEAD
                              "Write an incomplete sentence in all lower case here and hit the Predict button:",
                              value = "i want to go to the"),
                  submitButton("Predict")
            ),
            mainPanel(
                  fluidRow(
                        column(6, 
                  h3("Predicted Word"),
                  textOutput("pred.word")), 
                        column(6,
                  h3("Other Suggested Words"),
                  textOutput("sugg.words"))
                  ),
                  fluidRow(column(12,
                  h3("Frequency Plot of Other Possible Predicted Words from the Training Data"),
                  plotOutput("plot1")))
=======
                              "Write an incomplete sentence here:",
                              value = NULL)
            ),
            mainPanel(
                  h3("Top Predicted Word Choices (1-5 depending on data)"),
                  textOutput("predictions"), 
                  plotOutput("plot1")
>>>>>>> 057f9064ead60757160b226089d9a868800b102c
            )
      )
))