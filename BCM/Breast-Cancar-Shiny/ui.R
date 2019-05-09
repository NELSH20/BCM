
library(shiny)
library(shinythemes)

instructions.sidebar.1 <- paste("This Shiny app will classify a patient as being Benign or Malignant of ", 
                                "being diagnosed with Breast cancer .")
instructions.sidebar.2 <- paste("To obtain a prediction, provide the patient's characteristics through the", 
                                "inputs on this sidebar and click the 'Predict' button.")
instructions.predict.1 <- paste("The figure below shows the predicted probability a patient with the", 
                                "given characteristics will be diagnosed with cancer.")
instructions.predict.2 <- paste("Based on the provided threshold, a statement below the figure will classify",
                                "the patient as Benign or Malignant of Breast Cancer.")
instructions.model.1 <- paste("The figure below shows the ROC curve for the logistic regression model",
                              "used for predictions. Numbers below shows the performance metrics for the model.")
instructions.model.2 <- paste("Adjust the threshold through the slide-bar on the side panel on the left",
                              "to see the impact to accuracy, sensitivity, and specificity.")

# Define UI for application that draws a histogram
shinyUI(fluidPage( theme = shinytheme("superhero"),
  titlePanel("Predicting Breast Cancer diagnosis(Benign/Malignant)"),
  sidebarLayout(
    sidebarPanel(
      helpText(p(instructions.sidebar.1), p(instructions.sidebar.2)),
      sliderInput("threshold",
                  label="Threshold:",
                  min=0.0, 
                  max=1.0, 
                  value=0.4,
                  step=0.1),
      numericInput("age",
                   label="Age (integers only between 12-40):",
                   value=25,
                   min=12,
                   max=40, 
                   step=1),
      sliderInput("race",
                  label="Race: Asian: 1, Hispanic = 2,  Black = 3, Other = 4, White = 5",
                  min=0.0, 
                  max=5.0, 
                  value=3.0,
                  step=1.0),
      numericInput("area.mean",
                  label="Area Mean:",
                  value=300,
                  min=0,
                  max=500, 
                  step=1),
      actionButton("predBtn", "Predict")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Prediction",
               h3("Probability of Being Benign or Malignant"),
               p(instructions.predict.1), 
               p(instructions.predict.2),
               plotOutput("predPlot"),
               p(textOutput("classByThreshold"))
      )
      #   tabPanel("Model Info", 
      #            h3("ROC Curve for Logistic Regression Model"),
      #            p(instructions.model.1), 
      #            p(instructions.model.2),
      #            plotOutput("rocPlot"),
      #            p("Adjust Threshold slider input on the left Sidebar to obtain desired accuracy, sensitivity, and specificity"),
      #            p(textOutput("accuracy")),
      #            p(textOutput("sensitivity")),
      #           p(textOutput("specificity")),
      #            p(textOutput("auc"))
    )
  )
)
)
)