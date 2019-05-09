library(shiny)
library(dplyr)
library(caret)
library(ggplot2)
library(ROCR)
library(e1071)


data <- read.csv("BC_Model.csv")

data2 <- mutate( data, diagnosis= as.factor(diagnosis))

data3 <- select(data2, diagnosis, age, race, area.mean)

nrows <- NROW(data3)

set.seed(218)
index <- sample(1: nrows, 0.7 * nrows)

BC.train <- data3[index,]
BC.test <- data3[-index,]

shinyServer(function(input, output) {
 
  # Build Logistic Regression Model 
  logit.model<- glm(BC.train$diagnosis~. , data = BC.train, family = binomial)
  
  predict.train<- predict(logit.model, type = "response")
  ROCRpred<- prediction(predict.train, BC.train$diagnosis)
  
  # build the ROC curve based on the predicito object, true pos rate, false pos rate (y & x)
  
  ROCRperf <- performance(ROCRpred, "tpr", "fpr")
  
     
  output$rocPlot <- renderPlot(plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7),
                                    main="ROC Plot for Logistic Regression Model"))
  
  # get predictions from Test Set
  predict.test<- predict(logit.model, type = "response", newdata = BC.test)
  
  # AUC
  ROCRpred <- prediction(predict.test, BC.test$diagnosis)
  output$auc <- renderText(paste("Model AUC:",
                                 performance(ROCRpred, "auc")@y.values[[1]][1]))
  
  ##########################################################
  # REACTIVE FUNCTION ON CHANGING SLIDEY_BAR FOR THRESHOLD #
  ##########################################################
  model.metrics <- reactive({
    threshold <- input$threshold
    # Update estimated out-of-sample accuracy, sensitivy, and specificity
    pred.test.class <- as.factor(predict.test > threshold)
    levels(pred.test.class) <- c("FALSE", "TRUE")
    label.test.class <- BC.test$diagnosis
    levels(label.test.class) <- c("FALSE", "TRUE")
    confusionMatrix(data=pred.test.class, reference=label.test.class, 
                    positive="TRUE")
  })
  
  #################################################################
  # Use the reactive function above to update Model Metrics based #
  # on Threshold.
  #################################################################
  output$accuracy <- renderText(paste("Model Accuracy:",
                                      model.metrics()[["overall"]]["Accuracy"]))
  output$sensitivity <- renderText(paste("Model Sensitivity:",
                                         model.metrics()[["byClass"]]["Sensitivity"]))
  output$specificity <- renderText(paste("Model Specificity:",
                                         model.metrics()[["byClass"]]["Specificity"]))
  
  ######################################
  
  pred.plot.data <- reactive({ 
    input$predBtn
    isolate({
      # predict from input  age, race, area.mean
      input.df <- data.frame(
                             age=input$age,
                             race=input$race,
                             area.mean=input$area.mean)
      # predict
      pred.input <- predict(logit.model, type="response", newdata=input.df)
      
      ######
      ######
      
      classLabel <- ifelse(pred.input > input$threshold, "Benignant", "Malignant")
      output$classByThreshold <- renderText(paste("Based on threshold of",
                                                  input$threshold, ",", 
                                                  "this patient with the characteristics given would be classified as", classLabel,
                                                  "of diagnosed with breast cancer."))
      # plot 
      probabilities <- data.frame(diagnosis =c("TRUE","FALSE"),
                                  Probability=c(pred.input, 1-pred.input),
                                  dummyX=c("diagnosed"))
      mutate(probabilities, midpts=cumsum(Probability)-0.5*Probability)
    })
    
  })
  # plot                        
  output$predPlot <- renderPlot(ggplot(data=pred.plot.data(), 
                                       aes(x=dummyX, y=Probability, fill=diagnosis)) +
                                  geom_bar(stat="identity", width=0.5) + 
                                  coord_flip() + 
                                  scale_fill_manual(values=c("darkorange","darkolivegreen3")) +
                                  theme(axis.text=element_blank(), axis.ticks=element_blank(), 
                                        axis.title.y=element_blank()) +
                                  geom_text(aes(label=format(Probability, digits=3), y=midpts), hjust=0.5) + 
                                  ggtitle(paste0("Predicted Probability of Breast Cancer \n",
                                                 "from Logistic Regression Model of BMC Study Patients"))
  )
  
  
})


