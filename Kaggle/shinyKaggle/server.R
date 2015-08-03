library(shiny)
library(tm)
library(ROCR)
library(rpart)
library(rpart.plot)
library(randomForest)

#Loading and Pre-Processing
Ebaytrain = read.csv("eBayiPadTrain.csv", stringsAsFactors=FALSE)
Ebaytrain$sold = factor(Ebaytrain$sold)
Ebaytest = read.csv("eBayiPadTest.csv", stringsAsFactors=FALSE)
Ebay <- rbind(subset(Ebaytrain, select=-c(sold)), Ebaytest)
Ebay$charcount <- nchar(as.character(Ebay$description))
Ebay$condition = factor(Ebay$condition)
Ebay$carrier = factor(Ebay$carrier)
Ebay$color = factor(Ebay$carrier)
Ebay$storage = factor(Ebay$storage)
Ebay$productline = factor(Ebay$productline)
Ebay$cellular = factor(Ebay$cellular)
corpus = Corpus(VectorSource(Ebay$description))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm <- DocumentTermMatrix(corpus)
Ebay$wordcount <- rowSums(as.matrix(dtm))

shinyServer(function(input,output) {
  
  output$BaseLineAccuracy <- renderText({
    max(c(mean(Ebaytrain$sold),1-mean(Ebaytrain$sold)))
  })
  #Data Frame containing terms and frequencies for each observation.
  descWords <- reactive({
    df = as.data.frame(as.matrix(removeSparseTerms(dtm, input$sparsity)))
    colnames(df) = make.names(colnames(df)) #Sanitize Terms
    df
    })
  
  #Output of Terms
  output$termCount <- renderText(paste(c(ncol(descWords()), "Terms")))
  output$terms <- renderText(names(descWords()))
  
  #Total Data Set
  Total <- reactive({
    df <- descWords()
    if ("biddable" %in% input$AdditionalVars) df$biddable = Ebay$biddable
    if ("startprice" %in% input$AdditionalVars) df$startprice = Ebay$startprice
    if ("condition" %in% input$AdditionalVars) df$condition = Ebay$condition
    if ("cellular" %in% input$AdditionalVars) df$cellular = Ebay$cellular
    if ("carrier" %in% input$AdditionalVars) df$carrier = Ebay$carrier
    if ("color" %in% input$AdditionalVars) df$color = Ebay$color
    if ("storage" %in% input$AdditionalVars) df$storage = Ebay$storage
    if ("productline" %in% input$AdditionalVars) df$productline = Ebay$productline
    if ("charcount" %in% input$AdditionalVars) df$charcount = Ebay$charcount
    if ("wordcount" %in% input$AdditionalVars) df$charcount = Ebay$wordcount
    df
  })
  
  #Base Training dataset
  train <- reactive({
    df <- head(Total(), nrow(Ebaytrain))
    df$sold = Ebaytrain$sold
    df
  })
  
  test <- reactive({
    tail(Total(), nrow(Ebaytest))
  })
  
  
  #------------------ Logistic Regression ----------------------------
  
  #Basic Logistic Regression Model
  DescWordsLog <- reactive(glm(sold ~ . , data=train(), family=binomial))
  #Sumamry of the Baseic Logistic Regression Model
  output$LogSummary <- renderPrint(summary(DescWordsLog()))
  
  #Predictions
  LogPredTrain <- reactive(predict(DescWordsLog(), newdata=train(), type="response"))
  LogPredTest <- reactive(predict(DescWordsLog(), newdata=test(), type="response"))
  #Confusion Matrix of the predictions for the Logistic Regression Model on the Training Set
  LogPredTableTrain <- reactive(table(train()$sold, LogPredTrain() >= input$LogThreshold))
  output$LogConfusionMatrix <- renderTable(LogPredTableTrain())
  #Accuracy of Log Regression on the Trainin Set
  output$LogAccuracyTrain <- renderText({
    (LogPredTableTrain()[1,1] + LogPredTableTrain()[2,2])/sum(LogPredTableTrain())
  })
  
  output$Log_AUC <- renderPrint({
    pred = prediction(LogPredTrain(), train()$sold)
    performance(pred, "auc")@y.values
  })
  output$Log_ROC_Curve <- renderPlot({
    pred = prediction(LogPredTrain(), train()$sold)
    perf = performance(pred,"tpr","fpr")
    plot(perf)
  })
  
  #----------------------- CART ----------------------------------
  
  #Do some Cross-Validation Here!
  
  #Cart Model
  DescWordsCART <- reactive(rpart(sold ~ ., data=train(), method="class", minbucket=input$CartMinBucket))
  output$CARTPlot <- renderPlot(prp(DescWordsCART()))
  
  #Prediction
  CARTPredTrain <- reactive(predict(DescWordsCART(), newdata=train(), type="class"))
  CARTPredTableTrain <- reactive(table(train()$sold, CARTPredTrain()))
  output$CARTConfusionMatrix <- renderTable(CARTPredTableTrain())
  
  #Test Data Prediction
  CARTPredTest <- reactive(predict(DescWordsCART(), newdata=test(), type="class"))
  output$CARTTestSoldPercent <- renderText(mean(CARTPredTest()==1))
  
  #Accuracy
  output$CARTAccuracyTrain <- renderText((CARTPredTableTrain()[1,1] + CARTPredTableTrain()[2,2])/sum(CARTPredTableTrain()))
  
  #ROC
  output$CART_AUC <- renderPrint({
    predictROC = predict(DescWordsCART(), newdata=train())
    pred = prediction(predictROC[,2], train()$sold)
    as.numeric(performance(pred, "auc")@y.values)
  })
  output$CART_ROC_Curve <- renderPlot({
    predictROC = predict(DescWordsCART(), newdata=train())
    pred = prediction(predictROC[,2], train()$sold)
    perf = performance(pred,"tpr","fpr")
    plot(perf)
  })
  
  #----------------------- Random Forest ------------------------------
  
  #RF Model
  DescWordsRF <- reactive({
    set.seed(input$seed)
    randomForest(sold ~ ., data=train(), nodesize=input$nodesize, ntree=input$ntree)
  })
  
  #Prediction
  RFPredTrain <- reactive(predict(DescWordsRF(), newdata=train(), type="prob")[,2])
  RFPredTableTrain <- reactive(table(train()$sold, RFPredTrain() >= 0.5))
  output$RFConfusionMatrix <- renderTable(RFPredTableTrain())
  RFPredTableTest <- reactive(table(RFPredTest()))
  output$RFTableTest <- renderTable(RFPredTableTest())
  
  #Test Data Prediction
  RFPredTest <- reactive(predict(DescWordsRF(), newdata=test(), type="prob")[,2])
  output$RFTestSoldPercent <- renderText(mean(RFPredTest()>=0.5))
  
  #Accuracy
  output$RFAccuracyTrain <- renderText((RFPredTableTrain()[1,1] + RFPredTableTrain()[2,2])/sum(RFPredTableTrain()))
  
  #ROC
  output$RF_AUC <- renderPrint({
    predictROC = predict(DescWordsRF(), newdata=train(), type="prob")[,2]
    pred = prediction(RFPredTrain(), train()$sold)
    as.numeric(performance(pred, "auc")@y.values)
  })
  output$RF_ROC_Curve <- renderPlot({
    predictROC = predict(DescWordsRF(), newdata=train(), type="prob")[,2]
    pred = prediction(RFPredTrain(), train()$sold)
    perf = performance(pred,"tpr","fpr")
    plot(perf)
  })
  
  SubmitRF <- observe({
    if(input$RFOutput == 0) return()
    FileName <- isolate(input$RFFileName)
    RFPred <- isolate(RFPredTest())
    MySubmission = data.frame(UniqueID = Ebaytest$UniqueID, Probability1 = RFPred)
    write.csv(MySubmission, FileName, row.names=FALSE)
  })
  
})