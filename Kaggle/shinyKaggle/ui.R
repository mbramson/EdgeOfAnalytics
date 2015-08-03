library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Analytics Edge Kaggle Competition Playground"),
  
  sidebarPanel(
    numericInput("sparsity", "Term Sparsity", 0.97, min=0, max=1, step=0.0005),
    selectInput("AdditionalVars","Additional Variables", multiple=TRUE,
                choices=c("biddable","startprice","condition","cellular","carrier","color","storage","productline", "charcount","wordcount"),
                selected=c("biddable","startprice","productline")),
    numericInput("seed","Seed", 512, min=1, step=1)
  ),
  
  mainPanel(
    helpText("Baseline Accuracy"),
    verbatimTextOutput("BaseLineAccuracy"),
    h3(textOutput("termCount")),
    helpText("Terms Included in Dataset"),
    verbatimTextOutput("terms"),
    tabsetPanel(type="tabs",
      tabPanel("LogReg",
        numericInput("LogThreshold","Log Reg Threshold", 0.5, min=0, max=1, step=0.05),
        helpText("Logistic Regression Confusion Matrix"),
        tableOutput("LogConfusionMatrix"),
        helpText("Logistic Regression Accuracy"),
        verbatimTextOutput("LogAccuracyTrain"),
        helpText("Logistic Regression AUC"),
        verbatimTextOutput("Log_AUC"),
        plotOutput("Log_ROC_Curve"),
        verbatimTextOutput("LogSummary")   
      ),
      tabPanel("CART",
        numericInput("CartMinBucket","CART Minimum Buckets", 25, min=1, step=1),
        helpText("CART Confusion Matrix"),
        tableOutput("CARTConfusionMatrix"),
        helpText("CART Accuracy"),
        verbatimTextOutput("CARTAccuracyTrain"),
        plotOutput("CARTPlot"),
        helpText("CART AUC"),
        verbatimTextOutput("CART_AUC"),
        plotOutput("CART_ROC_Curve"),
        h3("Test Data"),
        helpText("Predicted Percent Sold in Test Data"),
        verbatimTextOutput("CARTTestSoldPercent")
      ),
      tabPanel("Random Forest",
        numericInput("nodesize","nodesize",10,min=1,step=1),
        numericInput("ntree","ntree",150,min=1,step=1),
        helpText("RF Confusion Matrix"),
        tableOutput("RFConfusionMatrix"),
        helpText("RF Accuracy"),
        verbatimTextOutput("RFAccuracyTrain"),
        helpText("RF AUC"),
        verbatimTextOutput("RF_AUC"),
        plotOutput("RF_ROC_Curve"),
        h3("Test Data"),
        helpText("Predicted Percent Sold in Test Data"),
        verbatimTextOutput("RFTestSoldPercent"),
        textInput("RFFileName","Output File Name:", value="RF_output.csv"),
        actionButton("RFOutput","Save Output")
      )
    )
  )
))