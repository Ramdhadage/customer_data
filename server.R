
library(shiny)
library(ggplot2)
library(tidyverse)
library(caret)
server <- function(input, output, session) {
  data1 <- read.csv(file.path(getwd(), "customer_data.csv"), header=TRUE, stringsAsFactors=FALSE)
  attach(data1)


  output$Menu1Tab1Plot1 <- renderPlot({
    ggplot(data1, aes(x=education)) + geom_bar(fill="Blue") + labs(x="Team")
  })
  output$Menu1Tab1Plot2 <- renderPlot({
    ggplot(data1, aes(x=gender, y=spending)) + geom_boxplot(fill='steelblue')
  })

  cols <- c("#66CC99", "#F76D5E", "#FFFFBF", "#72D8FF" )
  output$Menu1Tab1Plot3 <- renderPlot({

    # Basic density plot in ggplot2
    ggplot(data1, aes(x = age, colour = gender)) +
      geom_density(lwd = 1.2, linetype = 1) +
      scale_color_manual(values = cols)
  })

  var1=income
  output$summaryOutput1 <- renderPrint({
    summary(var1)
  })

  variance=var(var1)
  output$varianceText1 <- renderText({
    variance
  })

  output$SDText1 <- renderText({
    std=sqrt(variance)
    std
  })

  var1=income
  mn=mean(var1)
  output$MeanText1 <- renderText({
    mn
  })

  vr1=var(var1)
  std=sqrt(vr1)

  output$SQRTText1 <- renderText({
    std
  })

  ll=mn-std
  ll
  ul=mn+std
  ul
  output$Menu1_Tab3_Plot1_1 <- renderPlot({
    plot(var1)
    abline(h=ll,col='red')
    abline(h=ul,col='red')
  })

  var1=income
  output$Menu1_Tab4_Plot1_1 <- renderPlot({
    boxplot(var1)
  })

  q1=quantile(var1,0.25)
  #q1

  q3=quantile(var1,0.75)
  #q3

  iqr=q3-q1
  ll=q1-1.5*iqr
  ul=q3+1.5*iqr

  output$Menu1_Tab4_Plot2_1 <- renderPlot({
    plot(var1)
    abline(h=ll,col='red')
    abline(h=ul,col='red')
  })
  #--------------------------------
  set.seed(1)
  #use 70% of dataset as training set and 30% as test set
  sample <- sample(c(TRUE, FALSE), nrow(data1), replace=TRUE, prob=c(0.7,0.3))
  train  <- data1[sample, ]
  test   <- data1[!sample, ]


  # Building the model

  #train the model by using age column as target variable and other as independent variables
  model_age = lm(age ~ income + purchase_frequency + spending,  data = train)

  output$MCText1_1 <- renderPrint({
    est_param
  })

  # predicting the target variable
  model_age_pred <- predict(model_age, test)
  output$myTable1_1 <-renderPrint({
    pred
  })
  # output$myTable1_1 <- DT::renderDataTable({
  #   data.frame( R2 = R2(model_age_pred, test $ age),
  #               RMSE = RMSE(model_age_pred, test $ age),
  #               MAE = MAE(model_age_pred, test $ age))
  # })


  #train the model by assigning people_review column as target variable and other as independent variables
  model_income = lm(income ~ age + purchase_frequency + spending,  data = train)
  output$MCText2_1 <- renderPrint({
    model_income$coefficients
  })
  # predicting the target variable
  model_income_pred <- predict(model_income, test)
  # computing model performance metrics
  output$myTable2_1 <- DT::renderDataTable({
    data.frame( R2 = R2(model_income_pred, test $ income),
                RMSE = RMSE(model_income_pred, test $ income),
                MAE = MAE(model_income_pred, test $ income))
  })



  #train the model by assigning offer_price column as target variable and other as independent variables
  model_purch = lm(purchase_frequency ~ income + age + spending,  data = train)
  output$MCText3_1 <- renderPrint({
    model_purch$coefficients
  })

  # predicting the target variable
  model_purch_pred <- predict(model_purch, test)
  # computing model performance metrics
  output$myTable3_1 <- DT::renderDataTable({
    data.frame( R2 = R2(model_purch_pred, test $ purchase_frequency),
                RMSE = RMSE(model_purch_pred, test $ purchase_frequency),
                MAE = MAE(model_purch_pred, test $ purchase_frequency))
  })


  #train the model by assigning real_price column as target variable and other as independent variables
  model_spend = lm(spending ~ income + purchase_frequency + age ,  data = train)
  output$MCText4_1 <- renderPrint({
    model_spend$coefficients
  })
  # predicting the target variable
  model_spend_pred <- predict(model_spend, test)
  # computing model performance metrics
  output$myTable4_1 <- DT::renderDataTable({
    data.frame( R2 = R2(model_spend_pred, test $ spending),
                RMSE = RMSE(model_spend_pred, test $ spending),
                MAE = MAE(model_spend_pred, test $ spending))
  })

  var1=gender
  var2=education

  output$Menu4_Tab1_1 <- renderPrint({
    chisq.test(var1,var2)
  })

  var2=gender

  n=length(unique(var2))

  k=as.matrix(table(var2))

  output$Menu4_Tab2_1 <- renderPrint({
    chisq.test(k,p=rep(1/n,n))
  })


  var1=income
  mean_=60000 #user input//s

  observeEvent(input$mu_value_1, {
    if(input$mu_value_1 == ""){
      output$Menu4_Tab3_1 <- renderPrint({
        "Enter mu Value to get the results."
      })
    }else{
      mean1 <- as.numeric(input$mu_value_1)
      output$Menu4_Tab3_1 <- renderPrint({
        t.test(var1,mu=mean1, conf.level = 0.95)
      })
    }
  })




}
