options(shiny.maxRequestSize=30*1024^2)

if(getRversion() >= "2.15.1") utils::globalVariables(c('tsvm_model','gg_pr_Curve','gg_Cumulative_curve','gg_roc_curve','gg_lift_curve'))

shinyServer(function(input, output) {

  q <- observe({
    if (input$quit == 1) stopApp()
  })

  output$train_data <- renderDataTable({
    train_data <- input$train_data
    if(is.null(train_data)){
      return(NULL)
    }
    else{
      read.csv(file = train_data$datapath,header = input$header,sep = input$separator,quote = input$quote)
    }
  }
  )

  output$test_data <- renderDataTable({
    test_data <- input$test_data
    if(is.null(test_data)){
      return(NULL)
    }
    else{
      read.csv(file = test_data$datapath,header = input$header,sep = input$separator,quote = input$quote)
    }
  }
  )

  output$summary <- renderPrint({
    train_data <- input$train_data
    if(is.null(train_data)){
      return(NULL)
    }
    else{
      train_data <- as.data.frame(read.csv(file = train_data$datapath,header = T))
    }
    test_data <- input$test_data
    if(is.null(test_data)){
      return(NULL)
    }
    else{
      test_data <- as.data.frame(read.csv(file = test_data$datapath,header = T))
    }

    n1 <-NCOL(train_data)
    train_data[which(train_data[,n1]=='unknown'),n1] <- NA

    train_data[,n1] <- factor(train_data[,n1],levels=c('ok','fraud'))



    tsvm_model <<- svmlight(yuqi~.,data=train_data,temp.dir = "TSVM",pathsvm = "TSVM",type='C',svm.options = "-t 2 -g 0.1",out = TRUE,class.type='oaa')

    summary(tsvm_model)
  })


  output$pr_curve <- renderPlot({
    train_data <- input$train_data
    if(is.null(train_data)){
      return(NULL)
    }
    else{
      train_data <- as.data.frame(read.csv(file = train_data$datapath,header = T))
    }
    test_data <- input$test_data
    if(is.null(test_data)){
      return(NULL)
    }
    else{
      test_data <- as.data.frame(read.csv(file = test_data$datapath,header = T))
    }

    test_data_copy <- test_data
    n1 <- NCOL(train_data)

    pre <- predict(tsvm_model,test_data)

    predictions <- pre$posterior

    predictions <- predictions[,2]

    or <- order(predictions,decreasing = TRUE)

    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## predictions,labels
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    predictions <- predictions[or]

    labels <- test_data_copy[or,n1]

    labels <- ifelse(labels=='fraud',1,0)

    pred <- prediction(predictions,labels)


    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## precision/recall curve
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    pr_Curve <- performance(pred,'prec','rec')

    pr_Curve@y.values <- lapply(pr_Curve@y.values,function(x) rev(cummax(rev(x))))

    pr_Curve_data <- data.frame(as.data.frame(pr_Curve@x.values),as.data.frame(pr_Curve@y.values),row.names = NULL)

    names(pr_Curve_data) <- c('x','y')

    gg_pr_Curve  <<- ggplot(data=pr_Curve_data,mapping=aes(x=x,y=y))

    gg_pr_Curve <<- gg_pr_Curve + geom_line(colour=I('steelblue'),size=I(1.1))

    gg_pr_Curve <<- gg_pr_Curve+labs(x='Recall',y='Precision',title='Precision/Recall Curve')

    gg_pr_Curve <<- gg_pr_Curve+theme(plot.title=element_text(size=15,face='bold'),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15))

    print(gg_pr_Curve)
  })

  output$Cumulative_curve <- renderPlot({
    train_data <- input$train_data
    if(is.null(train_data)){
      return(NULL)
    }
    else{
      train_data <- as.data.frame(read.csv(file = train_data$datapath,header = T))
    }
    test_data <- input$test_data
    if(is.null(test_data)){
      return(NULL)
    }
    else{
      test_data <- as.data.frame(read.csv(file = test_data$datapath,header = T))
    }
    n1 <- NCOL(train_data)
    preds <- predict(tsvm_model,test_data[,1:(n1-1)],type="probability")
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## predictions,labels
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    pre <- as.data.frame(preds)

    or <- order(pre[,2],decreasing = TRUE)

    test <- test_data[or,n1]

    pre <- pre[or,2]

    pre <- data.frame(pre,test)

    predictions <- pre[,1]

    labels <- ifelse(pre[,2]=='fraud',1,0)

    pred <- prediction(predictions,labels)
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Cumulative_curve
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Cumulative_curve <- performance(pred,'rec','rpp')

    plot(Cumulative_curve)

    Cumulative_curve_data <- data.frame(as.data.frame(Cumulative_curve@x.values),as.data.frame(Cumulative_curve@y.values),row.names = NULL)

    names(Cumulative_curve_data) <- c('x','y')

    gg_Cumulative_curve  <<- ggplot(data=Cumulative_curve_data,mapping=aes(x=x,y=y))

    gg_Cumulative_curve <<- gg_Cumulative_curve + geom_line(colour=I('steelblue'),size=I(1.1))

    gg_Cumulative_curve <<- gg_Cumulative_curve+labs(x='Rate of positive predictions',y='Recall',title='Cumulative Recall Curve')

    gg_Cumulative_curve <<- gg_Cumulative_curve+theme(plot.title=element_text(size=15,face='bold'),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15))

    print(gg_Cumulative_curve)

  })

  output$roc_curve <- renderPlot({
    train_data <- input$train_data
    if(is.null(train_data)){
      return(NULL)
    }
    else{
      train_data <- as.data.frame(read.csv(file = train_data$datapath,header = T))
    }
    test_data <- input$test_data
    if(is.null(test_data)){
      return(NULL)
    }
    else{
      test_data <- as.data.frame(read.csv(file = test_data$datapath,header = T))
    }
    n1 <- NCOL(train_data)
    preds <- predict(tsvm_model,test_data[,1:(n1-1)],type="probability")
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## predictions,labels
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    pre <- as.data.frame(preds)

    or <- order(pre[,2],decreasing = TRUE)

    test <- test_data[or,n1]

    pre <- pre[or,2]

    pre <- data.frame(pre,test)

    predictions <- pre[,1]

    labels <- ifelse(pre[,2]=='fraud',1,0)

    pred <- prediction(predictions,labels)

    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## ROC_curve
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    roc_curve <- performance(pred,'tpr','fpr')

    plot(roc_curve)

    roc_curve_data <- data.frame(as.data.frame(roc_curve@x.values),as.data.frame(roc_curve@y.values),row.names = NULL)

    names(roc_curve_data) <- c('x','y')

    gg_roc_curve  <<- ggplot(data=roc_curve_data,mapping=aes(x=x,y=y))

    gg_roc_curve <<- gg_roc_curve + geom_line(colour=I('steelblue'),size=I(1.1))

    gg_roc_curve <<- gg_roc_curve+labs(x='False positive rate',y='True positive rate',title='ROC Curve')

    gg_roc_curve <<- gg_roc_curve+theme(plot.title=element_text(size=15,face='bold'),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15))

    print(gg_roc_curve)
  })

  output$lift_curve <- renderPlot({
    train_data <- input$train_data
    if(is.null(train_data)){
      return(NULL)
    }
    else{
      train_data <- as.data.frame(read.csv(file = train_data$datapath,header = T))
    }
    test_data <- input$test_data
    if(is.null(test_data)){
      return(NULL)
    }
    else{
      test_data <- as.data.frame(read.csv(file = test_data$datapath,header = T))
    }
    n1 <- NCOL(train_data)
    preds <- predict(tsvm_model,test_data[,1:(n1-1)],type="probability")
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## predictions,labels
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    pre <- as.data.frame(preds)

    or <- order(pre[,2],decreasing = TRUE)

    test <- test_data[or,n1]

    pre <- pre[or,2]

    pre <- data.frame(pre,test)

    predictions <- pre[,1]

    labels <- ifelse(pre[,2]=='fraud',1,0)

    pred <- prediction(predictions,labels)

    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Lift_curve
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    lift_curve <- performance(pred,'lift','rpp')

    plot(lift_curve)

    lift_curve_data <- data.frame(as.data.frame(lift_curve@x.values),as.data.frame(lift_curve@y.values),row.names = NULL)

    names(lift_curve_data) <- c('x','y')

    gg_lift_curve  <<- ggplot(data=lift_curve_data,mapping=aes(x=x,y=y))

    gg_lift_curve <<- gg_lift_curve + geom_line(colour=I('steelblue'),size=I(1.1))

    gg_lift_curve <<- gg_lift_curve+labs(x='Rate of positive predictions',y='Lift value',title='Lift Curve')

    gg_lift_curve <<- gg_lift_curve+theme(plot.title=element_text(size=15,face='bold'),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15))

    print(gg_lift_curve)
  })

  output$grid <- renderPlot({
    require(gridExtra)

    grid.arrange(gg_pr_Curve, gg_Cumulative_curve, gg_roc_curve, gg_lift_curve, ncol=2, nrow=2, widths=c(2,2), heights=c(2,2),main='Adaboost')
  })

  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('AdaboostReport', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },

    content = function(file) {
      cat('---
title: "',input$title,'"
author: "',input$author,'"
date: ',date(),'
output:', input$format,'_document
---
This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:
```{r,echo=T,prompt=T}
summary(tsvm_model)
```
```{r,echo=T,prompt=T}
plot_gg_pr_Curve()
plot_gg_Cumulative_curve()
plot_gg_roc_curve()
plot_gg_lift_curve()
plot_grid()
```       ',file='AdaboostReport.Rmd',append=F)

      out <- render('AdaboostReport.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )

  plot_gg_pr_Curve <- function(){
    print(gg_pr_Curve)
  }

  plot_gg_Cumulative_curve <- function(){
    print(gg_Cumulative_curve)
  }

  plot_gg_roc_curve <- function(){
    print(gg_roc_curve)
  }

  plot_gg_lift_curve <- function(){
    print(gg_lift_curve)
  }

  plot_grid <- function(){
    grid.arrange(gg_pr_Curve, gg_Cumulative_curve, gg_roc_curve, gg_lift_curve, ncol=2, nrow=2, widths=c(2,2), heights=c(2,2),main='Adaboost')
  }

  output$downloadData1 = downloadHandler(
    filename = function() {
      paste('pr_curve','.jpg', sep='')
    },
    content = function(file) {
      jpeg(file)
      plot_gg_pr_Curve()
      dev.off()
    },
    contentType='image/jpg')

  output$downloadData2 = downloadHandler(
    filename = function() {
      paste('pr_curve','.png', sep='')
    },
    content = function(file) {
      png(file)
      plot_gg_pr_Curve()
      dev.off()
    },
    contentType='image/png')

  output$downloadData3 = downloadHandler(
    filename = function() {
      paste('pr_curve','.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      plot_gg_pr_Curve()
      dev.off()
    },
    contentType='image/pdf')

  output$downloadData4 = downloadHandler(
    filename = function() {
      paste('cumulative_curve','.jpg', sep='')
    },
    content = function(file) {
      jpeg(file)
      plot_gg_Cumulative_curve()
      dev.off()
    },
    contentType='image/jpg')

  output$downloadData5 = downloadHandler(
    filename = function() {
      paste('cumulative_curve','.png', sep='')
    },
    content = function(file) {
      png(file)
      plot_gg_Cumulative_curve()
      dev.off()
    },
    contentType='image/png')

  output$downloadData6 = downloadHandler(
    filename = function() {
      paste('cumulative_curve','.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      plot_gg_Cumulative_curve()
      dev.off()
    },
    contentType='image/pdf')

  output$downloadData7 = downloadHandler(
    filename = function() {
      paste('roc_curve','.jpg', sep='')
    },
    content = function(file) {
      jpeg(file)
      plot_gg_roc_curve()
      dev.off()
    },
    contentType='image/jpg')

  output$downloadData8 = downloadHandler(
    filename = function() {
      paste('roc_curve','.png', sep='')
    },
    content = function(file) {
      png(file)
      plot_gg_roc_curve()
      dev.off()
    },
    contentType='image/png')

  output$downloadData9 = downloadHandler(
    filename = function() {
      paste('roc_curve','.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      plot_gg_roc_curve()
      dev.off()
    },
    contentType='image/pdf')
  output$downloadData10 = downloadHandler(
    filename = function() {
      paste('lift_curve','.jpg', sep='')
    },
    content = function(file) {
      jpeg(file)
      plot_gg_lift_curve()
      dev.off()
    },
    contentType='image/jpg')

  output$downloadData11 = downloadHandler(
    filename = function() {
      paste('lift_curve','.png', sep='')
    },
    content = function(file) {
      png(file)
      plot_gg_lift_curve()
      dev.off()
    },
    contentType='image/png')

  output$downloadData12= downloadHandler(
    filename = function() {
      paste('lift_curve','.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      plot_gg_lift_curve()
      dev.off()
    },
    contentType='image/pdf')
  output$downloadData13 = downloadHandler(
    filename = function() {
      paste('grid','.jpg', sep='')
    },
    content = function(file) {
      jpeg(file)
      plot_grid()
      dev.off()
    },
    contentType='image/jpg')

  output$downloadData14= downloadHandler(
    filename = function() {
      paste('grid','.png', sep='')
    },
    content = function(file) {
      png(file)
      plot_grid()
      dev.off()
    },
    contentType='image/png')

  output$downloadData15= downloadHandler(
    filename = function() {
      paste('grid','.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      plot_grid()
      dev.off()
    },
    contentType='image/pdf')

}
)
