library(shiny)
library(ggplot2)
library(reshape2)
shinyServer(function(input,output,session){
    plots <- reactiveValues(i=1,max=10,List=list())
    calculate <- eventReactive(input$Go,{
        req(input$Age)
        baseline <- baseline[baseline$time < 10*365,]
        Sys.setlocale(category="LC_ALL",locale="is_IS")
        Sex <- input$Sex == "Female"
        Age <- as.numeric(input$Age)
        if(Age<0) return(NULL)
        comorbVector <- sapply(1:length(variables),function(i){
            input[[variables[i]]]  
        })
        covariateVector <- c(2013,Sex,Age,comorbVector) - Means
        survival <- baseline$surv^(exp(sum(covariateVector*coefficients)))
        
        binaryMeans <- replace(Means,Means>1,0)
        nullCovariates <- c(2013,Sex,Age,rep(0,length(comorbVector))) - Means
        nullModel <- baseline$surv^(exp(sum(nullCovariates*coefficients)))

        return(data.frame(time=baseline$time/365.25,survival=survival,nullModel=nullModel))
    })
    output$plot <- renderPlot({
        plotData <- melt(calculate(),id.vars="time")
        plotData$variable <- factor(plotData$variable)
        ggplot(data=plotData,aes(time,value,col=variable)) + geom_path() + theme_bw()
    })
    output$print <- renderPrint({
        NULL
    })
    # times <- reactiveValues(Dia=0,DiaGroup=0)
    # observeEvent(input$Diabetes.Mellitus,{
    #     times$Dia <- Sys.time() 
    #     if(times$Dia > times$DiaGroup){
    #         updateCheckboxInput(session=session,inputId="Diabetes.end.organ.diagroup",label="Diabetes end organ diagroup",value=FALSE)
    #         times$DiaGroup <- 0
    #     }
    # })
    # observeEvent(input$Diabetes.end.organ.diagroup,{
    #     times$DiaGroup <- Sys.time() 
    #     if(times$DiaGroup > times$Dia){
    #         updateCheckboxInput(session=session,inputId="Diabetes.Mellitus",label="Diabetes Mellitus",value=FALSE)
    #         times$Dia <- 0
    #     }
    # })
    # output$markdown <- renderUI({
    #     HTML(markdown::markdownToHTML(knitr::knit('About.Rmd', quiet = TRUE)))
    # })
    # output$Comorbidities <- renderUI({
    #     names(variables)= gsub("\\."," ",variables)
    #     checkboxgroupInput(inputId = "Comorbidities",label = "Select all that apply",choices = variables)
    # })
    
    output$Comorbidities <- renderUI({
        names(variables)= gsub("\\."," ",variables)
        lapply(1:length(variables),function(i){
            checkboxInput(inputId = variables[i],label = names(variables[i]))
        })
    })
    output$Info <- renderUI({
        
    })
    
    
})