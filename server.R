library(shiny)
library(ggplot2)
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
        #covariateVector <- c(2013,Sex,Age,variables %in% input$Comorbidities) - Means
        survival <- baseline$surv^(exp(sum(covariateVector*coefficients)))
        lowerSurvival <- baseline$lower^(exp(sum(covariateVector*lower)))
        upperSurvival <- baseline$upper^(exp(sum(covariateVector*upper)))
        
        binaryMeans <- replace(Means,Means>1,0)
        nullCovariates <- c(2013,Sex,Age,rep(0,length(comorbVector))) - Means
        nullModel <- baseline$surv^(exp(sum(nullCovariates*coefficients)))
        lowerNull <- baseline$lower^(exp(sum(nullCovariates*lower)))
        upperNull <- baseline$upper^(exp(sum(nullCovariates*upper)))
        return(data.frame(time=baseline$time/365.25,survival=survival,lower=lowerSurvival,upper=upperSurvival,nullModel=nullModel,upperNull=upperNull,lowerNull=lowerNull))
    })
    output$plot <- renderPlot({
        NoComorbidities <- factor("NoComorbidities")
        ggplot(data=calculate(),aes(time,survival)) + geom_line() + geom_line(aes(time,lower),linetype=2) +
            geom_line(aes(time,upper),linetype=2) + geom_line(aes(time,nullModel,col=NoComorbidities)) +
            geom_line(aes(time,lowerNull,col=NoComorbidities),linetype=2) +geom_line(aes(time,upperNull,col=NoComorbidities),linetype=2)+theme_bw() 
    })
    output$print <- renderPrint({

    })
    observeEvent(input$Diabetes.Mellitus,{
        if(input$Diabetes.end.organ.diagroup)
            updateCheckboxInput(session=session,inputId="Diabetes.Mellitus",label="Diabetes Mellitus",value=FALSE) 
    })
    observeEvent(input$Diabetes.end.organ.diagroup,{
        if(input$Diabetes.Mellitus)
            updateCheckboxInput(session=session,inputId="Diabetes.end.organ.diagroup",label="Diabetes end organ diagroup",value=FALSE) 
    })
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
    
    
})