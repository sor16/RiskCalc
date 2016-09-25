library(shiny)
library(ggplot2)
library(reshape2)
Sys.setlocale(category="LC_ALL",locale="is_IS")
shinyServer(function(input,output,session){
    calculate <- eventReactive(input$Go,{
        req(input$Age)
        baseline <- baseline[baseline$time < 10*365,]
        Sex <- input$Sex == "Male"
        Age <- as.numeric(input$Age)
        if(Age<0) return(NULL)
        comorbVector <- sapply(1:length(variables),function(i){
            input[[variables[i]]]  
        })
        MARCAge <- ifelse(Age >= 60,trunc(Age/10)*10-50,0)
        MARCCovariates <- c(2013,Sex,MARCAge,comorbVector)
        MARCScore <- sum(MARCCovariates * MARCCoefficients)
        StatusNames <- c("Low Risk","Medium Risk","High Risk")
        StatusIndex <- 1*(MARCScore < 7) + 2*(MARCScore >= 7 & MARCScore <= 12) + 3*(MARCScore>12)
        Status <- StatusNames[StatusIndex]
        
        covariateVector <- c(2013,Sex,Age,comorbVector) - Means
        survival <- baseline$surv^(exp(sum(covariateVector*coefficients)))
        
        nullCovariates <- c(2013,Sex,Age,rep(0,length(comorbVector))) - Means
        nullModel <- baseline$surv^(exp(sum(nullCovariates*coefficients)))

        return(list(Status=Status,MARCScore=MARCScore,survData = data.frame(time=baseline$time/365.25,survival=survival,nullModel=nullModel)))
    })
    output$plot <- renderPlot({
        plotData <- melt(calculate()$survData,id.vars="time")
        plotData$variable <- factor(plotData$variable)
        levels(plotData$variable) = c("Subject","Same Age, No Comorbodities")
        ggplot(data=plotData,aes(time,value,col=variable)) + geom_path() + theme_bw() + ylab("Survival") +
            xlab("Time") + ggtitle("Comparison of Survival of Subject and of a Patient the Same Age with no Comorbidities")
    })
    
    output$Comorbidities <- renderUI({
        names(variables)= gsub("\\."," ",variables)
        lapply(1:length(variables),function(i){
            checkboxInput(inputId = variables[i],label = names(variables[i]))
        })
    })
    output$Info <- renderUI({
        HTML(
            paste("<h3>","MARC Score : ", calculate()$MARCScore,"<h3>",sep=""),
            paste("<h3>","Status : ", calculate()$Status,"<h3>",sep="")
        )
    })
    
    
})