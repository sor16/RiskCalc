#cat(paste("c(",paste(paste("\"",namesOfVariables,"\"=\"",namesOfVariables,"\"",sep=""),collapse=","),")",sep=""))
library(shiny)
library(ggplot2)
library(dplyr)
library(ggKaplanMeier)
library(survival)
library(magrittr)
setwd("~/Dropbox/MergaexliReiknir/RiskUI")
load("ShinyData.RData")
color <- "#52059f"
ui <- tagList(
    includeCSS("www/bootstrap.css"),
    navbarPage(title="Áhættureiknir",
    tabPanel("Myndir", icon = icon("stats", lib = "glyphicon"),
            column(width=4,
            #selectInput(inputId="Age",label="Select your age",choices=as.character(1:110),selected=60),
            textInput(inputId="Age",label="Select your age"),
            conditionalPanel(
            condition = "input.Age != ''",
            uiOutput("AgeRange")),
            selectInput(inputId="Sex",label="Select your sex",choices=c("Female","Male")),
            checkboxGroupInput(inputId="disease",label="",choices=c("Kransæðasjúkdómur"="Kransæðasjúkdómur",
                                                                "Sykursýki."="Sykursýki.",
                                                                "Hjartabilun"="Hjartabilun",
                                                                "Offita"="Offita",
                                                                "HTN."="HTN.",
                                                                "COPD"="COPD",
                                                                "Astmi"="Astmi",
                                                                "Heilabilun"="Heilabilun",
                                                                "Leiðnitruflanir.í.hjarta"="Leiðnitruflanir.í.hjarta",
                                                                "Hjartsláttartruflanir"="Hjartsláttartruflanir",
                                                                "Gáttaflökt"="Gáttaflökt",
                                                                "Sjálfsofnæmissjúkdómar"="Sjálfsofnæmissjúkdómar",
                                                                "Sjálfsofnæmissjd..Mótefnajákv."="Sjálfsofnæmissjd..Mótefnajákv.",
                                                                "Sjálfsofnæmissjúkd..Mótefnaneikv.."="Sjálfsofnæmissjúkd..Mótefnaneikv..",
                                                                "Krabbamein"="Krabbamein",
                                                                "Annar.blóðsjúkdómur"="Annar.blóðsjúkdómur",
                                                                "Heilaáföll"="Heilaáföll",
                                                                "Lifrarsjúkdómur"="Lifrarsjúkdómur",
                                                                "Nýrnasjúkdómur"="Nýrnasjúkdómur")),
            actionButton(inputId="Go","Submit")
        ),
        column(width=8,
            plotOutput("plot"),
            textOutput("print")
            #actionButton(inputId="backward","Previous Plot"),
            #actionButton(inputId="forward","Next Plot")
        )
    ),
    tabPanel("Töflur", icon = icon("table", lib = "font-awesome"),
            column(width=8,offset=4,
            tableOutput("table")
            )
    )
))
server <- function(input,output){
    plots <- reactiveValues(i=1,max=10,List=list())
    plotting <- eventReactive(input$Go,{
        req(input$Age)
        Sys.setlocale(category="LC_ALL",locale="is_IS")
        Sex <- switch(input$Sex,"Female" = 2,"Male" = 1)
        Age <- with(ShinyData,as.numeric(input$Age))
        if(is.null(input$disease)){
            namesOfDisease=rep(TRUE,nrow(ShinyData))
        }else{
            namesOfDisease=with(ShinyData,eval(parse(text=paste(input$disease,collapse=" & "))))
        }
        ShinyData <- ShinyData %>% filter(age < (input$AgeRange[2]) & age > (input$AgeRange[1]) & kon==Sex & namesOfDisease)
        if(nrow(ShinyData)<20){
             stop(paste("Not able to plot survival, only", nrow(ShinyData),"match these conditions."))
        }
        futime <- with(ShinyData,as.numeric(fudeath-diadat_case))
        survObject <- with(ShinyData,Surv(time=rep(0,nrow(ShinyData)),time2=futime,event=dead))
        fit=survfit(survObject~1,data=ShinyData)
        return(fit)
    })
    output$plot <- renderPlot({
        req(plotting())
        addrisk(gg_KM(plotting(),timeInYears=T,ticks="4x",colors=c("#52059f")))
    })
    output$print <- renderPrint({
        input$AgeRange[1]
    })
    output$table <- renderTable({
        
    })
    output$AgeRange <- renderUI({
        Age <- as.numeric(input$Age)
        ageRange <- c(Age-5, Age+5)
        sliderInput(inputId="AgeRange", label = "Age Range", min = 0, 
                    max = 100, value = ageRange)
    })
    # observeEvent(input$forward,{
    #     plots$i=ifelse(plots$i==10,plots$i%%plots$max+1,plots$i+1)
    #     plotting()
    # })
    # observeEvent(input$backward,{
    #     plots$i=ifelse(plots$i==1,plots$i+plots$max-1,plots$i-1)
    #     plotting()
    # })
    
}
shinyApp(server=server,ui=ui)