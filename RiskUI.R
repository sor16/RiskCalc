#cat(paste("c(",paste(paste("\"",namesOfVariables,"\"=\"",namesOfVariables,"\"",sep=""),collapse=","),")",sep=""))
library(shiny)
library(ggplot2)
library(dplyr)
library(ggKaplanMeier)
library(survival)
setwd("~/Dropbox/MergaexliReiknir/RiskUI")
load("ShinyData.RData")
color <- "#52059f"
ui <- tagList(
    includeCSS("www/bootstrap.css"),
    navbarPage(title="Áhættureiknir", theme="bootstrap.css",
    tabPanel("Myndir", icon = icon("stats", lib = "glyphicon"),
             sidebarPanel(width=4,
            selectInput(inputId="Age",label="Select your age",choices=as.character(1:110),selected=60),
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
        Sex <- switch(input$Sex,"Female" = 1,"Male" = 2)
        Age <- as.numeric(input$Age) 
        ShinyData <- ShinyData %>% filter(ageRange==round(Age/10)*10 & kon==Sex & as.numeric(fudeath-diadat_case)<365.25*10) %>% filter()
        if(nrow(ShinyData)<20){
             return(NULL)
        }
        survObject <- with(ShinyData,Surv(time=rep(0,nrow(ShinyData)),time2=as.numeric(fudeath-diadat_case),event=dead))
        fit=survfit(survObject~1,data=ShinyData)
        return(fit)
    })
    output$plot <- renderPlot({
        req(plotting())
        addrisk(gg_KM(plotting(),timeInYears=T,ticks="4x",colors=c("#52059f")))
    })
    output$print <- renderPrint({
        input$disease
    })
    output$table <- renderTable({
        
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