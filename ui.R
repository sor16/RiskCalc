library(shiny)
load("ShinyData.RData")
list2env(ShinyData,envir = globalenv())
color <- "#52059f"
shinyUI(tagList(
    includeCSS("www/bootstrapNew.css"),
    navbarPage(title="MARC",
               tabPanel("Calculator", icon = icon("stats", lib = "glyphicon"),
                        column(width=3,
                               textInput(inputId="Age",label="Select your age"),
                               selectInput(inputId="Sex",label="Select your sex",choices=c("Female","Male")),
                               
                               tags$div(HTML("<h5> <b>Select all that apply </b></h5>")),
                               uiOutput("Comorbidities"),
                               actionButton(inputId="Go","Submit")
                        ),
                        column(width=9,
                               plotOutput("plot"),
                               textOutput("print")
                        )
               )
               # tabPanel("About", icon = icon("table", lib = "font-awesome"),
               #         column(width=8,offset=4,
               #         )
               # )
    )))