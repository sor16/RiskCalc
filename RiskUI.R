library(shiny)
library(ggplot2)
library(dplyr)
ui <- fluidPage(
    column(width=4,
        titlePanel("Áhættureiknir"),
        selectInput(inputId="Age",label="Select yout age",choices=as.character(1:100)),
        actionButton(inputId="backward","Previous Plot"),
        actionButton(inputId="forward","Next Plot")
    ),
    column(width=8,
        plotOutput("plot")
    )
)
server <- function(input,output){
    plots=reactiveValues(i=1,max=10)
    plotting <- reactive({
        samples=replicate(10,sample_n(diamonds,100))
        plotList=lapply(1:10,function(i) ggplot(data=as.data.frame(samples[,i]),aes(carat,price)) +
                            geom_point() +theme_bw() + labs(title=(paste("Plot nr.",i))))
    })
    plotNr <- reactive({
        plotList[[plots$i]]
    })
    output$plot <- renderPlot({
        plotNr()
    })
    observeEvent(input$forward,{
        plots$i=ifelse(plots$i==10,plots$i%%plots$max+1,plots$i+1)
        plotNr()
    })
    observeEvent(input$backward,{
        plots$i=ifelse(plots$i==1,plots$i+plots$max-1,plots$i-1)
        plotNr()
    })
    
}
shinyApp(server=server,ui=ui)