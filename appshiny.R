library(readxl)
library(shiny)
library(imputeTS)
library(dplyr)
library(plyr)
library(editData)

Dados<-read_excel("Tempo05junho.xlsx", sheet=1, col_names=TRUE)
attach(Dados)

source("funcaost.R")

ui <- fluidPage((h3(" Dados  do covid-19 - Unifal/MG", style = "color:blue", align = "center")),
                (h3(" Dados atualizados desde 25/02/2020 até 05/06/2020", style = "color:green", align = "center")),
    selectInput3("EG", label = "Espaço Geográfico", multiple = FALSE, choices = unique(regioes), selected = "MG",width = 230),
    selectInput3("tipo", label = "Indicador", choices =unique(tipo),width = 230),
    selectInput3("Dec",label="Tipo de gráfico",choices=unique(Decomposicao),width = 230),
    plotOutput("Covid19plot")
)

server <- function(input, output) {
    
    output$Covid19plot <- renderPlot({
        
        funct1(as.character(input$EG),as.character(input$tipo),as.character(input$Dec)) 
        funct2(as.character(input$EG),as.character(input$tipo),as.character(input$Dec))
        funct3(as.character(input$EG),as.character(input$tipo),as.character(input$Dec))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
