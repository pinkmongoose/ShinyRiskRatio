library(shiny)
shinyUI(
  
  fluidPage(
    titlePanel("Epi 2 by 2 table explorer"),
    h4("Input data"),
    fluidRow(
      column(6,""),
      column(6,"Outcome")
    ),
    fluidRow(
      column(6,""),
      column(3,"(+)"),
      column(3,"(-)")
    ),
    fluidRow(
      column(3,"Exposure"),
      column(3,"(+)"),
      column(3,numericInput("DE",NULL,10,min=0,step=1)),
      column(3,numericInput("dE",NULL,5,min=0,step=1))
    ),
    fluidRow(
      column(3,""),
      column(3,"(-)"),
      column(3,numericInput("De",NULL,5,min=0,step=1)),
      column(3,numericInput("de",NULL,10,min=0,step=1))
    ),
    h4("Statistical analysis"),
    plotOutput("plot",height="150px"),
    tags$span("The risk ratio is", textOutput("rr",inline=T),". The probability of such a marked departure from a risk ratio of 1.0 occurring by chance is", textOutput("pval",inline=T), "(the P value)."),
    verbatimTextOutput("E"),
    verbatimTextOutput("F")
    
  )

)
