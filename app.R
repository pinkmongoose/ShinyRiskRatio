#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if (!("epiR" %in% installed.packages())) install.packages("epiR");
library(shiny)
library(epiR)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme="styles.css",
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

# Define server logic required to draw a histogram
server <- function(input, output) {
    options(shiny.error = function() {
        stop("Incomplete data.")
    })
    
    
    Model <- reactive({
        T <- matrix(c(input$DE,input$De,input$dE,input$de),c(2,2))
        #T <- table(T)
        colnames(T)<-rownames(T)<-c(TRUE,FALSE)
        E <- epi.2by2(T)
        F <- fisher.test(T)
        return(list(E=E,F=F))
    })
    
    output$rr <- renderText({
        signif(Model()$E$massoc.detail$RR.strata.wald$est,3)
    })
    output$pval <- renderText({
        signif(Model()$F$p.value,2)
    })
    
    output$E <- renderPrint({
        Model()$E
    })
    output$F <- renderPrint({
        Model()$F
    })
    
    output$plot <- renderPlot({
        ci <- Model()$E$massoc.detail$RR.strata.wald
        if (!is.finite(ci$lower)) return(NULL)
        if (!is.finite(ci$upper)) return(NULL)
        par(mar=c(3,3,3,0))
        plot(1,type="n",xlab="",ylab="",xlim=c(0,ci$upper*1.2),ylim=c(0,4),cex=2,main="risk ratio",axes=F,frame.plot=F)
        Axis(side=1,labels=T)
        segments(1,1,1,3,col="blue")
        segments(ci$lower,2,ci$upper,2,col="red")
        segments(ci$lower,1.8,ci$lower,2.2,col="red")
        segments(ci$upper,1.8,ci$upper,2.2,col="red")
        points(ci$est,2,type="o",pch=19,col="red",cex=3)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
