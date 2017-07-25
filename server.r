library(shiny)
library(epiR)
shinyServer(

  function(input,output) {
    
    options(shiny.error = function() {
      stop("Incomplete data.")
    })
    
    
    Model <- reactive({
      T <- matrix(c(input$DE,input$De,input$dE,input$de),c(2,2))
      T <- as.table(T)
      colnames(T)<-rownames(T)<-c(TRUE,FALSE)
      E <- epi.2by2(T)
      F <- fisher.test(T)
      return(list(E=E,F=F))
    })

    output$rr <- renderText({
      signif(Model()$E$res$RR.strata.wald$est,3)
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
      ci <- Model()$E$res$RR.strata.wald
      if (!is.finite(ci$lower)) return(NULL)
      if (!is.finite(ci$upper)) return(NULL)
      print(ci)
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
  

)
