library(shiny)
library(G2Sd)
library(readxl)
library(tidyverse)

grancompat <-
  function(x)
  {
    x <- as.data.frame(x)
    n.sieve <- nrow(x)
    n.sample <- ncol(x)
    if (!is.data.frame(x)) 
      stop("dataframe expected.",call. = FALSE)
    if (any(x < 0))
      stop("negative entries in dataframe.",call. = FALSE)
    if (any(x > 300))
      warning("Some high values are present.", call. = FALSE,immediate.=TRUE)
    if (any(apply(x,2,sum)==0))
      stop("Some column sums equal to zero",call. = FALSE)
    else {return(x)}

    return(x)
  }

shinyServer(function(input, output) {
  output$contents <- renderTable({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    bdd_gran <- read.table(inFile$datapath, header=TRUE,sep=input$separator,dec=input$decimal,row.names=1)
    bdd_gran <- round(bdd_gran,3);bdd_gran <- grancompat(bdd_gran)
    
    output$stat <- renderTable({ 
      
      if (input$gran=='Statistics') {
        result <-granstat(bdd_gran,statistic=input$method)$stat
        
        output$downloadData <- downloadHandler(
          filename = "Statistics.csv",
          content = function(file){
            write.csv2(granstat(bdd_gran,statistic=input$method)$stat,"Statistics.csv",row.names=TRUE)
            #           
          })
      }
      if (input$gran=='Index') {
        result <-granstat(bdd_gran,statistic=input$method)$index
        
        output$downloadData <- downloadHandler(
          filename = "Index.csv",
          content = function(file){
            write.csv2(granstat(bdd_gran,statistic=input$method)$index,"Index.csv",row.names=TRUE)
            #           
          })
        #           
        
      }
      if (input$gran=='Texture') {
        result <-granstat(bdd_gran,statistic=input$method)$sedim$texture
        
        output$downloadData <- downloadHandler(
          filename = "Texture.csv",
          content = function(file){
            write.csv2(granstat(bdd_gran,statistic=input$method)$sedim$texture,"Texture.csv",row.names=TRUE)
            #           
            #           
          })
      }
      
      if (input$gran=='Sedim') {
        result <-granstat(bdd_gran,statistic=input$method)$sedim$descript
        
        output$downloadData <- downloadHandler(
          filename = "Sedim.csv",
          content = function(file){
            write.csv2(granstat(bdd_gran,statistic=input$method)$sedim$descript,"Sedim.csv",row.names=TRUE)
            #           
            #           
          })
      }
      
      
      if (input$gran=='All') {
        result <-granstat(bdd_gran,statistic=input$method)
        
        output$downloadData <- downloadHandler(
          filename = "output.zip",
          content = function(fname){
            fs <- c()
            tmpdir <- tempdir()
            setwd(tempdir())
            print (tempdir())
            
            fs <- c("Statistics.csv","Index.csv","Texture.csv","Sedim.csv")
            write.csv2(granstat(bdd_gran,statistic=input$method)$stat,"Statistics.csv",row.names=TRUE)
            write.csv2(granstat(bdd_gran,statistic=input$method)$index,"Index.csv",row.names=TRUE)
            write.csv2(granstat(bdd_gran,statistic=input$method)$sedim$texture,"Texture.csv",row.names=TRUE)
            write.csv2(granstat(bdd_gran,statistic=input$method)$sedim$descript,"Sedim.csv",row.names=TRUE)
            zip(zipfile=fname, files=fs)
          }, contentType = "application/zip"
           )
      }
      
      result 
      
    })
    
    
    output$plot1 <-renderPlot({
      
      if(input$from==input$to)
      granplot(bdd_gran,xc=input$from,hist=input$hist,cum=input$cum,main=names(bdd_gran)[input$from],cexname=1.2)
      if(input$from!=input$to)
        granplot(bdd_gran,xc=input$from:input$to,hist=input$hist,cum=input$cum,cexname=1.2)
      # output$downloadplot1 <- downloadHandler(
      #   filename = "histo_output.png",
      #   content = function(file) {
      #     png(file)})
      
    },height=600,width=600) 
    
    output$plot2 <-renderPlot({
      
      grandistrib(bdd_gran,scale=input$distritype,xlab = "")
      
    },height=600,width=800) 
    
    
    bdd_gran
    
    
    
    #   output$stat <- renderDataTable({
    #     
    #     result_gran <- granstat(input$gran,statistic=method,aggr=aggr)
    #     
    #   })
  })
})


