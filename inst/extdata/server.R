library(shiny)
library(G2Sd)

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
        result <-granstat(bdd_gran,statistic=input$method,aggr=FALSE)$stat
        
        output$downloadData <- downloadHandler(
          filename = "Statistics.csv",
          content = function(file){
            write.csv2(granstat(bdd_gran,statistic=input$method,aggr=FALSE)$stat,"Statistics.csv",row.names=TRUE)
            #           
          })
      }
      if (input$gran=='Index') {
        result <-granstat(bdd_gran,statistic=input$method,aggr=FALSE)$index
        
        output$downloadData <- downloadHandler(
          filename = "Index.csv",
          content = function(file){
            write.csv2(granstat(bdd_gran,statistic=input$method,aggr=FALSE)$index,"Index.csv",row.names=TRUE)
            #           
          })
        #           
        
      }
      if (input$gran=='Texture') {
        result <-granstat(bdd_gran,statistic=input$method,aggr=FALSE)$sedim
        
        output$downloadData <- downloadHandler(
          filename = "Texture.csv",
          content = function(file){
            write.csv2(granstat(bdd_gran,statistic=input$method,aggr=FALSE)$sedim,"Texture.csv",row.names=TRUE)
            #           
            #           
          })
      }
      if (input$gran=='All') {
        result <-granstat(bdd_gran,statistic=input$method,aggr=TRUE)
        
        output$downloadData <- downloadHandler(
          filename = "output.zip",
          content = function(fname){
            fs <- c()
            tmpdir <- tempdir()
            setwd(tempdir())
            print (tempdir())
            
            fs <- c("Statistics.csv","Index.csv","Texture.csv")
            write.csv2(granstat(bdd_gran,statistic=input$method,aggr=FALSE)$stat,"Statistics.csv",row.names=TRUE)
            write.csv2(granstat(bdd_gran,statistic=input$method,aggr=FALSE)$index,"Index.csv",row.names=TRUE)
            write.csv2(granstat(bdd_gran,statistic=input$method,aggr=FALSE)$sedim,"Texture.csv",row.names=TRUE)
            zip(zipfile=fname, files=fs)
          }, contentType = "application/zip"
           )
      }
      
      result 
      
    })
    
    
    output$plot1 <-renderPlot({
      
      par(mfrow=c(length(c(input$from:input$to)),1))  
      for (i in c(input$from:input$to))
        granplot(bdd_gran,xc=i,hist=input$hist,cum=input$cum,main=names(bdd_gran)[i],cexname=1.2)
      
      # output$downloadplot1 <- downloadHandler(
      #   filename = "histo_output.png",
      #   content = function(file) {
      #     png(file)})
      
    },height="auto",width=600) 
    
    output$plot2 <-renderPlot({
      
      grandistrib(bdd_gran,scale=input$distritype,xlab = "")
      
    },height="auto",width=800) 
    
    
    bdd_gran
    
    
    
    #   output$stat <- renderDataTable({
    #     
    #     result_gran <- granstat(input$gran,statistic=method,aggr=aggr)
    #     
    #   })
  })
})


