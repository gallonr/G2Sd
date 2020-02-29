granstat <-
  function(x,phiSize=FALSE,web_interface=FALSE,statistic="all",modes=FALSE,
           FromLargetoSmall=TRUE){
    
    if(web_interface==TRUE){
      .G2Sd_web()}

    if(web_interface==FALSE){
    
    x <- .grancompat(x)
    
    if(any(as.numeric(row.names(x))==0)) 
      meshmin <- 1 else meshmin <- min(as.numeric(row.names(x)))
      
      STAT=c("arithmetic","geometric","folk.ward","all")
      statistic <- pmatch(statistic, STAT)
      
      if (phiSize) 
      {
        phi=as.numeric(row.names(x))
        row.names(x) <- .phi2um(phi)
      }
      
      if (modes==TRUE) .mode.sedim(x)
      
      index=.index.sedim(x,decreasing=FromLargetoSmall)
      texture=.texture.sedim(x)
      descript=.sedim.descript(x)
      
      if (statistic==1)
      {
        arith=.moment.arith(x)  
        result=new.env()
          result$stat=arith
          result$index=index
          result$sedim$texture=texture
          result$sedim$descript=descript
          result=as.list(result)
      }
      
      if (statistic==2)
      {
          geom=.moment.geom(x)
          result=new.env()
          result$stat=geom
          result$index=index
          result$sedim$texture=texture
          result$sedim$descript=descript
          result=as.list(result)
      }
      
      
      if (statistic==3)
      {
        fowa=.fowa.stat(x,decreasing=FromLargetoSmall)
          result=new.env()
          result$stat=fowa
          result$index=index
          result$sedim$texture=texture
          result$sedim$descript=descript
          result=as.list(result)
  
      }
      
      
      if (statistic==4)
      {
        arith=.moment.arith(x)
        geom=.moment.geom(x)
        fowa=.fowa.stat(x,decreasing=FromLargetoSmall)
          result=new.env()
          result$stat$arith=arith
          result$stat$geom=geom
          result$stat$fowa=fowa
          result$index=index
          result$sedim$texture=texture
          result$sedim$descript=descript
          result=as.list(result)
        }
      }
      return(result)
    
}


