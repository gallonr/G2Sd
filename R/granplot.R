granplot <-
function(x,xc=1,meshmin=1,hist=TRUE,cum=TRUE,main="",col.cum="red",
         col.hist="darkgray",cexname=0.9,cexlab=1.3,decreasing=FALSE) 
  {

    x <- x[order(as.numeric(row.names(x)),decreasing=decreasing),]
    
#     x <- as.data.frame(x)    
    um <- as.numeric(row.names(x))
    if (!is.na(pmatch(0,um)) ) um[pmatch(0,um)]=meshmin
    
    
if (length(xc)==1)
{
sum.sieve = sum(x[, xc])
class.weight = (x[, xc] * 100)/sum.sieve
class.weight.cum = round(cumsum(class.weight),2)
weight.gran <- data.frame(um=um,weight=class.weight,
                          weight.cum=class.weight.cum) 
hist.cum.plot <- ggplot(weight.gran)+
  theme(panel.background = element_blank(),
        panel.grid = element_line(colour="gray"),
        axis.title=element_text(size = 13,face="bold"),
        axis.text=element_text(size = 11,colour="black"),
        strip.text.x = element_text(size = rel(1.0), face = "bold"),
        axis.ticks=element_line(colour="black"),
        axis.line=element_line(colour="black"))+
  labs(title=names(granulo)[xc])

if (hist == TRUE & cum == TRUE) {

  if (decreasing)
{
    hist.cum.plot <- hist.cum.plot+
    geom_bar(aes(x=um,y=weight),stat="identity",fill=col.hist)+
    geom_line(aes(x=um,y=weight.cum),colour=col.cum,size=1.5)+
    ylab("Weight (%)")+xlab( expression(log[10](Particule~size)))+
    scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Percentage cum.(%)"))+
    scale_x_continuous(trans=.reverselog_trans(10))
  } else {
    hist.cum.plot <- hist.cum.plot+
    geom_bar(aes(x=um,y=weight),stat="identity",fill=col.hist)+
    geom_line(aes(x=um,y=weight.cum),colour=col.cum,size=1.5)+
    ylab("Weight (%)")+xlab( expression(log[10](Particule~size)))+
    scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Percentage cum.(%)"))+
    scale_x_log10()}

print(hist.cum.plot)
    

}
if (hist == FALSE & cum == TRUE) {
 
  if (decreasing)
  {
    hist.cum.plot <- hist.cum.plot+
      geom_line(aes(x=um,y=weight.cum),colour=col.cum,size=1.5)+
      ylab("Percentage cum.(%)")+xlab( expression(log[10](Particule~size)))+
    scale_x_continuous(trans=.reverselog_trans(10))
  } else {
    hist.cum.plot <- hist.cum.plot+
      geom_line(aes(x=um,y=weight.cum),colour=col.cum,size=1.5)+
      ylab("Percentage cum.(%)")+xlab( expression(log[10](Particule~size)))+
      scale_x_log10()}
  
  print(hist.cum.plot)
}

if (hist == TRUE & cum == FALSE) {
  if (decreasing)
  {
    hist.cum.plot <- hist.cum.plot+
      geom_bar(aes(x=um,y=weight),stat="identity",fill=col.hist)+
      ylab("Weight (%)")+xlab( expression(log[10](Particule~size)))+
      scale_x_continuous(trans=.reverselog_trans(10))
  } else {
    hist.cum.plot <- hist.cum.plot+
      geom_bar(aes(x=um,y=weight),stat="identity",fill=col.hist)+
      ylab("Weight (%)")+xlab( expression(log[10](Particule~size)))+
      scale_x_log10()}
  
  print(hist.cum.plot)
}
}

if (length(xc)!=1)
{
  class.weight = sapply( x[,xc], function(x){ (x/sum(x, na.rm=TRUE))*100})
  class.weight.cum = as.data.frame(round(apply(class.weight,2,cumsum),2));row.names(class.weight.cum) <- row.names(x)
  class.weight.cum <- melt(t(class.weight.cum ),id=names(t(class.weight.cum )))
  names(class.weight.cum) <- c("var","gsize","value")
  
  
    p <- ggplot(class.weight.cum,aes_string(x="gsize",y="value",shape="var",col="var"))+geom_line(size=2)+
      theme_bw()+labs(x=expression(log[10](Particule~size)), y="Percentage cum.(%)",title=main)+
    theme( plot.title = element_text(size = rel(1.5), colour = "black",face="bold"),
      axis.title.y = element_text(size = 13, face = "bold"),
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.text.y = element_text(size = 11, face = "bold"),
          axis.text.x = element_text(size = 11,face="bold"))+scale_colour_hue("Stations")+
      scale_x_log10()
  print(p)
}
}
