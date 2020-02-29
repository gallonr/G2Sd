grandistrib <-
function (x, main="", scale = "fine", xlab = "Stations", ylab = "Percentage") 
{
  if (scale == "fine") 
    {
    Descript <- .sedim.descript(x)
    Descript <- Descript %>% gather(-samples,key="class",value="value")
    Descript$class <- factor(Descript$class,c("boulder","vcgravel","cgravel","mgravel","fgravel","vfgravel","vcsand","csand","msand","fsand","vfsand","vcsilt","silt"),ordered=T)
  }
    
  if (scale == "large") 
  {
    Descript <- .texture.sedim(x)
    Descript <- Descript %>% gather(-c(samples,texture),key="class",value="value")
    Descript$class<- factor(Descript$class,c("Boulder","Gravel","Sand","Mud"),ordered=T)
    }
    
  p <- ggplot(Descript,aes(x=samples,y=value,fill=class))+geom_bar(stat="identity",position="stack")+
    theme_bw()+scale_fill_discrete(rainbow(length(levels(Descript$class))))+
    xlab(xlab)+ylab(ylab)+ggtitle(main)+
    guides(fill=guide_legend(title="Classes"))+
    theme(axis.title = element_text(face="bold",size=13),
          axis.text = element_text(size=11),
          legend.text = element_text(size=11),
          legend.title = element_text(face="bold",size=11))
  
  print(p)
}

