utils::globalVariables(c("runApp", "meshsize", "percentile","phi",'samples',
                         "D5","D10","D16","D25","D50","D75","D84","D90","D95",
                         "mean.fw.phi","sd.fw.phi","skewness.fw.phi","kurtosis.fw.phi",
                         "mean.descript","sorting","skewness","kurtosis","value","relative.value",
                         "meshsize.midpoint","mean.arith.um","sd.arith.um","mean.geom.um","sd.geom.um",
                         "cum.sum","phi.max","phi.min","ratio","Sand","Mud","sandmud","Gravel",
                         "granulo","weight","weight.cum","value.relative","texture",
                          "cgravel","csand","fgravel","fsand","mgravel","msand","silt","vcgravel",
                          "vcsand","vcsilt","vfgravel","vfsand"))





.um2phi <- function(um)
{return(-log2(um/1000))}

.phi2um <- function(phi)
{2^(-phi)*1000}

.fowa.stat <-   function(x,decreasing){
    x=as.data.frame(x)
    mat.D <- .percentile(x,decreasing)
    mat.D$percentile <- paste("D",mat.D$percentile,sep="")
    all.phi <- mat.D %>% select(-meshsize) %>% spread(percentile,phi)
    all.um <- mat.D %>% select(-phi) %>% mutate(meshsize=meshsize/1000) %>% spread(percentile,meshsize)
    
    all.phi <- all.phi %>% group_by(samples) %>% mutate(mean.fw.phi=(D16+D50+D84)/3) %>% 
      mutate(sd.fw.phi=(D84-D16)/4+(D95-D5)/6.6) %>% 
      mutate(skewness.fw.phi=(D16+D84-2*D50)/(2*(D84-D16))+(D5+D95-2*D50)/(2*(D95-D5))) %>% 
      mutate(kurtosis.fw.phi=(D95-D5)/(2.44*(D75-D25))) %>% select(-c(starts_with("D")))
    
    all.um <-all.um %>% group_by(samples) %>% mutate(mean.fw.um=exp((log2(D16)+log2(D50)+log2(D84))/3)*1000) %>% 
      mutate(sd.fw.um=exp((log2(D84)-log2(D16))/4+(log2(D95)-log2(D5))/6.6)*1000) %>% 
      mutate(skewness.fw.um=(log2(D16)+log2(D84)-2*log2(D50))/(2*(log2(D84)-log2(D16)))+(log2(D5)+log2(D95)-2*log2(D50))/(2*(log2(D95)-log2(D5)))) %>% 
      mutate(kurtosis.fw.um=(log2(D95)-log2(D5))/(2.44*(log2(D75)-log2(D25))))%>% select(-c(starts_with("D")))
    
    all <- full_join(all.um,all.phi,by="samples")
    
    
    all <- all %>% mutate( mean.descript=if_else(mean.fw.phi<=-5,"Very Coarse Gravel",
                                    if_else(mean.fw.phi>-5 & mean.fw.phi<=-4, "Coarse Gravel",
                                    if_else(mean.fw.phi>-4 & mean.fw.phi<=-3, "Medium Gravel",
                                    if_else(mean.fw.phi>-3 & mean.fw.phi<=-2, "Fine Gravel",
                                    if_else(mean.fw.phi>-2 & mean.fw.phi<=-1, "Very Fine Gravel",
                                    if_else(mean.fw.phi>-1 & mean.fw.phi<=0, "Very Coarse Sand",
                                    if_else(mean.fw.phi>0 & mean.fw.phi<=1, "Coarse Sand",
                                    if_else(mean.fw.phi>1 & mean.fw.phi<=2, "Medium Sand",
                                    if_else(mean.fw.phi>2 & mean.fw.phi<=3, "Fine Sand",
                                    if_else(mean.fw.phi>3 & mean.fw.phi<=4, "Very Fine Sand",
                                    if_else(mean.fw.phi>4 & mean.fw.phi<=5, "Very Coarse Silt",
                                    if_else(mean.fw.phi>5 & mean.fw.phi<=6, "Coarse Silt",
                                    if_else(mean.fw.phi>6 & mean.fw.phi<=7, "Medium Silt",
                                    if_else(mean.fw.phi>7 & mean.fw.phi<=8, "Fine Silt",
                                    if_else(mean.fw.phi>8 & mean.fw.phi<=9, "Very Fine Silt",
                                    if_else(mean.fw.phi>8, "Clay", "NA"))))))))))))))))) %>% 
      mutate(sorting=if_else (sd.fw.phi<0.35, "Very Well Sorted",
             if_else (sd.fw.phi>=0.35 & sd.fw.phi<0.5, "Well Sorted",
             if_else (sd.fw.phi>=0.5 & sd.fw.phi<0.7, "Moderately Well Sorted",
             if_else (sd.fw.phi>=0.7 & sd.fw.phi<1, "Moderately Sorted",
             if_else (sd.fw.phi>=1 & sd.fw.phi<2, "Poorly Sorted",
             if_else (sd.fw.phi>=2 & sd.fw.phi<4, "Very Poorly Sorted",
             if_else (sd.fw.phi>=4, "Extremely Poorly Sorted","NA")))))))) %>% 
      mutate(skewness=if_else (skewness.fw.phi>=0.3, "Very Fine Skewed",
             if_else (skewness.fw.phi<0.3 & skewness.fw.phi>=0.1, "Fine Skewed",
             if_else (skewness.fw.phi<0.1 & skewness.fw.phi>-0.1, "Symmetrical",
             if_else (skewness.fw.phi<=-0.1 & skewness.fw.phi>-0.3, "Coarse Skewed",
             if_else (skewness.fw.phi<=-0.3, "Very Coarse Skewed","NA")))))) %>%
      mutate(kurtosis=if_else (kurtosis.fw.phi<0.67, "Very Platykurtic",
             if_else (kurtosis.fw.phi>=0.67 & kurtosis.fw.phi<0.9, "Platykurtic",
             if_else (kurtosis.fw.phi>=0.9 & kurtosis.fw.phi<=1.11, "Mesokurtic",
             if_else (kurtosis.fw.phi>1.11 & kurtosis.fw.phi<=1.5, "Leptokurtic",
             if_else (kurtosis.fw.phi>1.5 & kurtosis.fw.phi<=3, "Very Leptokurtic",
             if_else (kurtosis.fw.phi>3, "Extremely Leptokurtic",'NA')))))))
      
      folk.ward <- all %>% unite(mean.descript,sorting,skewness,kurtosis,col="sediment",sep="/")

    return(folk.ward)
  }

.G2Sd_web <- function(){
  runApp(appDir=paste0(.libPaths()[1],"/G2Sd/extdata"))
}

.grancompat <- function(x)
{
  x <- as.data.frame(x)
  
  n.sieve <- nrow(x)
  n.sample <- ncol(x)
  
  if (any(as.matrix(x) < 0))
    stop("negative entries in dataframe.",call. = FALSE)
  if (any(as.matrix(x) > 300))
    warning("Some high values are present.", call. = FALSE,immediate.=TRUE)
  if (any(apply(x,2,sum)==0))
    stop("Some column sums equal to zero",call. = FALSE)
  else {return(x)}
}

.index.sedim <-  function(x,decreasing){
    x=as.data.frame(x)
    mat.D <- .percentile(x,decreasing)
    mat.D$percentile <- paste("D",mat.D$percentile,sep="")
    INDEX <- mat.D %>% select(-phi) %>% spread(percentile,meshsize)
    INDEX <- INDEX %>% mutate("D90/D10"=D90/D10,"D90-D10"=D90-D10,"D75/D25"=D75/D25,"D75-D25"=D75-D25,"Trask(So)"=sqrt(D75/D25),"Krumbein(Qd)"=(D25-D75)/2) 
    return(INDEX)
  }

.mode.sedim <-
function(x){
    x=.g2sd_tidy(x)
    
    all <- x %>% group_by(samples) %>% mutate(relative.value=value*100/sum(value))
    all[all$meshsize==0,"meshsize"] <- 10
    
    ggplot(all,aes(x=meshsize,y=relative.value))+geom_line()+facet_wrap(~samples,scales="free_y")+
      scale_x_log10()+
      xlab(expression(paste("Particule size ",log[10],"(",mu,"m)")))+ylab("Pourcentage (%)")  
}

.moment.arith <-  function(x){
    
    x <- .g2sd_tidy(x)

    all <- x %>% group_by(samples) %>% mutate(relative.value=value*100/sum(value))
    
    all <- all %>% group_by(samples) %>% arrange(meshsize) %>% mutate(meshsize.midpoint=abs((lag(meshsize)-meshsize)/2)/1000)
    all[is.na(all$meshsize.midpoint),"meshsize.midpoint"] <- all[is.na(all$meshsize.midpoint),"meshsize"]
    
    arith <- all %>% group_by(samples) %>% mutate(mean.arith.um=sum(relative.value*meshsize.midpoint)/100) %>% 
      mutate(sd.arith.um=sqrt(sum(relative.value*(meshsize.midpoint-mean.arith.um)^2)/100)) %>% 
      mutate(skewness.arith.um=sum(relative.value*(meshsize.midpoint-mean.arith.um)^3)/(100*sd.arith.um^3)) %>% 
      mutate(kurtosis.arith.um=sum(relative.value*(meshsize.midpoint-mean.arith.um)^4)/(100*sd.arith.um^4)) %>% 
      mutate(mean.arith.um=mean.arith.um*1000,sd.arith.um=sd.arith.um*1000) %>% select(-c("meshsize","value","relative.value","meshsize.midpoint")) %>% 
      distinct()
  
    return(arith)
  }
.moment.geom <- function(x){
    
    
    x <- .g2sd_tidy(x)
    
    all <- x %>% group_by(samples) %>% mutate(relative.value=value*100/sum(value))
    all[all$meshsize==0,"meshsize"] <- 1
    all <- all %>% group_by(samples) %>% arrange(meshsize) %>% mutate(meshsize.midpoint=abs((lag(meshsize)-meshsize)/2)/1000)
    all[is.na(all$meshsize.midpoint),"meshsize.midpoint"] <- all[is.na(all$meshsize.midpoint),"meshsize"]
    
    geom <- all %>% group_by(samples) %>% mutate(mean.geom.um=exp(sum(relative.value*log2(meshsize.midpoint))/100)) %>% 
      mutate(sd.geom.um=exp(sqrt(sum(relative.value*(log2(meshsize.midpoint)-log2(mean.geom.um))^2)/100))) %>% 
      mutate(skewness.geom.um=sum(relative.value*(log2(meshsize.midpoint)-log2(mean.geom.um))^3)/(100*log2(sd.geom.um)^3)) %>% 
      mutate(kurtosis.geom.um=sum(relative.value*(log2(meshsize.midpoint)-log2(mean.geom.um))^4)/(100*log2(sd.geom.um)^4)) %>% 
      mutate(mean.geom.um=mean.geom.um*1000,sd.geom.um=sd.geom.um*1000) %>% select(-c("meshsize","value","relative.value","meshsize.midpoint")) %>% 
      distinct()
    
    return(geom)
  }

.percentile <- function(x,decreasing){
    
    x <- .g2sd_tidy(x)
    x[x$meshsize==0,"meshsize"] <- 1
    
    x <- x %>% mutate(phi=.um2phi(meshsize))
    
    if(decreasing)
    all <- x %>% group_by(samples) %>% mutate(value.relative=value/sum(value)) %>% arrange(desc(meshsize)) %>% mutate(cum.sum=cumsum(value.relative))
    if(!decreasing)
    all <- x %>% group_by(samples) %>% mutate(value.relative=value/sum(value)) %>% arrange(meshsize) %>% mutate(cum.sum=cumsum(value.relative))

    D=c(0.05,0.1,0.16,0.25,0.5,0.75,0.84,0.9,0.95)
    
    mat.D <- NULL
    for(i in 1:length(D))
    {
    Dmin <- all %>% group_by(samples) %>% filter(cum.sum<=D[i]) %>% filter(cum.sum==max(cum.sum)) %>% filter(phi==max(phi))%>% filter(cum.sum==max(cum.sum)) %>%
      select(-c("value","value.relative")) %>% rename(min=cum.sum,meshsize.min=meshsize,phi.min=phi)
    Dmax <- all %>% group_by(samples) %>% filter(cum.sum>=D[i]) %>% filter(cum.sum==min(cum.sum)) %>% filter(phi==min(phi))%>% filter(cum.sum==min(cum.sum)) %>%
      select(-c("value","value.relative")) %>% rename(max=cum.sum,meshsize.max=meshsize,phi.max=phi)
    Dall <- full_join(Dmin,Dmax,by="samples")
    Dall <- Dall %>% mutate(ratio=(D[i]-min)/(max-min)) %>% mutate(phi=((phi.max-phi.min)*ratio)+phi.min)
    Dall[(na.omit(Dall$phi.min)==na.omit(Dall$phi.max)),"phi"] <- Dall[(na.omit(Dall$phi.min)==na.omit(Dall$phi.max)),"phi.min"]
    Dall <- Dall %>% mutate(meshsize=.phi2um(phi)) %>% select(samples,phi,meshsize) %>% mutate(percentile=D[i]*100)
    mat.D <- bind_rows(mat.D,Dall)
    }
    mat.D$percentile <- 100-mat.D$percentile
    return(mat.D)
  }

.sedim.descript <- function(x){
    
  x <- .g2sd_tidy(x)  
  
  sedim <- c(63000,31500,2^c(4:-3)*1000,63,40)

  all <- x %>% group_by(samples) %>% mutate(relative.value=value*100/sum(value)) %>% 
    mutate(class=if_else(meshsize>=sedim[1],"boulder",
                       if_else(meshsize<sedim[1] & meshsize>=sedim[2],"vcgravel",
                       if_else(meshsize<sedim[2] & meshsize>=sedim[3],"cgravel",
                       if_else(meshsize<sedim[3] & meshsize>=sedim[4],"mgravel",
                       if_else(meshsize<sedim[4] & meshsize>=sedim[5],"fgravel",
                       if_else(meshsize<sedim[5] & meshsize>=sedim[6],"vfgravel",
                       if_else(meshsize<sedim[6] & meshsize>=sedim[7],"vcsand",
                       if_else(meshsize<sedim[7] & meshsize>=sedim[8],"csand",
                       if_else(meshsize<sedim[8] & meshsize>=sedim[9],"msand",
                       if_else(meshsize<sedim[9] & meshsize>=sedim[10],"fsand",
                       if_else(meshsize<sedim[10] & meshsize>=sedim[11],"vfsand",
                       if_else(meshsize<sedim[11] & meshsize>=sedim[12],"vcsilt",
                       if_else(meshsize<sedim[12] ,"silt","NA"))))))))))))))
                                                                                                               
sediment <- all %>% group_by(samples,class) %>% summarise(value=sum(relative.value)) %>% 
  spread(class,value) %>% select(samples,everything(),any_of(c("cgravel","mgravel","fgravel","vfgravel","vcsand",
                                 "csand","msand","fsand","vfsand","vcsilt","silt")))
    return(sediment)
  }
.texture.sedim <- function(x){
    
  x <- .g2sd_tidy(x)  
  
  texture <- c(63000,2000,63)
  all <- x %>% group_by(samples) %>% mutate(relative.value=value*100/sum(value)) %>% 
        mutate(class=if_else(meshsize>=texture[1],"Boulder",
                             if_else(meshsize<texture[1] & meshsize>=texture[2],"Gravel", 
                             if_else(meshsize<texture[2] & meshsize>=texture[3],"Sand", 
                             if_else(meshsize<texture[3],"Mud", "NA")))))
  Texture <- all %>% group_by(samples,class) %>% summarise(value=sum(relative.value))   
  
  Texture <- Texture %>% spread(class,value) %>% 
    mutate(sandmud=if_else(Sand==0 & Mud==0,0,
                   if_else(Sand>0 & Mud==0,10,
                   if_else(Sand==0 & Mud>0,0.01,
                   if_else(Sand>0 & Mud>0,Sand/Mud,0))))) 
  
  if(!"sandmud" %in% colnames(Texture)){
    Texture <- Texture %>% mutate(sandmud = 0)
  }
  
  if(!"Gravel" %in% colnames(Texture)){
    Texture <- Texture %>% mutate(Gravel = 0)
  }
  
  Texture <- Texture %>%
    mutate(texture=if_else(sandmud>=9 & Gravel>80, "Gravel",
                   if_else(sandmud>=9 & (Gravel>30 & Gravel<=80), "Sandy Gravel",
                   if_else(sandmud>=9 & (Gravel>5 & Gravel<=30), "Gravelly Sand",
                   if_else(sandmud>=9 & (Gravel>0 & Gravel<=5), "Slightly Gravelly Sand",
                   if_else(sandmud>=9 & Gravel==0, "Sand",
                   if_else((sandmud>=1 & sandmud<9) & Gravel>80, "Gravel",
                   if_else((sandmud>=1 & sandmud<9) & (Gravel>30 & Gravel<=80), "Muddy Sandy Gravel",
                   if_else((sandmud>=1 & sandmud<9) & (Gravel>5 & Gravel<=30), "Gravelly Muddy Sand",
                   if_else((sandmud>=1 & sandmud<9) & (Gravel>0 & Gravel<=5), "Slightly Gravelly Muddy Sand",
                   if_else((sandmud>=1 & sandmud<9) & Gravel==0, "Muddy Sand",
                   if_else((sandmud>=1/9 & sandmud<1) & Gravel>80, "Gravel",
                   if_else((sandmud>=1/9 & sandmud<1) & (Gravel>30 & Gravel<=80), "Muddy Gravel",
                   if_else((sandmud>=1/9 & sandmud<1) & (Gravel>5 & Gravel<=30), "Gravelly Mud",
                   if_else((sandmud>=1/9 & sandmud<1) & (Gravel>0 & Gravel<=5), "Slightly Gravelly Sandy Mud",
                   if_else((sandmud>=1/9 & sandmud<1) & Gravel==0, "Sandy Mud",
                   if_else(sandmud<1/9 & Gravel>80, "Gravel",
                   if_else(sandmud<1/9 & (Gravel>30 & Gravel<=80), "Muddy Gravel",
                   if_else(sandmud<1/9 & (Gravel>5 & Gravel<=30), "Gravelly Mud",
                   if_else(sandmud<1/9 & (Gravel>0 & Gravel<=5), "Slightly Gravelly Mud",
                   if_else(sandmud<1/9 & Gravel==0, "Mud","NA"))))))))))))))))))))) %>% 
    select(-sandmud) %>% select(samples,everything(),Sand,Mud,texture) 

    return(Texture)
}

.reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

.g2sd_tidy <- function(x)
{
  x <- x %>% rownames_to_column("meshsize") %>% gather(-meshsize,key="samples",value="value")
  x$meshsize <- as.numeric(x$meshsize)
  return(x)
}
