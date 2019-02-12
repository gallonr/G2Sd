.fowa.stat <-
  function(x,phi,um,decreasing){
    
    folk.ward=data.frame(matrix(ncol=0,nrow=9))
    
    
    for (b in 1:dim(x)[2])
    {y=x[b]
    sum.sieve=sum(y)
    class.weight=(y*100)/sum.sieve
    cum.sum=cumsum(class.weight)[,1]
    
    if (min(cum.sum)>5) 
    {
      fowa=data.frame(rep(0,9))
      row.names(fowa)=c("Sediment","Mean.fw.um","Sd.fw.um","Skewness.fw.um","Kurtosis.fw.um","Mean.fw.phi","Sd.fw.phi","Skewness.fw.phi","Kurtosis.fw.phi")
      names(fowa)=names(x)[b]
    }
    
    if (min(cum.sum)<5)
    {
      mat.D=.percentile(x[,b],phi,um,decreasing)
      mat.D[is.na(mat.D)] <- 0
      
      mean.phi=(mat.D[6,1]+mat.D[4,1]+mat.D[2,1])/3
      mean.mm=((mat.D[6,2]+mat.D[4,2]+mat.D[2,2])/3)/1000
      
      sd.phi=-(((mat.D[2,1]-mat.D[6,1])/4)+((mat.D[1,1]-mat.D[7,1])/6.6))
      sd.mm=((mat.D[2,2]-mat.D[6,2])/4+(mat.D[1,2]-mat.D[7,2])/6.6)/1000
      
      skewness.phi=-(((mat.D[6,1]+mat.D[2,1]-(2*mat.D[4,1]))/(2*(mat.D[2,1]-mat.D[6,1])))+ ((mat.D[7,1]+mat.D[1,1]-(2*mat.D[4,1]))/(2*(mat.D[1,1]-mat.D[7,1]))))
      skewness.mm=-skewness.phi
      
      kurtosis.phi=(mat.D[1,1]-mat.D[7,1])/(2.44*(mat.D[3,1]-mat.D[5,1]))
      kurtosis.mm=kurtosis.phi
      
      if (mean.phi<=-5) mean.descript="Very Coarse Gravel"
      if (mean.phi>-5 & mean.phi<=-4) mean.descript="Coarse Gravel"
      if (mean.phi>-4 & mean.phi<=-3) mean.descript="Medium Gravel"
      if (mean.phi>-3 & mean.phi<=-2) mean.descript="Fine Gravel"
      if (mean.phi>-2 & mean.phi<=-1) mean.descript="Very Fine Gravel"
      if (mean.phi>-1 & mean.phi<=0) mean.descript="Very Coarse Sand"
      if (mean.phi>0 & mean.phi<=1) mean.descript="Coarse Sand"
      if (mean.phi>1 & mean.phi<=2) mean.descript="Medium Sand"
      if (mean.phi>2 & mean.phi<=3) mean.descript="Fine Sand"
      if (mean.phi>3 & mean.phi<=4) mean.descript="Very Fine Sand"
      if (mean.phi>4 & mean.phi<=5) mean.descript="Very Coarse Silt"
      if (mean.phi>5 & mean.phi<=6) mean.descript="Coarse Silt"
      if (mean.phi>6 & mean.phi<=7) mean.descript="Medium Silt"
      if (mean.phi>7 & mean.phi<=8) mean.descript="Fine Silt"
      if (mean.phi>8 & mean.phi<=9) mean.descript="Very Fine Silt"
      if (mean.phi>8) mean.descript="Clay"
      
      if (sd.phi<0.35) sorting="Very Well Sorted"
      if (sd.phi>=0.35 & sd.phi<0.5) sorting="Well Sorted"
      if (sd.phi>=0.5 & sd.phi<0.7) sorting="Moderately Well Sorted"
      if (sd.phi>=0.7 & sd.phi<1) sorting="Moderately Sorted"
      if (sd.phi>=1 & sd.phi<2) sorting="Poorly Sorted"
      if (sd.phi>=2 & sd.phi<4) sorting="Very Poorly Sorted"
      if (sd.phi>=4) sorting="Extremely Poorly Sorted"
      
      
      if (skewness.phi>=0.3) skewness.descript="Very Fine Skewed"
      if (skewness.phi<0.3 & skewness.phi>=0.1) skewness.descript="Fine Skewed"
      if (skewness.phi<0.1 & skewness.phi>-0.1) skewness.descript="Symmetrical"
      if (skewness.phi<=-0.1 & skewness.phi>-0.3) skewness.descript="Coarse Skewed"
      if (skewness.phi<=-0.3) skewness.descript="Very Coarse Skewed"
      
      if (kurtosis.phi<0.67) kurtosis.descript="Very Platykurtic"
      if (kurtosis.phi>=0.67 & kurtosis.phi<0.9) kurtosis.descript="Platykurtic"
      if (kurtosis.phi>=0.9 & kurtosis.phi<=1.11) kurtosis.descript="Mesokurtic"
      if (kurtosis.phi>1.11 & kurtosis.phi<=1.5) kurtosis.descript="Leptokurtic"
      if (kurtosis.phi>1.5 & kurtosis.phi<=3) kurtosis.descript="Very Leptokurtic"
      if (kurtosis.phi>3) kurtosis.descript="Extremely Leptokurtic"
      
      .sedim.descript=paste(mean.descript,sorting,skewness.descript,kurtosis.descript,sep=",")
      
      result.fw.phi=data.frame(c(round(mean.phi,3),round(sd.phi,3),round(skewness.phi,3),round(kurtosis.phi,3)))
      names(result.fw.phi)=names(x)[b]
      
      result.fw.mm=data.frame(c(round(mean.mm,3),round(sd.mm,3),round(skewness.mm,3),round(kurtosis.mm,3)))
      names(result.fw.mm)=names(x)[b]
      
      fowa=data.frame(rbind(.sedim.descript,result.fw.mm,result.fw.phi))
      row.names(fowa)=c("Sediment","Mean.fw.um","Sd.fw.um","Skewness.fw.um","Kurtosis.fw.um","Mean.fw.phi","Sd.fw.phi","Skewness.fw.phi","Kurtosis.fw.phi")
      names(fowa)=names(x)[b]
    }
    folk.ward=cbind(folk.ward,fowa)
    }
    folk.ward
  }

.G2Sd_web <-
function(){
  runApp(appDir=paste0(.libPaths()[1],"/G2Sd/extdata"))
}

.grancompat <-
function(x)
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

.index.sedim <-
  function(x,phi,um,decreasing){
    x=as.data.frame(x)
    INDEX=data.frame(matrix(ncol=0,nrow=9))
    for (b in 1:dim(x)[2])
    {
      mat.D=.percentile(x[,b],phi,um,decreasing)
      index=data.frame(matrix(ncol=1,nrow=9))
      row.names(index)=c("D10(um)","D50(um)","D90(um)","D90/D10","D90-D10","D75/D25","D75-D25","Trask(So)","Krumbein(Qd)")
      names(index)=names(x)[b]
      index[1,1]=round(mat.D["10",2],3)
      index[2,1]=round(mat.D["50",2],3)
      index[3,1]=round(mat.D["90",2],3)
      index[4,1]=round(mat.D["90",2]/mat.D["10",2],3)
      index[5,1]=round(mat.D["90",2]-mat.D["10",2],3)
      index[6,1]=round(mat.D["75",2]/mat.D["25",2],3)
      index[7,1]=round(mat.D["75",2]-mat.D["25",2],3)
      index[8,1]=round(sqrt(mat.D["75",2]/mat.D["25",2]),3)
      index[9,1]=round((mat.D["25",1]-mat.D["75",1])/2,3)
      
      
      INDEX=cbind(INDEX,index)
    }
    return(INDEX)
  }
.mgrep <-
function(mpattern,x,FUN="grep")
{
  select=NULL
  for (i in 1:length(mpattern))
  {
    if (FUN=="grep")
      values=grep(mpattern[i],x)
    if (FUN=="which")
      values=which(x==mpattern[i])  
    select=c(select,values)
  }
  return(sort(select))
}
.mode.sedim <-
function(x,um){
    
    x=as.data.frame(x)
    sum.sieve=apply(x,2,sum)
    
    MODE=data.frame(matrix(ncol=0,nrow=5))
    for (b in 1:dim(x)[2])
    {
      
      
      class.weight=(x[,b]*100)/sum.sieve[b]            
      tab.mod=cbind(um,class.weight)
      if (pmatch(0,um)!=0) tab.mod=tab.mod[-pmatch(0,um),]
      
      plot(tab.mod[,1],tab.mod[,2],type="b",lwd=3,xlab="Particule size (microns)",ylab="Pourcentage (%)",xaxt="n",log="x")
      a=identify(tab.mod,plot=FALSE,n=4)
      
      mod=data.frame(tab.mod[a,1])
      names(mod)=names(x)[b]
      row.names(mod)=tab.mod[a,1]
      
      if (dim(mod)[1]==1) mod.descript="1 Mode" else mod.descript=paste(dim(mod)[1],"Modes")
      
      
      MODE.sedim=data.frame(matrix(ncol=1,nrow=4))
      
      for (i in 1:dim(mod)[1])
        MODE.sedim[i,]=mod[i,]
      MODE.sedim=rbind(mod.descript,MODE.sedim)
      names(MODE.sedim)=names(x)[b]
      row.names(MODE.sedim)[1]="Nb Mode"
      MODE.sedim
      MODE=cbind(MODE,MODE.sedim)
    }
    return(MODE)
  }
.moment.arith <-
  function(x,um){
    
    
    x=as.data.frame(x)
    sum.sieve=apply(x,2,sum)
    
    arith=data.frame(matrix(ncol=0,nrow=4))
    for (b in 1:dim(x)[2])
    {
      class.weight=(x[b]*100)/sum.sieve[b]
      
      
      
      mid.point=rep(0,(length(um)))
      
      for(i in 2:length(um))
      {
        
        mid.point[i]=(um[i]+um[i-1])/2
        
      }
      
      fm=class.weight*mid.point
      mean.arith=apply(fm,2,sum)/100
      
      fmM2=class.weight*(mid.point-mean.arith)^2
      sd.arith=sqrt(apply(fmM2,2,sum)/100)
      
      fmM3=class.weight*(mid.point-mean.arith)^3
      skewness.arith=apply(fmM3,2,sum)/(100*sd.arith^3)
      
      fmM4=class.weight*(mid.point-mean.arith)^4
      kurtosis.arith=apply(fmM4,2,sum)/(100*sd.arith^4)
      
      
      moment.arit=data.frame(rbind(round(mean.arith,3),round(sd.arith,3),round(skewness.arith,3),round(kurtosis.arith,3)))
      colnames(moment.arit)=colnames(x)[b]
      arith=cbind(arith,moment.arit)
      rownames(arith)=c("mean.arith.um","sd.arith.um","skewness.arith.um","kurtosis.arith.um")
    }
    return(arith)
  }
.moment.geom <-
  function(x,phi){
    
    
    x=as.data.frame(x)
    sum.sieve=apply(x,2,sum)
    
    geom=data.frame(matrix(ncol=0,nrow=4))
    for (b in 1:dim(x)[2])
    {
      class.weight=(x[b]*100)/sum.sieve[b]
      
      mid.point=rep(0,(length(phi)))
      
      for(i in 2:length(phi))
      {
        
        mid.point[i]=(phi[i]+phi[i-1])/2
        
      }
      
      
      logm=log10(2^(-mid.point)*1000)
      flogm=class.weight*logm
      mean.geom=10^(apply(flogm,2,sum)/100)
      
      fmM2=class.weight*(logm-log10(mean.geom))^2
      sd.geom=10^(sqrt(apply(fmM2,2,sum)/100))
      
      fmM3=class.weight*(logm-log10(mean.geom))^3
      skewness.geom=(apply(fmM3,2,sum)/(100*log10(sd.geom)^3))
      
      fmM4=class.weight*(logm-log10(mean.geom))^4
      kurtosis.geom=(apply(fmM4,2,sum)/(100*log10(sd.geom)^4))
      kurtosis3.geom=kurtosis.geom
      
      moment.geo=as.data.frame(rbind(round(mean.geom,3),round(sd.geom,3),round(skewness.geom,3),round(kurtosis.geom,3)))
      names(moment.geo)=names(x)[b]
      geom=cbind(geom,moment.geo)
      rownames(geom)=c("mean.geom.um","sd.geom.um","skewness.geom.um","kurtosis.geom.um")
    }
    return(geom)
  }
# .northarrow <-
# function(loc,size,bearing=0,cols,letter_dist=1,cex=1,...) {
#     # checking arguments
#     if(missing(loc)) stop("loc is missing")
#     if(missing(size)) stop("size is missing")
#     # default colors are white and black
#     if(missing(cols)) cols <- rep(c("white","black"),8)
#     # calculating coordinates of polygons
#     radii <- rep(size/c(1,4,2,4),4)
#     x <- radii[(0:15)+1]*cos((0:15)*pi/8+bearing)+loc[1]
#     y <- radii[(0:15)+1]*sin((0:15)*pi/8+bearing)+loc[2]
#     # drawing polygons
#     for (i in 1:15) {
#       x1 <- c(x[i],x[i+1],loc[1])
#       y1 <- c(y[i],y[i+1],loc[2])
#       polygon(x1,y1,col=cols[i])
#     }
#     # drawing the last polygon
#     polygon(c(x[16],x[1],loc[1]),c(y[16],y[1],loc[2]),col=cols[16])
#     # drawing letters
#     b <- c("E","N","W","S")
#     for (i in 0:3) text((size+letter_dist*par("cxy")[1])*cos(bearing+i*pi/2)+loc[1],
#                         (size+letter_dist*par("cxy")[2])*sin(bearing+i*pi/2)+loc[2],b[i+1],
#                         cex=cex)
#   }
.percentile <-
  function(x,phi,um,decreasing){
    
    all <- data.frame(x,phi,um)
    all <- all[order(as.numeric(all$um),decreasing=decreasing),]
    all <- all[all[,1]!=0,]
    x <- as.numeric(all[,1])
    
    sum.sieve=sum(x)
    class.weight=(x*100)/sum.sieve
    cum.sum=as.numeric(cumsum(class.weight))
    D=c(10,16,25,50,75,84,90,95)
    
    minimum.cumsum <- min(cum.sum)
    if (any(D< minimum.cumsum))
    {
      warning(paste0(paste0("D",D[D< minimum.cumsum],collapse=", ")," can't be calculated"), call. = FALSE,immediate.=TRUE)
      class.weight.PHI=cbind(cum.sum,all$phi,all$um)
      mat.D=data.frame(matrix(ncol=2,nrow=length(D)))
      row.names(mat.D)=D
      names(mat.D)=c("Phi","um")
      nbclass <- which(D> minimum.cumsum,arr.ind=TRUE)
    }
    
    if (all(D> minimum.cumsum))
    {
      class.weight.PHI=cbind(cum.sum,all$phi,all$um)
      mat.D=data.frame(matrix(ncol=2,nrow=length(D)))
      row.names(mat.D)=D
      names(mat.D)=c("Phi","um")
      nbclass <- 1:length(D)
    }
    
    
    for (i in nbclass)
    {

      greaterpercent <-  class.weight.PHI[class.weight.PHI[,1]>D[i],]
      if (is.null(dim(greaterpercent))) 
      { 
        greaterpercent <- as.data.frame(t(greaterpercent))
        names(greaterpercent) <- c("cum.sum","phi","um")
      }
      
      greaterphi=greaterpercent[greaterpercent[,1]==min(greaterpercent[,1]),-1]
      greaterphi=as.numeric(greaterphi[greaterphi[2]==min(greaterphi[2])][1])
      greaterpercent=min(greaterpercent[,1])
      lesspercent=class.weight.PHI[class.weight.PHI[,1]<D[i],]
      
      if (is.null(dim(lesspercent))) 
      { 
        lesspercent <- as.data.frame(t(lesspercent))
        names(lesspercent) <- c("cum.sum","phi","um")
      }
      lessphi=lesspercent[lesspercent[,1]==max(lesspercent[,1]),-1]
      lessphi=as.numeric(lesspercent[lessphi[2]==min(lessphi[2])][1])
      lesspercent=max(lesspercent[,1])
      
      ifelse  (dim(class.weight.PHI[class.weight.PHI[,1]==D[i],])[1]==0,
               {ratio1=(D[i]-lesspercent)/(greaterpercent-lesspercent)
               ratio2=(greaterphi-lessphi)*ratio1
               phi=lessphi+ratio2
               um=1/(2^phi*100)},
               {phi=as.numeric(subset(class.weight.PHI,class.weight.PHI[,1]==D[i],2))
               um=as.numeric(subset(class.weight.PHI,class.weight.PHI[,1]==D[i],3))})
      
      
      result=c(phi,um)
      mat.D[i,]=result
    }
    return(mat.D)
    
  }

.sedim.descript <-
function(x,um){
    
    
    um=as.numeric(um)
    
    x=as.data.frame(x)
    sum.sieve=apply(x,2,sum)
    sediment=data.frame(matrix(ncol=0,nrow=13))
    for (b in 1:dim(x)[2])
    {
      
      class.weight=(x[,b])*100/sum.sieve[b]
      class.weight.um=cbind(class.weight,um)
      
      
      seuil.sedim=c(63000,31500,2^c(4:-3)*1000,63,40,NA)
      class.sedim=c("boulder","vcgravel","cgravel","mgravel","fgravel","vfgravel","vcsand","csand","msand","fsand","vfsand","vcsilt","silt")
      sedim=data.frame(cbind(seuil.sedim,class.sedim),stringsAsFactors = FALSE)
      sedim[,1]=as.numeric(sedim[,1])
      
      
      result=data.frame(matrix(nrow=dim(sedim)[1],ncol=dim(sedim)[1]))
      result[,1]=sedim[,1]
      names(result)=c("Sedim","Pourcentage")
      sedim.result=0
      
      sedim.percent=subset(class.weight.um,class.weight.um[,2]>=sedim[1,1])
      sedim.result=sum(as.numeric(sedim.percent[,1]))
      result[1,1]=sedim.result
      
      sedim.percent=subset(class.weight.um,class.weight.um[,2]<sedim[1,1] & class.weight.um[,2]>=(sedim[2,1]))
      sedim.result=sum(as.numeric(sedim.percent[,1]))
      result[2,1]=sedim.result
      
      sedim.percent=subset(class.weight.um,class.weight.um[,2]<sedim[2,1] & class.weight.um[,2]>=(sedim[3,1]))
      sedim.result=sum(as.numeric(sedim.percent[,1]))
      result[3,1]=sedim.result
      
      sedim.percent=subset(class.weight.um,class.weight.um[,2]<sedim[3,1] & class.weight.um[,2]>=(sedim[4,1]))
      sedim.result=sum(as.numeric(sedim.percent[,1]))
      result[4,1]=sedim.result
      
      sedim.percent=subset(class.weight.um,class.weight.um[,2]<sedim[4,1] & class.weight.um[,2]>=(sedim[5,1]))
      sedim.result=sum(as.numeric(sedim.percent[,1]))
      result[5,1]=sedim.result
      
      sedim.percent=subset(class.weight.um,class.weight.um[,2]<sedim[5,1] & class.weight.um[,2]>=(sedim[6,1]))
      sedim.result=sum(as.numeric(sedim.percent[,1]))
      result[6,1]=sedim.result
      
      sedim.percent=subset(class.weight.um,class.weight.um[,2]<sedim[6,1] & class.weight.um[,2]>=(sedim[7,1]))
      sedim.result=sum(as.numeric(sedim.percent[,1]))
      result[7,1]=sedim.result
      
      sedim.percent=subset(class.weight.um,class.weight.um[,2]<sedim[7,1] & class.weight.um[,2]>=(sedim[8,1]))
      sedim.result=sum(as.numeric(sedim.percent[,1]))
      result[8,1]=sedim.result
      
      sedim.percent=subset(class.weight.um,class.weight.um[,2]<sedim[8,1] & class.weight.um[,2]>=(sedim[9,1]))
      sedim.result=sum(as.numeric(sedim.percent[,1]))
      result[9,1]=sedim.result
      
      sedim.percent=subset(class.weight.um,class.weight.um[,2]<sedim[9,1] & class.weight.um[,2]>=(sedim[10,1]))
      sedim.result=sum(as.numeric(sedim.percent[,1]))
      result[10,1]=sedim.result
      
      sedim.percent=subset(class.weight.um,class.weight.um[,2]<sedim[10,1] & class.weight.um[,2]>=(sedim[11,1]))
      sedim.result=sum(as.numeric(sedim.percent[,1]))
      result[11,1]=sedim.result
      
      sedim.percent=subset(class.weight.um,class.weight.um[,2]<sedim[11,1] & class.weight.um[,2]>=(sedim[12,1]))
      sedim.result=sum(as.numeric(sedim.percent[,1]))
      result[12,1]=sedim.result
      
      sedim.percent=subset(class.weight.um,class.weight.um[,2]<sedim[12,1])
      sedim.result=sum(as.numeric(sedim.percent[,1]))
      result[13,1]=sedim.result
      
      result=data.frame(result[,1])
      names(result)=names(x)[b]
      row.names(result)=class.sedim
      
      sediment=cbind(sediment,round(result,3))
    }
    return(sediment)
  }
.texture.sedim <-
function(x,um){
    
    um=as.numeric(um)
    x=as.data.frame(x)
    sum.sieve=apply(x,2,sum)
    
    Texture=data.frame(matrix(ncol=0,nrow=5))
    for (b in 1:dim(x)[2])
    {
      
      class.weight=(x[,b]*100)/sum.sieve[b]           
      class.weight.um=cbind(class.weight,um)
      class.weight.um[,1] <- as.numeric(as.character(class.weight.um[,1]))
      class.weight.um[,2] <- as.numeric(as.character(class.weight.um[,2]))
      
      seuil.texture=c(63000,2000,63,0)
      class.texture=c("Boulder","Gravel","Sand","mud")
      texture=data.frame(cbind(seuil.texture,class.texture),stringsAsFactors = FALSE)
      texture[,1]=as.numeric(texture[,1])
      
      result=data.frame(matrix(nrow=dim(texture)[1],ncol=dim(texture)[2]))
      result[,1]=texture[,2]
      names(result)=c("Texture","Pourcentage")
      texture.result=0
      
      texture.percent=subset(class.weight.um,class.weight.um[,2]>=texture[1,1])
      texture.result=sum(texture.percent[,1])
      result[1,2]=texture.result
      
      texture.percent=subset(class.weight.um,class.weight.um[,2]<texture[1,1] & class.weight.um[,2]>=(texture[2,1]))
      texture.result=sum(texture.percent[,1])
      result[2,2]=texture.result 
      
      texture.percent=subset(class.weight.um,class.weight.um[,2]<texture[2,1] & class.weight.um[,2]>=(texture[3,1]))
      texture.result=sum(texture.percent[,1])
      result[3,2]=texture.result 
      
      texture.percent=subset(class.weight.um,class.weight.um[,2]<(texture[3,1]))
      texture.result=sum(texture.percent[,1])
      result[4,2]=texture.result 
      
      mud=round(result[4,2],3)
      gravel=round(result[2,2],3)
      sand=round(result[3,2],3)
      
      
{if (mud==0 & sand==0) mudsand=0
 if (mud==0 & sand>0) mudsand=10
 if (sand==0 & mud>0) mudsand=0.01
 if (sand>0 & mud>0) mudsand=sand/mud} 
      
      if (mudsand>=9){
        if (gravel>80) texture="Gravel"
        if (gravel>30 & gravel<=80) texture="Sandy Gravel"
        if (gravel>5 & gravel<=30) texture="Gravelly Sand"
        if (gravel>0 & gravel<=5) texture="Slightly Gravelly Sand"
        if (gravel==0) texture="Sand"}
      
      if (mudsand>=1 & mudsand<9){
        if (gravel>80) texture="Gravel"
        if (gravel>30 & gravel<=80) texture="Muddy Sandy Gravel"
        if (gravel>5 & gravel<=30) texture="Gravelly Muddy Sand"
        if (gravel>0 & gravel<=5) texture="Slightly Gravelly Muddy Sand"
        if (gravel==0) texture="Muddy Sand"}
      
      if (mudsand>=(1/9) & mudsand<1){
        if (gravel>80) texture="Gravel"
        if (gravel>30 & gravel<=80) texture="Muddy Gravel"
        if (gravel>5 & gravel<=30) texture=" Gravelly Mud"
        if (gravel>0 & gravel<=5) texture="Slightly Gravelly Sandy Mud"
        if (gravel==0) texture="Sandy Mud"} 
      
      if (mudsand<(1/9)){
        if (gravel>80) texture="Gravel"
        if (gravel>30 & gravel<=80) texture="Muddy Gravel"
        if (gravel>5 & gravel<=30) texture=" Gravelly Mud"
        if (gravel>0 & gravel<=5) texture="Slightly Gravelly Mud"
        if (gravel==0) texture="Mud"} 
      
      row.names(result)=result[,1]
      name.texture=row.names(result)
      result=data.frame(result[,2])
      
      texture.sedim=data.frame(rbind(texture,round(result,3)))
      row.names(texture.sedim)=c("Texture",name.texture)
      names(texture.sedim)=names(x)[b]  
      texture.sedim
      Texture=cbind(Texture,texture.sedim)
    }
    
    return(Texture)
}

.reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

# .grantern <- function(x,xc)
# {
#   sedim <- t(granstat(granulo,aggr=F)$sedim)
#   sedim <- sedim[,-1]
#   sedim <- as.data.frame(apply(sedim,2,function(x)as.numeric(as.character(x))))
#   sedim$mudsand <- sedim$Sand/sedim$mud
#   # sedim <- data.frame(Gravel=c(100,0,0),mud=c(0,100,0),Sand=c(0,0,100))
#   # 
#   # sedim$Grav <- sedim$Gravel / (sedim$Gravel + sedim$Sand + sedim$mud) * 100
#   # sedim$san <- sedim$Sand / (sedim$Sand + sedim$mud) * 100
#   # 
#   # sedim$san[is.na(sedim$san)] <- 0
#   
# 
# #   X1 <- Y1 <- numeric(length(sedim$Grav))
# # 
# #   if (any(sedim$Grav <= 0.5))
# #     {
# #     sel.line <- which(sedim$Grav <= 0.5)
# #   Y1[sel.line] <- (sedim$Grav[sel.line]* 19.919 )/ 0.5
# #   }
# #   if (any(sedim$Grav > 0.5 & sedim$Grav <= 5))
# #   {
# #     sel.line <- which(sedim$Grav > 0.5 & sedim$Grav <= 5)
# #     Y1[sel.line] <- (((sedim$Grav[sel.line] - 0.5) * 39.837 )/ 4.5) + 19.919
# #   }
# # 
# #   if (any(sedim$Grav > 5 & sedim$Grav <= 30))
# #   {
# #     sel.line <- which(sedim$Grav > 5 & sedim$Grav <= 30)
# #     Y1[sel.line] <- (((sedim$Grav[sel.line] - 5) * 79.674) / 25) + 59.756
# #   }
# #   if (any(sedim$Grav > 30 & sedim$Grav <= 80))
# #   {
# #     sel.line <- which(sedim$Grav > 30 & sedim$Grav <= 80)
# #     Y1[sel.line] <- (((sedim$Grav[sel.line] - 30) * 179.267) / 50) + 139.43
# #   }
# #   if (any(sedim$Grav > 80 & sedim$Grav <= 100))
# #   {
# #     sel.line <- which(sedim$Grav > 80 & sedim$Grav <= 100)
# #     Y1[sel.line] <- (((sedim$Grav[sel.line] - 80) * 79.675) / 20) + 318.697
# #   }
# # 
# # 
# # 
# #   if (any(sedim$san <= 10))
# #   {
# #     sel.line <- which(sedim$san <= 10)
# #     tmp <- sedim$san[sel.line]*6.9
# #     X1[sel.line]<- ((Y1[sel.line]*(230-tmp))/398.372) + tmp
# # 
# #   }
# # 
# #   if (any(sedim$san > 10 & sedim$san <= 50))
# #   {
# #     sel.line <- which(sedim$san > 10 & sedim$san <= 50)
# #     tmp <- (((sedim$san[sel.line] - 10) * (230 - 69)) / 40) + 69
# #     X1[sel.line] <- ((Y1[sel.line]*(230-tmp))/398.372) + tmp
# #   }
# #   if (any(sedim$san > 50 & sedim$san <= 90))
# #   {
# #     sel.line <- which(sedim$san > 50 & sedim$san <= 90)
# #     tmp <- (((sedim$san[sel.line] - 50) * (391 - 230)) / 40) + 230
# #     X1[sel.line] <- ((Y1[sel.line]*(230-tmp))/398.372) + tmp
# #   }
# #   if (any(sedim$san > 90 & sedim$san <= 100))
# #   {
# #     sel.line <- which(sedim$san > 90 & sedim$san <= 100)
# #     tmp <- (((sedim$san[sel.line] - 90) * (460 - 391)) / 10) + 391
# #     X1[sel.line] <- ((Y1[sel.line]*(230-tmp))/398.372) + tmp
# #   }
# # 
# #   sedim$x = X1 + 94 - 1.1
# #   sedim$y = 421 - Y1 - 1.3
# # 
# # crd = coord_tern()
# # sedim.tern = tlr2xy(sedim,crd,inverse=TRUE)
# 
# 
# 
# ggtern(sedim.tern,aes(x,y,z))+geom_point()
# 
#   if (any(sedim$Sand==0 & sedim$mud==0))
#   sedim[sedim$Sand==0 & sedim$mud==0,"mudsand"] <- 0
#   if (any(sedim$Sand>0 & sedim$mud==0))
#   sedim[sedim$Sand>0 & sedim$mud==0,"mudsand"] <- 10
#   if (any(sedim$Sand==0 & sedim$mud>0))
#   sedim[sedim$Sand==0 & sedim$mud>0,"mudsand"] <- 0.01
# 
#   sedim$mudsand[sedim$mudsand>10] <- 10 
#   
#   points <- data.frame(
#     rbind(c( 1,1,0,0),
#           c( 2,0.8*5,0.2*5,0.8),
#           c( 3,0.8*5,0.1*5,0.9),
#           c( 4,0.8*5,0.03*5,0.97),
#           c( 5,0.8*5,0*5,1),
#           c( 6,0.3,0.7,0.3),
#           c( 7,0.3,0.35,0.65),
#           c( 8,0.3,0.08,0.92),
#           c( 9,0.3,0,1),
#           c(10,0.05,0.95,0.05),
#           c(11,0.05,0.85,0.15),
#           c(12,0.05,0,1),
#           c(13,0.05,0.08,0.91),
#           c(14,0.01,0.97,0.03),
#           c(15,0.01,0.87,0.13),
#           c(16,0.01,0.47,0.53),
#           c(17,0.01,0.09,0.91),
#           c(18,0.01,0,1),
#           c(19,0,1,0),
#           c(20,0,0.9,0.1),
#           c(21,0,0.5,0.5),
#           c(22,0,0.9,0.1),
#           c(23,0,1,0)
#     )
#   )
#   colnames(points) = c("IDPoint","T","L","R")
# 
#   
#   base <- ggtern(data=points,aes(L,T,R)) +
#     theme_bw() + theme_hidetitles() + theme_hidearrows() +
#     geom_point(shape=21,size=10,color="blue",fill="white") +
#     geom_text(aes(label=IDPoint),color="blue")
#   print(base)
#   ggtern(aes(x=1,y=0,z=0))+theme_clockwise()
#   gravel <- data.frame(x=c(0.2,0,0),
#                   y=c(0.8,1,0.8),
#                   z=c(0,0,0.2),type="Gravel")
#   ggtern()+geom_polygon(data=gravel,aes(x,y,z,fill=type))
#   
#   muddy.gravel <- data.frame(
#                        x=c(0.2,0.1,0.35,0.7),
#                        y=c(0.8,0.8,0.3,0.3),
#                        z=c(0,0.1,0.35,0),
#                        type="Muddy gravel")
#   
#   ggtern()+geom_polygon(data=muddy.gravel,aes(x,y,z,fill=type))
#   
#   sandy.gravel <- data.frame(
#                        x=c(.15,0,0,.09),
#                        y=c(4.3,4,0.3,0.3),
#                        z=c(0.97,1,1,.91),type="Sandy gravel")
#   
#   ggtern()+geom_polygon(data=sandy.gravel,aes(x,y,z,fill=type))
#   
#   muddy.sand <- data.frame(x=c(0.1,0.03,0.09,0.35),
#                        y=c(0.8,0.8,0.3,0.3),
#                        z=c(0.9,0.97,0.91,0.65),type="Muddy sand")
#                        
#  ggtern()+geom_polygon(data=muddy.sand,aes(x,y,z,fill=type))
#                        
#   gravelly.mud <- data.frame(x=c(0.35,0.7,0.95,0.47),
#                              y=c(0.3,0.3,0.05,0.05),
#                              z=c(0.65,0.3,0.1,0.52),type="Gravelly mud")
#   gravelly.muddy.sand <- data.frame(x=c(0.35,0.09,0.1,0.47),
#                              y=c(0.3,0.3,0.05,0.05),
#                              z=c(0.65,0.92,0.9,0.52),type="Gravelly muddy sand")
#   gravelly.sand <- data.frame(x=c(0.09,0,0,0.1),
#                                     y=c(0.3,0.3,0.05,0.05),
#                                     z=c(0.92,0,0.9,1),type="Gravelly sand")
#   
#   granu.poly <- rbind(gravel,muddy.gravel,muddy.sand,sandy.gravel,gravelly.mud,
#                       gravelly.muddy.sand,gravelly.sand)
#   
#   
#     theme_classic()
#   
#     lines <- data.frame(x = c(0.1, 0.03, 0.85), 
#                         y = c(0.8, 0.8, 0.05), 
#                         z = c(0.9, 0.97, 0.14), 
#                         xend = c(1, 0.1, 1)/3, 
#                         yend = c(0, 0, 0)/3, 
#                         zend = c(0.5, 0.9, 0.1)/3)
#     
#   ggtern(sedim,aes(x=mud,z=Sand,y=Gravel))+geom_point()+
#     labs(x="Mud",z="Sand",y="Gravel")+tline(Tintercept=c(0.8,0.3,0.05,0.01))+
#     geom_Tisoprop(value=c(0.1,0.5,0.9))+
#   
#   geom_segment(data = lines,aes(x, y, z,xend = xend, yend = yend, zend = zend))
#     theme_classic()
#   
# }