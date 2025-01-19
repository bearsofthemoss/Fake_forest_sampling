
##################################### FUNCTIONS


### Database sampling function

fun_DataBase <- function(DataBase,nEst, EstT, SamSizeT){
  
  TwoDimData<-as.data.frame(DataBase[1:nEst, 1:10, 1])
  VecDataAc<-if(nEst==1){c(TwoDimData[,1])}else{if(nEst>1)
  {c(TwoDimData[,1],TwoDimData[,2],TwoDimData[,3],TwoDimData[,4],TwoDimData[,5],TwoDimData[,6],TwoDimData[,7],TwoDimData[,8],TwoDimData[,9],TwoDimData[,10])}else{NA}}
  
  
  # Plot 3D
  for (i in 1:9999)
  {
    TwoDimData<-as.data.frame(DataBase[1:nEst, 1:10, i+1])
    VecData<-if(nEst==1){c(TwoDimData[,1])}else{if(nEst>1)
    {c(TwoDimData[,1],TwoDimData[,2],TwoDimData[,3],TwoDimData[,4],TwoDimData[,5],TwoDimData[,6],TwoDimData[,7],TwoDimData[,8],TwoDimData[,9],TwoDimData[,10])}else{NA}}
    VecDataAc<-c(VecDataAc,VecData)
  }
  
  VecParabolic<-data.frame(EstT,SamSizeT,VecDataAc)
  
}



### Multy Histograms function

fun_MultyHist <- function(Dist,nEst,VectorA, EstE, SamSizeV){
  
  for (i in 1:nEst)
  {
    prueba3<-VectorA[VectorA$EstT==EstE[i],]
    Estra <- EstE[i]
    minV<- min(prueba3$VecDataAc)
    maxV<- max(prueba3$VecDataAc)
    
    myhist <- list()
    
    for (j in 1:10)
    {
      
      prueba4<-prueba3[prueba3$SamSizeT==SamSizeV[j],]
      SamSizeN <- SamSizeV[j]
      
      myhist[[j]] <- ggplot(prueba4, aes(VecDataAc)) + geom_histogram() + 
        ggtitle(SamSizeN) + theme(plot.title = element_text(size = 8), 
                                  axis.title = element_text(size = 6), axis.text = element_text(size = 5)) +
        xlim(minV, maxV)
    }
    
    prueba<-grid.arrange(myhist[[1]], myhist[[2]],myhist[[3]],myhist[[4]],myhist[[5]],
                         myhist[[6]],myhist[[7]],myhist[[8]],myhist[[9]],myhist[[10]], ncol = 4, top= Estra)
    
    ggsave(file=paste(Dist,"-",Estra,".pdf"), prueba)
    
  }
  
}



### Probability tables function


fun_ProbTables <- function(Dist,nEst,VectorA, EstE, SamSizeV){
  
  myframe <- c("Est","Sample","p5","p10","p15","p20","p25","p30","p40","p50")
  
  for (i in 1:nEst)
  {
    #i<-1
    prueba3<-VectorA[VectorA$EstT==EstE[i],]
    
    Estra <- EstE[i]
    
    for (j in 1:10)
    {
      #j<-1
      prueba4<-prueba3[prueba3$SamSizeT==SamSizeV[j],]
      prueba4e<-prueba4$VecDataAc*100
      p5<-sum(ifelse(prueba4e>=(-5) & prueba4e<=5,1,0))/10000
      p10<-sum(ifelse(prueba4e>=(-10) & prueba4e<=10,1,0))/10000
      p15<-sum(ifelse(prueba4e>=(-15) & prueba4e<=15,1,0))/10000
      p20<-sum(ifelse(prueba4e>=(-20) & prueba4e<=20,1,0))/10000
      p25<-sum(ifelse(prueba4e>=(-25) & prueba4e<=25,1,0))/10000
      p30<-sum(ifelse(prueba4e>=(-30) & prueba4e<=30,1,0))/10000
      p40<-sum(ifelse(prueba4e>=(-40) & prueba4e<=40,1,0))/10000
      p50<-sum(ifelse(prueba4e>=(-50) & prueba4e<=50,1,0))/10000
      SamSizeN <- SamSizeV[j]
      
      myframe_S <- c(Estra,SamSizeN,p5,p10,p15,p20,p25,p30,p40,p50)
      
      myframe<-rbind(myframe,myframe_S)
      
    }
    
  }
  
  write.csv(myframe, file = paste("TableProb_",Dist,".csv"))
  
}



### Mean probability tables function


fun_MeanTables <- function(Dist,nEst,VectorA, EstE, SamSizeV)
{
  myframe <- c("Est","10","15","30","45","80","130","215","360", "600", "1000")
  myframeV <-as.vector(myframe)
  
  for (i in 1:nEst)
  {
    #i<-1
    prueba3<-VectorA[VectorA$EstT==EstE[i],]
    
    Estra <- EstE[i]
    
    myframeEst <- c(Estra)
    
    for (j in 1:10)
    {
      #j<-1
      prueba4<-prueba3[prueba3$SamSizeT==SamSizeV[j],]
      prueba4e<-prueba4$VecDataAc*100
      AverUnc<-mean(abs(prueba4e))
      
      myframeEst <- cbind(myframeEst,AverUnc)
      myframeEstV <- as.vector(myframeEst)
      
    }
    myframeV<-rbind(myframeV,myframeEstV)
  }
  
  colnames(myframeV)<-NULL
  rownames(myframeV)<-NULL
  
  write.csv(myframeV, file = paste("TableMean_",Dist,".csv"))
  
}



