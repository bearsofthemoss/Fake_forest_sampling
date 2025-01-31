library(R.matlab)
library(ggplot2)
library(gridExtra)
library(here)


Uni_resultsModel1_Folder <-readMat(here::here( "Data_folder","matlab_output",
                                               "Uni_resultsModel1_v7.mat"))

J_resultsModel1_Folder <-readMat(here::here( "Data_folder","matlab_output",
                                             "J_resultsModel1_v7.mat"))

Arb_resultsModel1_Folder <-readMat(here::here( "Data_folder","matlab_output",
                                               "Arb_resultsModel1_v7.mat"))


propU <- as.data.frame(Uni_resultsModel1_Folder$Proportional)
propJ <- as.data.frame(J_resultsModel1_Folder$Proportional)
propArb <- as.data.frame(Arb_resultsModel1_Folder$Proportional)



# These should be the same across any file
truncLR <- as.data.frame(Uni_resultsModel1_Folder$TruncatedUniform)
triang   <- as.data.frame(Uni_resultsModel1_Folder$TriangularUshaped)


# Step 1 give column names
colnames(propU) <- c("10-21","22-32","33-44","45-55","56-66","66-78","79-89","90-100")
colnames(propJ) <- c("10-21","22-32","33-44","45-55","56-66","66-78","79-89","90-100")
colnames(propArb) <- c("10-21","22-32","33-44","45-55","56-66","66-78","79-89","90-100")
colnames(truncLR) <- c("10-21","22-32","33-44","45-55","56-66","66-78","79-89","90-100")
colnames(triang) <- c("10-21","22-32","33-44","45-55","56-66","66-78","79-89","90-100")

# step 2, give row names (will be replaced with letters later)
propU$row <- 1
propJ$row <- 1
propArb$row <- 1

truncLR$row <- c(1:16)
triang$row <- c(1:8)

# Step 3 fix truncated
truncL <- truncLR[1:8,]
truncR <- truncLR[c(15,14,13,12,11,10,9,8),]


#  add row num to separate rows from shapes
propU$row_num <- "Uniform"
propJ$row_num <- "Reverse-J"
propArb$row_num <- "Even-aged"
truncL$row_num <- truncL$row
truncR$row_num <- c(1:8)
triang$row_num <- triang$row

# Step 4 add Dist name
truncL$Distribution <- "Weighted left"
truncR$Distribution <- "Weighted right"
triang$Distribution <- "Triangular"

# step 5 assign row letters?
propU$Distribution <- "Proportional"
propJ$Distribution <- "Proportional"
propArb$Distribution <- "Proportional"


fff <- rbind(propU, propArb, propJ, truncL, truncR, triang)

## give the shape assignment
fff[fff$row_num=="1","sel_shape"] <- "1"    
fff[fff$row_num=="2","sel_shape"] <- "2"    
fff[fff$row_num=="3","sel_shape"] <- "3"    
fff[fff$row_num=="4","sel_shape"] <- "4"    
fff[fff$row_num=="5","sel_shape"] <- "5"    
fff[fff$row_num=="6","sel_shape"] <- "6"    
fff[fff$row_num=="7","sel_shape"] <- "7"    
fff[fff$row_num=="8","sel_shape"] <- "8"    
fff[fff$row_num=="Uniform","sel_shape"] <- "Proportional"    
fff[fff$row_num=="Reverse-J","sel_shape"] <- "Proportional"    
fff[fff$row_num=="Even-aged","sel_shape"] <- "Proportional" 



fff$sel_shape <- factor(fff$sel_shape, levels=c("Proportional",1:8))

fff$Distribution <- factor(fff$Distribution, levels=c(
  "Proportional","Triangular","Weighted left","Weighted right"))

head(fff)

# gather for graphing
ggg<- gather(fff, "size.class","value",1:8)

head(ggg)

ggg <- ggg[ggg$value!=0,]

ggplot(ggg, aes(x=size.class, y=row_num, size=value/100,
                     col=value/100,
                     shape=sel_shape))+geom_point()+
  facet_wrap(~Distribution, ncol=2, scales="free_y")+
  labs(x="Diameter size class (centimeters)",
       y=  "Sampling strategies",
       col="Proportion of size class",
       size="Proportion of size class",
       shape="Size class category")+
  scale_size(range = c(2, 8))+
  guides(size=guide_legend(title="Proportion of size class"))+
  
  scale_colour_gradientn(colours = c("brown","darkgrey","lightblue","darkblue"),
                         values = c(0.6,0.4,0.2,0))+ 
  theme_bw()+theme(strip.background =element_rect(fill="white"))+
  theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(strip.text.x = element_text(size = 14),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "right")+
  scale_shape_manual(values=c( 8, 14,10, 12,  9,  3, 17, 15,19))+
  geom_hline(yintercept=c(1.5, 2.5, 3.5,4.5,5.5,6.5,7.6), linetype= "dashed")



write.csv(ggg, file=here::here("Data_folder","Dia_class_proportions.csv"))
