library(readxl)
library(here)
library(tidyr)
library(ggplot2)

fd <- file.path(here::here(),"Data_folder","forest_distributions")

#(for figure 2)
sc<-  read_excel(here::here(fd,"Alex_re_arrange_4_29.xlsx"))


head(sc)
gs<-gather(sc, "size.class","value",3:10)
gs$row<-as.factor(gs$row)
head(gs)
table(gs$Distribution)

gs[gs$Distribution=="Triangle shaped","Distribution"]<-"Triangular"
gs[gs$Distribution=="Truncated triangles left","Distribution"]<-"not used"
gs[gs$Distribution=="Truncated triangles right","Distribution"]<-"not used"


gs[gs$Distribution=="Triangular left","Distribution"]<- "not used" # was skewed left
gs[gs$Distribution=="Triangular right","Distribution"]<- "not used"# was skewed right

gs[gs$Distribution=="Parabolic","Distribution"]<- "not used"# was skewed right


gs <- gs[!gs$Distribution=="not used", ]

gs[gs$Distribution=="Truncated uniform left","Distribution"]<- "Weighted left"
gs[gs$Distribution=="Truncated uniform right","Distribution"]<- "Weighted right"


gs[gs$Distribution=="Proportional uniform","Distribution"]<-"Uniform"
gs[gs$Distribution=="Forests in Mexico","Distribution"]<-"Reverse-J"
gs[gs$Distribution=="Proportional arbogast","Distribution"]<-"Even-aged distribution"


table(gs$Distribution)

gs$group<-paste(gs$row, gs$Distribution)
gs$Distribution<-factor(gs$Distribution, levels=c( "Reverse-J", "Even-aged distribution","Uniform",
                                              "Parabolic","Weighted left","Weighted right",
                                              "Triangular"))

gs<-gs[gs$value>0,]
table(gs$size.class)

table(gs$Distribution)


tail(gs)
gs$size.class[gs$size.class == "sc1"] <- "10-21"
gs$size.class[gs$size.class == "sc2"] <- "22-32"
gs$size.class[gs$size.class == "sc3"] <- "33-44"
gs$size.class[gs$size.class == "sc4"] <- "45-55"
gs$size.class[gs$size.class == "sc5"] <- "56-66"
gs$size.class[gs$size.class == "sc6"] <- "66-78"
gs$size.class[gs$size.class == "sc7"] <- "79-89"
gs$size.class[gs$size.class == "sc8"] <- "90-100"

table(gs$Distribution)

# 
# ggplot(gs, aes(x=size.class, y=value, color=row))+geom_point()+
#   facet_wrap(~Distribution, ncol=3)+geom_line(aes(group=group))+ylab("Proportion of distribution (%)")+xlab("Tree diameter class (cm)")+
#   theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
#   theme(strip.text.x = element_text(size = 14),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
#   theme(legend.position="none")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# 
# ## 4-27
# 
# #write.csv(gs, file="new_fig3_ary.csv")
# 
# # read in new formatted .csv
# #gs<-read.csv( file="new_fig3_ary.csv")
# 
# gs$Dist_name<-factor(gs$Distribution , levels=c("Proportional","Parabolic",
#                                             "Skewed left","Skewed right",
#                                             "Truncated uniform left","Truncated uniform right"))
# 
# # gs$legend_use <- factor(gs$legend_use, levels=c("Reverse-J","Even-aged distribution","Uniform",
# #                                                 "Sampling strategy 1","Sampling strategy 2","Sampling strategy 3","Sampling strategy 4",
# #                                                 "Sampling strategy 5","Sampling strategy 6","Sampling strategy 7","Sampling strategy 8"))
# 
# library(RColorBrewer)
# 
# gm <- RColorBrewer::brewer.pal(n=9, name="BrBG")[c(1:4,5:9)]
# 
# 
# 
# ggplot(gs, aes(x=size.class, y=value, color=row))+geom_point()+
#   facet_wrap(~Dist_name, ncol=2)+geom_line(aes(group=group))+ylab("Proportion of distribution (%)")+xlab("Tree diameter class (cm)")+
#   theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+
#   theme(text=element_text(size=16))+
#   scale_color_manual(values=c("red","orange","blue",gm), name="")+
#   theme(strip.text.x = element_text(size = 14),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 

gs$new_order <- as.character(gs$row)
gs[gs$Distribution=="Reverse-J","new_order"] <- "Reverse-J"
gs[gs$Distribution=="Even-aged distribution","new_order"] <- "Even-aged distribution"
gs[gs$Distribution=="Uniform","new_order"] <- "Uniform"

gs$new_facet <- as.character(gs$Distribution)
gs[gs$Distribution=="Reverse-J","new_facet"] <- "Proportional"
gs[gs$Distribution=="Even-aged distribution","new_facet"] <- "Proportional"
gs[gs$Distribution=="Uniform","new_facet"] <- "Proportional"

gs$new_order <- factor(gs$new_order, levels=c("Reverse-J","Even-aged distribution","Uniform","1","2","3","4","5","6","7","8"))

gs$new_facet <- factor(gs$new_facet, levels=c("Proportional","Parabolic","Skewed left","Skewed right","Weighted left","Weighted right", "Triangular"))


table(gs$new_facet)
## give the color assignment
  gs[gs$new_order=="1","sel_shape"] <- "1"    
  gs[gs$new_order=="2","sel_shape"] <- "2"    
  gs[gs$new_order=="3","sel_shape"] <- "3"    
  gs[gs$new_order=="4","sel_shape"] <- "4"    
  gs[gs$new_order=="5","sel_shape"] <- "5"    
  gs[gs$new_order=="6","sel_shape"] <- "6"    
  gs[gs$new_order=="7","sel_shape"] <- "7"    
  gs[gs$new_order=="8","sel_shape"] <- "8"    
  gs[gs$new_order=="Uniform","sel_shape"] <- "Proportional"    
  gs[gs$new_order=="Reverse-J","sel_shape"] <- "Proportional"    
  gs[gs$new_order=="Even-aged distribution","sel_shape"] <- "Proportional"    

  head(gs)
  
f3 <- ggplot(gs, aes(x=size.class, y=new_order, size=value/100,
               col=value/100,
               shape=sel_shape))+geom_point()+
  facet_wrap(~new_facet, ncol=2, scales="free_y")+
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


dpi=900    #pixels per square inch
tiff(here::here("Fig_3.tif"), width=10*dpi, height=8*dpi, res=dpi)
f3
dev.off()


