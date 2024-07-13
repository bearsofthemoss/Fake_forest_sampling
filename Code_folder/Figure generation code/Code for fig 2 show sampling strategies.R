library(readxl)
library(here)

fd <- file.path(here::here(),"Data_folder","forest_distributions")

#(for figure 2)
sc<-  read_excel(
  file.path(fd,"Alex_re_arrange_4_29.xlsx"), sheet="size.class.ay")


head(sc)
gs<-gather(sc, "size.class","value",3:10)
gs$row<-as.factor(gs$row)
head(gs)
table(gs$Distribution)

gs[gs$Distribution=="Triangle shaped","Distribution"]<-"not used"
gs[gs$Distribution=="Truncated triangles left","Distribution"]<-"not used"
gs[gs$Distribution=="Truncated triangles right","Distribution"]<-"not used"

gs <- gs[!gs$Distribution=="not used", ]

gs[gs$Distribution=="Proportional uniform","Distribution"]<-"Uniform"
gs[gs$Distribution=="Forests in Mexico","Distribution"]<-"Reverse-J"
gs[gs$Distribution=="Proportional arbogast","Distribution"]<-"Even-aged distribution"

gs[gs$Distribution=="Triangular left","Distribution"]<- "Skewed left"
gs[gs$Distribution=="Triangular right","Distribution"]<- "Skewed right"


table(gs$Distribution)

gs$group<-paste(gs$row, gs$Distribution)
gs$Distribution<-factor(gs$Distribution, levels=c( "Reverse-J", "Even-aged distribution","Uniform",
                                                   "Skewed left","Parabolic","Skewed right",
                                                   "Truncated uniform left","Truncated uniform right"))

gs<-gs[gs$value>0,]
table(gs$size.class)

tail(gs)
gs$size.class[gs$size.class == "sc1"] <- "10-20"
gs$size.class[gs$size.class == "sc2"] <- "20-30"
gs$size.class[gs$size.class == "sc3"] <- "30-40"
gs$size.class[gs$size.class == "sc4"] <- "40-50"
gs$size.class[gs$size.class == "sc5"] <- "50-60"
gs$size.class[gs$size.class == "sc6"] <- "60-70"
gs$size.class[gs$size.class == "sc7"] <- "70-80"
gs$size.class[gs$size.class == "sc8"] <- "80-90"

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


gs$new_order <- as.character(gs$row)
gs[gs$Distribution=="Reverse-J","new_order"] <- "Reverse-J"
gs[gs$Distribution=="Even-aged distribution","new_order"] <- "Even-aged distribution"
gs[gs$Distribution=="Uniform","new_order"] <- "Uniform"

gs$new_facet <- as.character(gs$Distribution)
gs[gs$Distribution=="Reverse-J","new_facet"] <- "Proportional"
gs[gs$Distribution=="Even-aged distribution","new_facet"] <- "Proportional"
gs[gs$Distribution=="Uniform","new_facet"] <- "Proportional"

gs$new_facet <- factor(gs$new_order, levels=c("Reverse-J","Even-aged distribution","Uniform","1","2","3","4","5","6","7","8"))

gs$new_facet <- factor(gs$new_facet, levels=c("Proportional","Parabolic","Skewed left","Skewed right","Truncated uniform left","Truncated uniform right"))

ggplot(gs, aes(x=size.class, y=new_order, size=value/100, col=value/100))+geom_point()+
  facet_wrap(~new_facet, ncol=2, scales="free_y")+
  labs(x="Diameter size class (centimeters)",
        y=  "Sampling strategies",
       col="Proportion of size class",
       size="Proportion of size class")+
  scale_size(range = c(2.5, 6))+
  #guides(size=guide_legend(title="Proportion of size class"))+
  
  scale_colour_gradientn(colours = c("brown","darkgrey","lightblue","darkblue"),
                         values = c(0.6,0.4,0.2,0))+ 
  theme_bw()+theme(strip.background =element_rect(fill="white"))+
  theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(strip.text.x = element_text(size = 14),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "right")+
  geom_hline(yintercept=c(1.5, 2.5, 3.5,4.5,5.5,6.5,7.6), linetype= "dashed")

