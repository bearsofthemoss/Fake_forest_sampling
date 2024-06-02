library(readxl)

#(for figure 2)
sc<-  as.data.frame(read_excel("Alex_re_arrange_4_29.xlsx", sheet="size.class"))


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


ggplot(gs, aes(x=size.class, y=value, color=row))+geom_point()+
  facet_wrap(~Distribution, ncol=3)+geom_line(aes(group=group))+ylab("Proportion of distribution (%)")+xlab("Tree diameter class (cm)")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(strip.text.x = element_text(size = 14),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(legend.position="none")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




## 4-27
names(gs)

#write.csv(gs, file="new_fig3_ary.csv")

# read in new formatted .csv
#gs<-read.csv( file="new_fig3_ary.csv")

gs$Dist_name<-factor(gs$Distribution , levels=c("Proportional","Parabolic",
                                            "Skewed left","Skewed right",
                                            "Truncated uniform left","Truncated uniform right"))

# gs$legend_use <- factor(gs$legend_use, levels=c("Reverse-J","Even-aged distribution","Uniform",
#                                                 "Sampling strategy 1","Sampling strategy 2","Sampling strategy 3","Sampling strategy 4",
#                                                 "Sampling strategy 5","Sampling strategy 6","Sampling strategy 7","Sampling strategy 8"))

library(RColorBrewer)

gm <- RColorBrewer::brewer.pal(n=9, name="BrBG")[c(1:4,5:9)]


ggplot(gs, aes(x=size.class, y=value, color=row))+geom_point()+
  facet_wrap(~Dist_name, ncol=2)+geom_line(aes(group=group))+ylab("Proportion of distribution (%)")+xlab("Tree diameter class (cm)")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+ 
  theme(text=element_text(size=16))+
  scale_color_manual(values=c("red","orange","blue",gm), name="")+
  theme(strip.text.x = element_text(size = 14),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

fffig2

legend_use


ggplot(gs, aes(x=size.class, y=row, size=value))+geom_point()+
  facet_wrap(~Distribution, ncol=3, scales="free_y")+
  xlab("Diameter size class (centimeters)")+
  guides(size=guide_legend(title="Proportion of size class"))+
  ylab("Sampling strategy")+
  theme_bw()+theme(strip.background =element_rect(fill="white"))+theme(plot.title = element_text(hjust = 0.5))+  theme(text=element_text(size=16))+
  theme(strip.text.x = element_text(size = 14),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "bottom")+
  geom_hline(yintercept=c(1.5, 2.5, 3.5), linetype= "dashed")
