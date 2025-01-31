library(readxl)
library(here)
library(tidyr)
library(ggplot2)


ggg <- read.csv(here::here("Data_folder","Dia_class_proportions.csv"))

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
