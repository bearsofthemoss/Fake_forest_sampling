


### read in d15 and combined



head(d15)

head(combined)


below <- aggregate(list(less_than15 = d15$p15) , by = list(dist_name = d15$dist_name,
                              number_trees = d15$Sample,
                             Diam_distribution = d15$Diam_distribution,
                              model = d15$model
                             ),
          FUN="mean", na.rm=T)
below$id <- paste(below$model, below$Diam_distribution, below$Est, below$number_trees, below$dist_name, below$Diam_distribution)

avg <- aggregate(list(average = mug$value) , by = list(dist_name = mug$dist_name,
                                     number_trees = mug$number_trees,
                                     #Est = mug$Est,
                                     model = mug$model,
                                     Diam_distribution = mug$Diam_distribution),
                 FUN="mean", na.rm=T)
avg$id <- paste(avg$model, avg$Diam_distribution, avg$Est, avg$number_trees, avg$dist_name, avg$Diam_distribution)


head(avg)
head(below)

avg$less_than15 <- below$less_than15[match(avg$id, below$id)]

table(avg$id %in% below$id)

# Specify the starting and ending colors
start_color <- "darkred"
mid_color <- "lightgreen"
end_color <- "darkgreen"

# Create a color palette with 9 colors
color_palette <- colorRampPalette(c(start_color, mid_color, end_color))(9)

# Display the resulting color palette
print(color_palette)


ggplot(avg, aes(x=average, y=less_than15*100 , col=number_trees, shape=model))+
  facet_grid( dist_name ~ Diam_distribution)+ geom_point()+
  scale_color_manual(values = color_palette) +
  labs(x="Average uncertainty value (%)", y="Percent of model runs < 15% uncertainty")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank())

head(avg)
