 ## Heatmap 

library(ggplot2)
library(ggmap)

pred.points <- ggplot(data=plot_data, aes(x = plot_data$longitude,
                                          y = plot_data$latitude,
                                          colour = value)) +
  geom_point()

print(pred.points)


plot_data <- d[,3:4]
plot_data$value <- d$value

plot_less <- plot_data[1:500,]

this_might_work <- poll[poll$epoch == 735101,]

library(kriging)
krig_plot <- kriging(this_might_work$longitude, this_might_work$latitude, this_might_work$value, lags=2)
