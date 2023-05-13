
#03_data-visualization

 library(tidyverse)
 #the plot we created
 ggplot(data = storms) + geom_point(mapping = aes(x = pressure, y = wind))
 