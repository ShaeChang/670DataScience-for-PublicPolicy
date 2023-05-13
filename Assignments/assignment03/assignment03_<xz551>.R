glimpse(storms)
library(tidyverse)
ggplot(data = storms) +
  geom_point(mapping = aes(x = pressure, y = wind))


