
library(ggplot2)
library(tidyverse)


#1. Import data

data <- read_csv("./data/bcfa_C17_fa.csv")

str(data) #structure not STRING

view(data)



#2. Visualise the data

ggplot(forest, aes(x=QuadDiam, y=Density, colour=StandType)) +
  geom_point() + geom_smooth(method="lm", alpha=0.2)