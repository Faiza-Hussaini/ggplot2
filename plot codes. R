mydata <- read.csv("mydata.csv", na.strings = c("", "NA"))

# Frequency Tables (FDT)
FDTQL <- function(x) {
  AbsFreq <- table(x)
  RelFreq <- round(prop.table(AbsFreq), 2)
  CumFreq <- cumsum(RelFreq)
  cbind(AbsFreq, RelFreq, CumFreq)
}
FDTQL(mydata$Gendar)


# Tidyverse library & ggplot2 visualization
library(tidyverse)
mydata<- read.csv("mydata.csv")  
head(mydata)
colnames (mydata)

# Scatter plot: Age vs Height by Gendar
ggplot( data = mydata, aes(x = Height, y = Age, color = Gendar)) +
  geom_point()

# Boxplot: Age distribution by Gendar
ggplot( data = mydata, aes(x = Gendar, y = Age)) +
  geom_boxplot()

# Facet plot: Age vs Height by Gendar for each Gendar
ggplot(data = mydata, aes(x = Height, y = Age, color = Gendar)) +
  geom_point() +
  facet_wrap(~ Gendar)

# Smoot line with sccater plot 
ggplot(data = mydata, aes(x = Height, y = Age)) +
  geom_point() +
  geom_smooth()

# Histogram of Age
ggplot( data = mydata, aes(x = Age)) +
  geom_histogram(bins = 10)

# Density plot of Age
ggplot(data = mydata, aes(x = Age)) +
  geom_density()

# Bar Plot of gendar count 
ggplot(data = mydata , aes(x = Gendar)) + geom_bar()
