library(ggplot2)
library(reshape2)

#import master metadata tables and extract relevant information
setwd("~/Dropbox/1_Research/tv-final/2. Metadata")
all.tv <- read.csv("TV_Scripts_Master.csv")
all.tv$medium <- "tv"
tv.int <- subset(all.tv, date>1989 & date<2016, select=c(medium, title, genre, int.percent))

all.film <- read.csv("Film_Scripts_Master.csv")
all.film$medium <- "film"
film.int <- subset(all.film, date>1989 & date<2016, select=c(medium, title, genre, int.percent))

# test for normality
shapiro.test(film.int$int.percent)
shapiro.test(tv.int$int.percent)

hist(film.int$int.percent)
hist(tv.int$int.percent)

#combine data tables and aggregate values
mydata <- rbind(film.int, tv.int)

means.medium <- aggregate(int.percent ~ medium, mydata, mean)


#boxplot gg code
title_all = "INT Scene Percentage in Film and TV"
title_genre = "INT Scene Percentage by Genre in Film and TV"

theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top", legend.title=element_blank(), panel.grid.major.x=element_blank())

boxplot.medium <- ggplot(mydata, aes(factor(medium), int.percent))
boxplot.genre <- ggplot(mydata, aes(factor(genre), int.percent))

#boxplot total tv v film
boxplot.medium + geom_boxplot(outlier.shape = NA, aes(fill=factor(medium))) + 
  scale_fill_brewer(palette="Pastel2") + 
  ggtitle(title_all) + 
  guides(fill=FALSE) +
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", 
               fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) }) +
  geom_text(data = means.medium, 
            aes(label = round(int.percent, digits = 3), 
                y = int.percent + 0.05)) +
  geom_text(data = means.medium, aes(label = medium)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x=element_blank())

## BOXPLOTS IN EMAIL
boxplot.genre + geom_boxplot(outlier.shape = NA, aes(fill=factor(medium))) + 
  scale_fill_brewer(palette="Pastel2") + 
  ggtitle(title_genre) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x=element_blank())

# text for significance using tukey
myData = read.csv("TV_Scripts_Master.csv")
attach(myData)
results = DTK.test(int.percent,genre,0.05)
results
DTK.plot(results)

myData = read.csv("Film_Scripts_Master.csv")
attach(myData)
results = DTK.test(int.percent,genre,0.05)
results
DTK.plot(results)

