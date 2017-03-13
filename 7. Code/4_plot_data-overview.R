### FIGURE 1, FIGURE 2 -- data overview -----------------
library(ggplot2)

#count unique shows
master <- read.csv("TV_Scripts_Master.csv")
shows <- gsub("_\\S*","",master$title)
table <- as.matrix(table(shows))

## Figure 1

#histograms for media Distribution
tv <- as.data.frame(read.csv("TV_Scripts_Master.csv"))
film <- as.data.frame(read.csv("Film_Scripts_Master.csv"))

tv.post <- subset(tv, date>1989 & date<2016)
film.post <-subset(film, date>1989 & date<2016)

hist.tv <- hist(tv.post$date)
hist.tv2 <- hist(tv$date, breaks=seq(1990,2015,by=5), main="")
hist.tv2 <- hist(tv$date, breaks=60, main="TV Shows by Date", xlab = "year")

hist.film <- hist(film.post$date)
hist.film <- hist(film$date, breaks=seq(1990,2015,by=5), main="")
hist.film <- hist(film$date, breaks=60, main="Films by Date", xlab = "year")

ggplot() + 
  geom_histogram(data=film.post, aes(date, fill="Film", y= -..count..), binwidth = 1) +
  geom_histogram(data=tv.post, aes(date, fill="Television", y= ..count..), binwidth = 1) +
  ggtitle("Media Distribution by Date") + xlab("Year") + ylab("Number of Screenplays") +
  scale_fill_brewer(palette="Pastel2")


## Figure 2. Paired bar graphs for genre

#paired bar graphs
tv <- as.data.frame(read.csv("TV_Scripts_Master.csv"))
film <- as.data.frame(read.csv("Film_Scripts_Master.csv"))

tv.post <- subset(tv, date>1989 & date<2016)
film.post <-subset(film, date>1989 & date<2016)

grouped.tv <- as.data.frame(table(tv.post$genre))
grouped.film <- as.data.frame(table(film.post$genre))

setwd("~/Dropbox/1_Research/tv-final/5. Plot Tables/INT-EXT Ratios by Genre")
genre.info <- read.csv("Grouped_Genre_Both.csv")

ggplot(genre.info, aes(x = genre, y = Freq, fill=medium)) +
  geom_bar(stat='identity', position = position_dodge()) + ggtitle("Dataset Breakdown by Genre") + ylab("Number of Screenplays") + scale_fill_brewer(palette="Pastel2")

