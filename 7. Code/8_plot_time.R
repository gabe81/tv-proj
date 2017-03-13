### FIGURE 1, FIGURE 2 -- data overview -----------------
library(ggplot2)
library(reshape2)
library(plyr)
#count unique shows
setwd("~/Dropbox/1_Research/tv-final/2. Metadata/")

master.tv <- read.csv("TV_Time_Master.csv")
master.film <- read.csv("Film_Time_Master.csv")
tv.post <- subset(master.tv, date>1989 & date<2016)
film.post <- subset(master.film, date>1989 & date<2016)

tv.int <- data.frame(tv.post$title, tv.post$int.0.0.0.25, tv.post$int.0.25.0.50, tv.post$int.0.50.0.75, tv.post$int.0.75.1.0)
tv.genre <- data.frame(tv.post$genre, tv.post$int.0.0.0.25, tv.post$int.0.25.0.50, tv.post$int.0.50.0.75, tv.post$int.0.75.1.0)
film.int <- data.frame(film.post$title, film.post$int.0.0.0.25, film.post$int.0.25.0.50, film.post$int.0.50.0.75, film.post$int.0.75.1.0)
film.genre <- data.frame(film.post$genre, film.post$int.0.0.0.25, film.post$int.0.25.0.50, film.post$int.0.50.0.75, film.post$int.0.75.1.0)

genres.tv <- aggregate(.~tv.genre$tv.post.genre, data=tv.genre, FUN=mean)
genres.film <- aggregate(.~film.genre$film.post.genre, data=film.genre, FUN=mean)

colnames(genres.tv) <- c("genre", "number", "X0.0~0.25", "X0.25~0.50", "X0.50~0.75", "X0.75~1.0")
colnames(genres.film) <- c("genre", "number", "X0.0~0.25", "X0.25~0.50", "X0.50~0.75", "X0.75~1.0")


## create ggplot line graph for every show
for (i in 1:nrow(tv.int)) { 
  temp <- melt(tv.int[i, 1:ncol(tv.int)], id.vars = as.character(tv.int$title[i]), factorsAsStrings = FALSE)
  temp$variable <- c("title", "X0.0~0.25", "X0.25~0.50", "X0.50~0.75", "X0.75~1.0")
  title <- temp[1,2]
  temp <- temp[2:5,]
  ggobj <- ggplot(temp, aes(x=variable,y=as.numeric(value), group=1)) + geom_line(stat = "identity") + geom_point() + stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1) + ggtitle(title) + labs(x="Script Time", y="INT Percentage") + scale_y_continuous(limits=c(0, 1)) 
  print(ggobj)
  setwd("~/Dropbox/1_Research/tv-final/9. Appendix/TV_All_Time_Plots/")
  ggsave(ggobj,filename=paste(title,"_Space-Time.pdf",sep=""))
}

for (i in 1:nrow(film.int)) { 
  temp <- melt(film.int[i, 1:ncol(film.int)], id.vars = as.character(film.int$time[i]), factorsAsStrings = FALSE)
  temp$variable <- c("title", "X0.0~0.25", "X0.25~0.50", "X0.50~0.75", "X0.75~1.0")
  title <- temp[1,2]
  temp <- temp[2:5,]
  ggobj <- ggplot(temp, aes(x=variable,y=as.numeric(value), group=1)) + geom_line(stat = "identity") + geom_point() + stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1) + ggtitle(title) + labs(x="Script Time", y="INT Percentage")+ scale_y_continuous(limits=c(0, 1))
  print(ggobj)
  setwd("~/Dropbox/1_Research/tv-final/9. Appendix/Film_All_Time_Plots/")
  ggsave(ggobj,filename=paste(title,"_Space-Time.pdf",sep=""))
}

for (i in 1:nrow(genres.tv)) { 
  temp <- melt(genres.tv[i, 2:ncol(genres.tv)])
  cut <- temp[2:7,]
  title <- genres.tv[i,1]
  ggobj <- ggplot(cut, aes(x=cut$variable,y=cut$value, group=1)) + geom_line(stat = "identity") + geom_point() + stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1) + ggtitle(title) + labs(x="Script Time", y="INT Percentage")
  print(ggobj)
  setwd("~/Dropbox/1_Research/tv-final/9. Appendix/TV_Genre_Time_Plots")
  ggsave(ggobj,filename=paste(title,"_Space-Time.pdf",sep=""))
}

for (i in 1:nrow(genres.film)) { 
  temp <- melt(genres.film[i, 2:ncol(genres.film)])
  cut <- temp[2:5,]
  title <- genres.film[i,1]
  ggobj <- ggplot(cut, aes(x=cut$variable,y=cut$value, group=1)) + geom_line(stat = "identity") + geom_point() + stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1) + ggtitle(title) + labs(x="Script Time", y="INT Percentage")
  print(ggobj)
  setwd("~/Dropbox/1_Research/tv-final/9. Appendix/Film_Genre_Time_Plots")
  ggsave(ggobj,filename=paste(title,"_Space-Time.pdf",sep=""))
}


## tv -- all lines in genre
genres.tv$number <- NULL 
melted.tv <- melt(genres.tv)
ggobj <- ggplot(melted.tv, aes(x=variable,y=value, group=genre, colour=genre)) + geom_line(stat = "identity") + geom_point() + ggtitle("Space-Time in TV by Genre") + labs(x="Script Time", y="INT Percentage")
print(ggobj)

## film -- all lines
genres.film$number <- NULL 
melted.film<- melt(genres.film)
ggobj <- ggplot(melted.film, aes(x=variable,y=value, group=genre, colour=genre)) + geom_line(stat = "identity") + geom_point() + ggtitle("Space-Time in Film by Genre") + labs(x="Script Time", y="INT Percentage")
print(ggobj)

#tv -- all lines complete
tv.post <- subset(master.tv, date>1989 & date<2016)
tv.all <- tv.post[,13:30]
tv.all$title <- "TV"

tv.all <- data.frame(aggregate(. ~ title, data=tv.all, mean, na.rm = TRUE), stringsAsFactors = FALSE)
tv.all <- melt(tv.all)

tv.all$title[1:6] <- "int" 
tv.all$title[7:12] <- "dom"
tv.all$title[13:18] <- "work"
tv.all$variable <- NULL
tv.all$variable[1:6] <- c("0.0", "0.0-0.25", "0.25-0.50", "0.50-0.75", "0.75-1.00", "1.00")
tv.all$variable[7:12] <- c("0.0", "0.0-0.25", "0.25-0.50", "0.50-0.75", "0.75-1.00", "1.00")
tv.all$variable[13:18] <- c("0.0", "0.0-0.25", "0.25-0.50", "0.50-0.75", "0.75-1.00", "1.00")
tv.all <- tv.all[-c(1, 6, 7, 12, 13, 18),]

ggobj <- ggplot(tv.all, aes(x=variable,y=value, group=title, colour=title)) + geom_line(stat = "identity") + geom_point() + ggtitle("Space-Time in Television") + labs(x="Script Time", y="INT Percentage")
print(ggobj)

#film -- all lines domvwork
film.post <- subset(master.film, date>1989 & date<2016)
film.all <- film.post[,12:29]
film.all$title <- "film"
film.all <- data.frame(aggregate(. ~ title, data=film.all, mean, na.rm = TRUE), stringsAsFactors = FALSE)
film.all <- melt(film.all)

film.all$title[1:6] <- "int" 
film.all$title[7:12] <- "dom"
film.all$title[13:18] <- "work"
film.all$variable <- NULL
film.all$variable[1:6] <- c("0.0", "0.0-0.25", "0.25-0.50", "0.50-0.75", "0.75-1.00", "1.00")
film.all$variable[7:12] <- c("0.0", "0.0-0.25", "0.25-0.50", "0.50-0.75", "0.75-1.00", "1.00")
film.all$variable[13:18] <- c("0.0", "0.0-0.25", "0.25-0.50", "0.50-0.75", "0.75-1.00", "1.00")
film.all <- film.all[-c(1, 6, 7, 12, 13, 18),]

ggobj <- ggplot(film.all, aes(x=variable,y=value, group=title, colour=title)) + geom_line(stat = "identity") + geom_point() + ggtitle("Space-Time in Film") + labs(x="Script Time", y="INT Percentage")
print(ggobj)

#figure 8 -- domvwork boxplots
title_all = "INT Scene Labels by Domestic or Workplace"

film.post <- subset(master.film, date>1989 & date<2016)
film.all <- film.post[,12:29]
film.all$title <- "film"
film.all$int.mean <- rowMeans(subset(film.all, select = c(int.0.0.0.25, int.0.25.0.50, int.0.50.0.75, int.0.75.1.0 )), na.rm = TRUE)
film.all$dom.mean <- rowMeans(subset(film.all, select = c(dom.0.0.0.25, dom.0.25.0.50, dom.0.50.0.75, dom.0.75.1.0 )), na.rm = TRUE)
film.all$work.mean <- rowMeans(subset(film.all, select = c(work.0.0.0.25, work.0.25.0.50, work.0.50.0.75, work.0.75.1.0 )), na.rm = TRUE)
film.bar <- film.all[,19:22]

tv.post <- subset(master.tv, date>1989 & date<2016)
tv.all <- tv.post[,12:29]
tv.all$title <- "tv"
tv.all$int.mean <- rowMeans(subset(tv.all, select = c(int.0.0.0.25, int.0.25.0.50, int.0.50.0.75, int.0.75.1.0 )), na.rm = TRUE)
tv.all$dom.mean <- rowMeans(subset(tv.all, select = c(dom.0.0.0.25, dom.0.25.0.50, dom.0.50.0.75, dom.0.75.1.0 )), na.rm = TRUE)
tv.all$work.mean <- rowMeans(subset(tv.all, select = c(work.0.0.0.25, work.0.25.0.50, work.0.50.0.75, work.0.75.1.0 )), na.rm = TRUE)
tv.bar <- tv.all[,19:22]


all <- rbind(film.bar, tv.bar)
all.melt <- melt(all)

means.medium <- aggregate(.~title, all, mean)

shapiro.test(film.bar$int.mean)
shapiro.test(tv.int$int.percent)

boxplot.medium <- ggplot(all.melt, aes(title, value))
boxplot.medium + geom_boxplot(outlier.shape = NA, aes(fill=variable)) + 
  scale_fill_brewer(palette="Pastel2") + 
  ggtitle("Percentage of Scenes Labelled as Domestic or Workplace") + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x=element_blank())
