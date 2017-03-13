## Figure 5. All Int-Ext labels by Scene type
library(ggplot2)


setwd("~/Dropbox/1_Research/tv-final/5. Plot Tables/INT-EXT Labels by Space Type")

# Read all-csv
tv.mfw.int <- read.csv("TV_MFW_Int.csv", header = F)
tv.mfw.ext <- read.csv("TV_MFW_Ext.csv", header = F)
film.mfw.int <- read.csv("Film_MFW_Int.csv", header = F)
film.mfw.ext <- read.csv("Film_MFW_Ext.csv", header = F)

#calculate percentages
tv.mfw.int$perc <- tv.mfw.int$V3 / sum(tv.mfw.int$V3) * 100
tv.mfw.ext$perc <- tv.mfw.ext$V3 / sum(tv.mfw.ext$V3) * 100
film.mfw.int$perc <- film.mfw.int$V3 / sum(film.mfw.int$V3) * 100
film.mfw.ext$perc <- film.mfw.ext$V3 / sum(film.mfw.ext$V3) * 100

colnames(tv.mfw.int) <- c("medium","words", "freq", "type1", "type2", "perc")
colnames(tv.mfw.ext) <- c("medium", "words", "freq", "type1", "perc")

colnames(film.mfw.int) <- c("medium","words", "freq", "type1", "type2", "perc")
colnames(film.mfw.ext) <- c("medium", "words", "freq", "type1", "perc")

bar.mfw.int <- rbind(tv.mfw.int, film.mfw.int)
bar.mfw.ext <- rbind(tv.mfw.ext, film.mfw.ext)

tv.domestic.mean <- tv.mfw.int[tv.mfw.int$type2 == "domestic", ]
tv.dom.perc <- (sum(tv.domestic.mean$freq))/(sum(tv.mfw.int$freq))

tv.work.mean <- tv.mfw.int[tv.mfw.int$type2 == "workplace", ]
tv.work.perc <- (sum(tv.work.mean$freq))/(sum(tv.mfw.int$freq))

film.domestic.mean <- film.mfw.int[film.mfw.int$type2 == "domestic", ]
film.dom.perc <- (sum(film.domestic.mean$freq))/(sum(film.mfw.int$freq))

film.work.mean <- film.mfw.int[film.mfw.int$type2 == "workplace", ]
film.work.perc <- (sum(film.work.mean$freq))/(sum(film.mfw.int$freq))


short.tv <- tv.mfw.int[c(1:20),]
short.film <- film.mfw.int[c(1:20),]

short.tv.ext <- tv.mfw.ext[c(1:20),]
short.film.ext <- film.mfw.ext[c(1:20),]

dom.work <- data.frame()
dom.work <- rbind(dom.work, tv.dom.perc, film.dom.perc)
dom.work <- row.names("tv", "film")
dom.work <- colnames("tv", "film")

#create tables for cleaning prior to plotting -- removed values that didn't have double values
short.int <- rbind(short.tv, short.film)
write.csv(short.int, "Short_INT.csv")

short.ext <- rbind(short.tv.ext, short.film.ext)
write.csv(short.ext, "Short_EXT.csv")
#stacked


## figure 6 -- top 10 word counts

short.int <- read.csv("Short_INT.csv", header= TRUE)
#top 10 int
ggplot(short.int, aes(x = reorder(words, perc), y=perc, fill=medium)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  xlab("") + 
  ylab("Percentage of Total Scenes") + 
  ggtitle("Top 10 Words in INT Scene Directions") +
  coord_flip() +
  scale_fill_brewer(palette="Pastel2") 

short.ext <- read.csv("Short_EXT.csv", header= TRUE)
#top 10 int
ggplot(short.ext, aes(x = reorder(words, perc), y=perc, fill=medium)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  xlab("") + 
  ylab("Percentage of Total Scenes") + 
  ggtitle("Top 10 Words in EXT Scene Directions") +
  coord_flip() +
  scale_fill_brewer(palette="Pastel2") 


#figure 7 -- int by building
bar.mfw.int$type1 <- factor(bar.mfw.int$type1, levels = bar.mfw.int$type1[order(bar.mfw.int$perc, decreasing = FALSE)])

ggplot(bar.mfw.int, aes(factor(type1), y=perc, fill=medium)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  ylab("Percentage of Total Scenes") +
  xlab("") +
  ggtitle("INT Words by Type of Building") +
  coord_flip() +
  scale_fill_brewer(palette="Pastel2")

#Figure 7 -- ext by type
bar.mfw.ext$type1 <- factor(bar.mfw.ext$type1, levels = bar.mfw.ext$type1[order(bar.mfw.ext$perc, decreasing = FALSE)])

ggplot(bar.mfw.ext, aes(reorder(type1, -perc), y=perc, fill=medium)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  ylab("Percentage of Total Scenes") +
  xlab("") +
  coord_flip() +
  scale_fill_brewer(palette="Pastel2")

#figure 8 - domestic v work

domvwork <- NULL
perc <- c(film.dom.perc*100, film.work.perc*100, tv.dom.perc*100, tv.work.perc*100)
medium <- c("film", "film", "tv", "tv")
type <- c("domestic", "workplace", "domestic", "workplace")
domvwork <- data.frame(medium, type, perc)


ggplot(domvwork, aes(type, perc, fill=medium)) + 
  geom_bar(stat="identity", position= position_dodge()) +
  ylab("Percentage of Total Scenes") + 
  xlab("") +
  coord_flip() +
  scale_fill_brewer(palette="Pastel2") 








