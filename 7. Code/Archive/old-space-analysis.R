library(tm)
library(koRpus)
library(proxy)
library(ggplot2)
library(reshape2)
library(wordcloud)
library(sfsmisc)
library(DTK)

## INT/EXT Ratio loop, generate master.table. This code assumes you have the scraped data, which is not included in the article files --------------------
## loop to calculate all int/ext/weighted ratios for all shows
scenes <- data.frame()
weight <- data.frame()
weight.ratios <- data.frame()
avg.int = 0
avg.ext = 0
print(length(files))
for (i in 1:length(files)){
  space<-read.csv(files[i], header=F)
  #space.all<- rbind(space.all, space)
  scenes[i,1]<- gsub("_Scenes.csv", "", files[i])
  weight[i,1] <-gsub("_Scenes.csv", "", files[i])
  j = 0
  k = 0
  int.rows<-space[grep("INT", space$V2), ]
  int.weight <- mean(int.rows$V3)
  ext.rows<-space[grep("EXT", space$V2), ]
  ext.weight <- mean(ext.rows$V3)
  #weight.ratio <- int.weight/ext.weight
  #print(paste("weight.ratio:", weight.ratio))
  space.df <- data.frame(do.call('rbind', strsplit(as.character(space$V2),' ',fixed=TRUE)))
  space.v<-unlist(strsplit(as.character(space$V2), split=" "))
  space.table<-data.frame(table(space.v))
  space.table<-space.table[order(-space.table$Freq), ]
  int<-space.table[space.table[,1] == "INT", 2]
  ext<-space.table[space.table[,1] == "EXT", 2]
  if ((is.nan(int.weight) == TRUE)|(length(int.weight) == 0)){
    int.weight <- 1
  }
  if ((is.nan(ext.weight) == TRUE)|(length(ext.weight) == 0)){
    ext.weight <- 1
  }
  if (length(int.rows)==0){
    ext <- 1
  }
  if (length(ext.rows)==0){
    int <- 1
  }
  if (length(ext)==0){
    ext <- 1
  }
  if (length(int)==0){
    int <- 1
  }
  print(files[i])
  print(paste("int.rows: ", int.rows))
  print(paste("ext.rows: ", ext.rows))
  print(paste("int: ", int))
  print(paste("int.weight:", int.weight))
  print(paste("ext: ", ext))
  print(paste("ext.weight:", ext.weight))
  if (length(ext|int)>=0){
    int.ext<- int/ext
    int.perc <- int/(int+ext)
    #     if (length(int.weight)>300){
    #       int.weight <- as.numeric(1)
    #       ext.weight <- as.numeric(1)
    #     }
    #     if (length(ext.weight)>300){
    #       ext.weight <- as.numeric(1)
    #       int.weight <- as.numeric(1)
    #     }
    
    int.weight.ratio <- int.weight/(int.weight+ext.weight)
    if (int.weight.ratio >= 10){
      int.weight.ratio <- 1
    }
    int.ext.weighted <- (int/ext)*(int.weight.ratio)
    print(paste("int.weight.ratio:" , int.weight/ext.weight))
    #print(paste("ext.weight:" , ext.weight))
    weight[i,2] <- as.numeric(int.weight)
    weight[i,3] <- as.numeric(ext.weight)
    scenes[i,2] <- as.numeric(int/ext)
    scenes[i,3] <- as.numeric(int.ext.weighted)
    scenes[i,4] <- as.numeric(int.perc)
    scenes[i,5] <- as.numeric(int.weight.ratio)
    #print(files[i])
  }
}
#write.table(scenes, file = "../TV_Scripts_Scenes_Int_Ext_Ratio.csv", sep=",")
## attach corresponding ratios to master table
master.table <- master.label
master.table <- apply(master.table, 2, function(y) gsub(".txt", "", y))
master.table <- merge(x=master.table, y=scenes, by.x="title", by.y="V1")
#write.table(master.table, file = "/Users/Fedor/Desktop/tv-final/2. Metadata/Film_Scripts_Master_2.csv", sep=",")


### INT/EXT Ratio filtering by genre --------
#subset averages by comedy/drama/fantasy/sci-fi/crime/action/horror

master.table <- read.csv("/Users/Fedor/Desktop/tv-final/2. Metadata/TV_Scripts_Master.csv")
ratio.table <- data.frame()
colnames(ratio.table) <- c("int.ext.ratio", "int.ext.weighted", "total scripts")

comedy.filt <- master.table[master.table$genre == 'comedy',]
comedy.ratio <- mean(comedy.filt$int.ext.ratio)
comedy.ratio.w <- mean(comedy.filt$int.ext.weighted)
comedy.percentage <- mean(comedy.filt$int.percent)
total.comedy <- nrow(comedy.filt)
comedy.vector <- c("comedy", total.comedy, comedy.ratio, comedy.ratio.w, comedy.percentage)

drama.filt <- master.table[master.table$genre == 'drama',]
drama.ratio <- mean(drama.filt$int.ext.ratio)
drama.ratio.w <- mean(drama.filt$int.ext.weighted)
drama.percentage <- mean(drama.filt$int.percent)
total.drama <- nrow(drama.filt)
drama.vector <- c("drama", total.drama, drama.ratio, drama.ratio.w, drama.percentage)

fantasy.filt <- master.table[master.table$genre == 'fantasy',]
fantasy.ratio <- mean(fantasy.filt$int.ext.ratio)
fantasy.ratio.w <- mean(fantasy.filt$int.ext.weighted)
fantasy.percentage <- mean(fantasy.filt$int.percent)
total.fantasy <- nrow(fantasy.filt)
fantasy.vector <- c("fantasy", total.fantasy, fantasy.ratio, fantasy.ratio.w, fantasy.percentage)


scifi.filt <- master.table[master.table$genre == 'sci-fi',]
scifi.ratio <- mean(scifi.filt$int.ext.ratio)
scifi.ratio.w <- mean(scifi.filt$int.ext.weighted)
scifi.percentage <- mean(scifi.filt$int.percent)
total.scifi <- nrow(scifi.filt)
scifi.vector <- c("sci-fi", total.scifi, scifi.ratio, scifi.ratio.w, scifi.percentage)



crime.filt <- master.table[master.table$genre == 'crime',]
crime.ratio <- mean(crime.filt$int.ext.ratio)
crime.ratio.w <- mean(crime.filt$int.ext.weighted)
crime.percentage <- mean(crime.filt$int.percent)
total.crime <- nrow(crime.filt)
crime.vector <- c("crime", total.crime, crime.ratio, crime.ratio.w, crime.percentage)


action.filt <- master.table[master.table$genre == 'action',]
action.ratio <- mean(action.filt$int.ext.ratio)
action.ratio.w <- mean(action.filt$int.ext.weighted)
action.percentage <- mean(action.filt$int.percent)
total.action <- nrow(action.filt)
action.vector <- c("action", total.action, action.ratio, action.ratio.w, action.percentage)



horror.filt <- master.table[master.table$genre == 'horror',]
horror.ratio <- mean(horror.filt$int.ext.ratio)
horror.ratio.w <- mean(horror.filt$int.ext.weighted)
horror.percentage <- mean(horror.filt$int.percent)
total.horror <- nrow(horror.filt)
horror.vector <- c("horror", total.horror, horror.ratio, horror.ratio.w, horror.percentage)


ratio.table <- data.frame(comedy.vector, drama.vector, fantasy.vector, scifi.vector, crime.vector, action.vector, horror.vector)
write.table(ratio.table, file ="TV_Genre_Ratios.csv", sep=",")

### Figure 1. Data Overview


### INT/EXT Plotting ------
master.label <- read.csv("/Users/Fedor/Desktop/tv-final/2. Metadata/TV_Scripts_Master.csv")
ratios <- read.csv("~/Desktop/tv-final/5. Plot Tables/TV_Genre_Ratios.csv")

## box plots
ggplot(master.label, aes(x=genre, y=int.percent)) + geom_boxplot() + 
  ggtitle("Percentage of Script Lines Dedicated to Interior Scenes by Genre") + 
  xlab("genre") + ylab("Percent of Lines INT")
ggplot(master.label, aes(x=genre, y=int.ext.weighted)) + geom_boxplot() + ggtitle("Percentage of Interior Scenes Averaged by Genre") + xlab("genre") + ylab("interior percentage")

### MFW Generator ---------------
## get most frequent word table
## to add: get sums
t.int<-data.frame(matrix(ncol=1))
t.ext<-data.frame(matrix(ncol=1))
colnames(t.int)<-c("words")
colnames(t.ext)<-c("words")
## filter out shows by genre then run the loop
#filt <- master.label[master.label$genre == 'horror',]
#filt$title <- sub("$", "_Scenes.csv", filt$title)
#for (i in 1:length(filt$title)){
space.int.d <- data.frame()
space.ext.d <- data.frame()
for (i in 1:length(files)){
  #print(com.filt$title[i])
  #space<-read.csv("Community_1x01_-_Pilot_Scenes.csv", header=F)
  #setwd("~/Dropbox/Research/Me/TV-Scripts/R-Space/TV_Scripts_All_Scenes_3/")
  #space<-read.csv(file.path("~/Dropbox/Research/Me/TV-Scripts/R-Space/TV_Scripts_All_Scenes_3", filt$title[i]), stringsAsFactors=FALSE, header = FALSE)
  space <-read.csv(files[i], header = FALSE)
  space <- format(space)
  print(files[i])
  int.rows<-grep("INT", space$V2)
  if (length(int.rows)==0){
    next
  }
  ext.rows<-grep("EXT", space$V2)
  if (length(ext.rows)==0){
    next
  }
  #int
  space.int<-space[int.rows,]
  #space.int.d<- data.frame()
  space.int.d <- rbind(space.int.d, space[int.rows,])
  #int.all <- cbind(int.all$V2, space.int.d$V2)
  space.int.v<-unlist(strsplit(as.character(space.int$V2), split=" "))
  space.int.v<-space.int.v[space.int.v != ""]
  space.int.v<-space.int.v[space.int.v != "INT"]
  space.int.table<-data.frame(table(space.int.v))
  space.int.table<-space.int.table[order(-space.int.table$Freq), ]
  #ext
  space.ext<-space[ext.rows,]
  space.ext.d <- rbind(space.ext.d, space[ext.rows,])
  #ext.all <- cbind(ext.all$V2, space.ext$V2)
  space.ext.v<-unlist(strsplit(as.character(space.ext$V2), split=" "))
  space.ext.v<-space.ext.v[space.ext.v != ""]
  space.ext.v<-space.ext.v[space.ext.v != "EXT"]
  space.ext.table<-data.frame(table(space.ext.v))
  space.ext.table<-space.ext.table[order(-space.ext.table$Freq), ]
  #merge into master df
  colnames(space.int.table)<-c("words", files[i])
  t.int<-merge(t.int, space.int.table, by = "words", all=TRUE)
  colnames(space.ext.table)<-c("words", files[i])
  t.ext<-merge(t.ext, space.ext.table, by = "words", all=TRUE)
}

write.table(space.int.d, "../All_Int_Scenes.csv")
write.table(space.ext.d, "../All_Ext_Scenes.csv")

space.ext.d<- read.csv("All_Ext_Scenes.csv", header = TRUE, sep = ",")

t.int<-data.frame(matrix(ncol=1))
t.ext<-data.frame(matrix(ncol=1))
colnames(t.int)<-c("words")
colnames(t.ext)<-c("words")

int.homo <- data.frame()
#unlist and clean for all int scenes for 1000 scene sample and loop
for(i in 1:1000){
  int.samp <- space.int.d[sample(nrow(space.int.d), 1000), ]
  space.int.v<-unlist(strsplit(as.character(int.samp$V2), split=" "))
  space.int.v<-space.int.v[space.int.v != ""]
  space.int.v<-space.int.v[space.int.v != "INT"]
  space.int.table<-data.frame(table(space.int.v))
  space.int.table<-space.int.table[order(-space.int.table$Freq), ]
  hom.value = nrow(space.int.table)/sum(space.int.table$Freq)
  print(hom.value)
  int.homo <- rbind(hom.value, int.homo)
}

write.table(int.homo, "../TV_Int_Homogeneity")
tv.homogeneity = mean(int.homo$X0.295745175540572)
print(tv.homogeneity)

ext.homo <- data.frame()
#unlist and clean for all ext scenes for 1000 scene sample and loop
for(i in 1:1000){
  ext.samp <- space.ext.d[sample(nrow(space.ext.d), 1000), ]
  space.ext.v<-unlist(strsplit(as.character(ext.samp$V2), split=" "))
  space.ext.v<-space.ext.v[space.ext.v != ""]
  space.ext.v<-space.ext.v[space.ext.v != "ext"]
  space.ext.table<-data.frame(table(space.ext.v))
  space.ext.table<-space.ext.table[order(-space.ext.table$Freq), ]
  hom.value = nrow(space.ext.table)/sum(space.ext.table$Freq)
  print(hom.value)
  ext.homo <- rbind(hom.value, ext.homo)
}

write.table(ext.homo, "../TV_Ext_Homogeneity")
film.ext.homogeneity = mean(ext.homo$X0.257569721115538)
print(film.ext.homogeneity)

colnames(space.int.table)<-c("words", files[i])
t.int<-merge(t.int, space.int.table, by = "words", all=TRUE)
t.int[is.na(t.int)] <- 0
t.int<-t.int[-1,]
row.names(t.int)<-t.int[,1]
t.int<-t.int[,-1]
t.int.final<-t(t.int)
mfw.int <- data.frame(sort(colSums(t.int.final), decreasing = TRUE))
#do same for t.ext
t.ext[is.na(t.ext)] <- 0
t.ext<-t.ext[-1,]
row.names(t.ext)<-t.ext[,1]
t.ext<-t.ext[,-1]
t.ext.final<-t(t.ext)

# tabled 

mfw.ext <- data.frame(sort(colSums(t.ext.final), decreasing = TRUE))

write.table(mfw.int, "../MFW_Int_Unfiltered.csv", sep=",")
write.table(mfw.ext, "../MFW_Ext_Unfiltered.csv", sep=",")

write.table(t.int.final, "../TV_Scenes_Int_Horror_MFW.csv", sep=",")
write.table(t.ext.final, "../TV_Scenes_Ext_Horror_MFW.csv", sep=",")

### FIGURE 1, FIGURE 2 -- data overview -----------------

#count unique shows
master <- read.csv("TV_Scripts_Master.csv")
shows <- gsub("_\\S*","",master$title)
table <- as.matrix(table(shows))

## Figure 1

#histograms for media Distribution
tv <- as.data.frame(read.csv("TV_Scripts_Master.csv"))
film <- as.data.frame(read.csv("Film_Scripts_Master.csv"))

tv.post <- subset(tv, date>1989 & date<2016)
film.post <-subset(film, year>1989 & year<2016)

hist.tv <- hist(tv.post$date)
hist.tv2 <- hist(tv$date, breaks=seq(1990,2015,by=5), main="")
hist.tv2 <- hist(tv$date, breaks=60, main="TV Shows by Date", xlab = "year")

hist.film <- hist(film.post$year)
hist.film <- hist(film$year, breaks=seq(1990,2015,by=5), main="")
hist.film <- hist(film$year, breaks=60, main="Films by Date", xlab = "year")

ggplot() + 
  geom_histogram(data=film.post, aes(year, fill="Film", y= -..count..), binwidth = 1) +
  geom_histogram(data=tv.post, aes(date, fill="Television", y= ..count..), binwidth = 1) +
  ggtitle("Media Distribution by Date") + xlab("Year") + ylab("Number of Screenplays")


## Figure 2. Paired bar graphs for genre

#paired bar graphs
tv <- as.data.frame(read.csv("TV_Scripts_Master.csv"))
film <- as.data.frame(read.csv("Film_Scripts_Master.csv"))

tv.post <- subset(tv, date>1989 & date<2016)
film.post <-subset(film, year>1989 & year<2016)

grouped.tv <- as.data.frame(table(tv.post$genre))
grouped.film <- as.data.frame(table(film.post$genre))

genre.info <- read.csv("Genre_Both.csv")

ggplot(genre.info, aes(x = genre, y = Freq, fill=medium)) +
  geom_bar(stat='identity', position = position_dodge()) + ggtitle("Dataset Breakdown by Genre") + ylab("Number of Screenplays")


## Ratios by Genre

## Figure 4. Excerpt from Mad-Men file

## Figure 5. All Int-Ext labels by Scene type

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

short.tv <- tv.mfw.int[c(1:10),]
short.film <- film.mfw.int[c(1:10),]

short <- rbind(short.tv, short.film)

#stacked


#figure 5 -- int by building
ggplot(bar.mfw.int, aes(x = reorder(type1, -perc), y=perc, fill=medium)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  xlab("Type of Space") + 
  ylab("Percentage of Total Scenes") + 
  ggtitle("Types of Buildings by Medium")


#figure 6 -- domestic vs workplace int
ggplot(bar.mfw.int, aes(x = reorder(type2, -perc), y=perc, fill=medium)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  xlab("Type of Space") + 
  ylab("Percentage of Total Scenes") + 
  ggtitle("Domestic and Workplace Spaces by Medium")

#figure 6.5 --ext
ggplot(bar.mfw.ext, aes(x = reorder(type1, -perc), y=perc, fill=medium)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  xlab("Type of Space") + 
  ylab("Percentage of Total Scenes") + 
  ggtitle("Types of Exterior Spaces by Medium")

#figure 7 -- top 10 int
ggplot(short, aes(x = reorder(words, -perc), y=perc, fill=medium)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  xlab("Type of Space") + 
  ylab("Percentage of Total Scenes") + 
  ggtitle("Top 10 Most Occuring Words in Space Titles by Medium")



