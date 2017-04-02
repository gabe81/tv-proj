## TIME Ratio Generator
## Same as int/ext loop, but now aware of temporal categories
## loop to calculate all int/ext/weighted ratios for all shows
library(dplyr)

setwd("~/Dropbox/1_Research/tv-final/3. Scenes Per Script/TV_Scenes/")
files <- list.files(path="~/Dropbox/1_Research/tv-final/3. Scenes Per Script/TV_Scenes")
time <- data.frame()
weight.ratios <- data.frame()

domdict <- read.csv("~/Dropbox/1_Research/tv-final/5. Plot Tables/INT-EXT Labels by Space Type/TV_MFW_INT.csv", header = F)
domestic <- domdict[which(domdict$V5=='domestic'),]
dommatch <- domestic$V2

workplace <- domdict[which(domdict$V5=='workplace'),]
workmatch <- workplace$V2

avg.int = 0
avg.ext = 0
i = 1
for (i in 1:length(files)){
  print(files[i])
  space<-as.matrix(read.csv(files[i], header=F))
  time[i,1]<- gsub("_Scenes.csv", "", files[i])
  len.space = NROW(space)
  
  first.scene = ifelse(grepl("INT", space[1, 2]), 1, 0)
  
  first.dom = ifelse(grepl(paste(dommatch,collapse="|"), space[1, 2]), 1, 0)
  first.work = ifelse(grepl(paste(workmatch,collapse="|"), space[1, 2]), 1, 0)
  
  last.scene = ifelse(grepl("INT", space[len.space, 2]), 1, 0)
  
  last.dom = ifelse(grepl(paste(dommatch,collapse="|"), space[len.space, 2]), 1, 0)
  last.work = ifelse(grepl(paste(workmatch,collapse="|"), space[len.space, 2]), 1, 0)
  
  print(paste("first.scene:", first.dom, first.work))
  print(paste("last.scene:", last.dom, last.work))
  
  space.split = split(space, rep(1:4, each = ceiling(nrow(space)/4)))
  space.one <- data.frame(space.split[1])
  
  space.two <- data.frame(space.split[2])
  space.three <- data.frame(space.split[3])
  space.four <- data.frame(space.split[4])
  
  search.int.one = length(grep("INT", space.one$X1, value=TRUE))
  search.ext.one = length(grep("EXT", space.one$X1, value=TRUE))
  one.value = (search.int.one)/(search.int.one + search.ext.one)
  
  search.dom.one = length(grep(paste(dommatch,collapse="|"), space.one$X1, value=TRUE))
  search.work.one = length(grep(paste(workmatch,collapse="|"), space.one$X1, value=TRUE))
  one.value.dom = (search.dom.one)/(search.dom.one + search.work.one)
  one.value.work = (search.work.one)/(search.dom.one + search.work.one)
  print(paste("dom:", round(one.value.dom, digits=3), "work:", round(one.value.work, digits=3)))
  
  search.int.two = length(grep("INT", space.two$X2, value=TRUE))
  search.ext.two = length(grep("EXT", space.two$X2, value=TRUE))
  two.value = (search.int.two)/(search.int.two + search.ext.two)
  #print(paste("two.value:", two.value))
  
  search.dom.two = length(grep(paste(dommatch,collapse="|"), space.two$X2, value=TRUE))
  search.work.two = length(grep(paste(workmatch,collapse="|"), space.two$X2, value=TRUE))
  two.value.dom = (search.dom.two)/(search.dom.two + search.work.two)
  two.value.work = (search.work.two)/(search.dom.two + search.work.two)
  print(paste("dom:", round(two.value.dom, digits=3), "work:", round(two.value.work, digits=3)))
  
  search.int.three = length(grep("INT", space.three$X3, value=TRUE))
  search.ext.three = length(grep("EXT", space.three$X3, value=TRUE))
  three.value = (search.int.three)/(search.int.three + search.ext.three)
  #print(paste("three.value:", three.value))
  
  search.dom.three = length(grep(paste(dommatch,collapse="|"), space.three$X3, value=TRUE))
  search.work.three = length(grep(paste(workmatch,collapse="|"), space.three$X3, value=TRUE))
  three.value.dom = (search.dom.three)/(search.dom.three + search.work.three)
  three.value.work = (search.work.three)/(search.dom.three + search.work.three)
  print(paste("dom:", round(three.value.dom, digits=3), "work:", round(three.value.work, digits=3)))
  
  search.int.four = length(grep("INT", space.four$X4, value=TRUE))
  search.ext.four = length(grep("EXT", space.four$X4, value=TRUE))
  four.value = (search.int.four)/(search.int.four + search.ext.four)
  #print(paste("four.value:", four.value))
  
  search.dom.four = length(grep(paste(dommatch,collapse="|"), space.four$X4, value=TRUE))
  search.work.four = length(grep(paste(workmatch,collapse="|"), space.four$X4, value=TRUE))
  four.value.dom = (search.dom.four)/(search.dom.four + search.work.four)
  four.value.work = (search.work.four)/(search.dom.four + search.work.four)
  print(paste("dom:", round(four.value.dom, digits=3), "work:", round(four.value.work, digits=3)))
  
  time[i, 2] <- round(as.numeric(first.scene), digits= 2)
  time[i, 3] <- round(as.numeric(one.value), digits = 2)
  time[i, 4] <- round(as.numeric(two.value), digits = 2)
  time[i, 5] <- round(as.numeric(three.value), digits = 2)
  time[i, 6] <- round(as.numeric(four.value), digits = 2)
  time[i, 7] <- round(as.numeric(last.scene), digits = 2)
  time[i, 8] <- round(as.numeric(first.dom), digits= 2)
  time[i, 9] <- round(as.numeric(one.value.dom), digits = 2)
  time[i, 10] <- round(as.numeric(two.value.dom), digits = 2)
  time[i, 11] <- round(as.numeric(three.value.dom), digits = 2)
  time[i, 12] <- round(as.numeric(four.value.dom), digits = 2)
  time[i, 13] <- round(as.numeric(last.dom), digits = 2)
  time[i, 14] <- round(as.numeric(first.work), digits= 2)
  time[i, 15] <- round(as.numeric(one.value.work), digits = 2)
  time[i, 16] <- round(as.numeric(two.value.work), digits = 2)
  time[i, 17] <- round(as.numeric(three.value.work), digits = 2)
  time[i, 18] <- round(as.numeric(four.value.work), digits = 2)
  time[i, 19] <- round(as.numeric(last.work), digits = 2)
}

## attach corresponding ratios to master table
setwd("~/Dropbox/1_Research/tv-final/2. Metadata")
colnames(time) <- c("title", "int.0.0", "int.0.0~0.25", "int.0.25~0.50", "int.0.50~0.75", "int.0.75~1.0", "int.1.0", "dom.0.0", "dom.0.0~0.25", "dom.0.25~0.50", "dom.0.50~0.75", "dom.0.75~1.0", "dom.1.0", "work.0.0", "work.0.0~0.25", "work.0.25~0.50", "work.0.50~0.75", "work.0.75~1.0", "work.1.0")
master.label <- read.csv("Film_Scripts_Master.csv")
master.table <- master.label
master.table <- apply(master.table, 2, function(y) gsub(".txt", "", y))
master.table <- merge(x=master.table, y=time, by.x="title")
write.table(master.table, file = "Film_Time_Master.csv", sep=",")
