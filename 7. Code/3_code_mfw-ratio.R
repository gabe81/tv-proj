### Most Frequent Word (MFW) Generator ---------------
## get most frequent word table for each scene, then re-combine that data across various labels. 
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