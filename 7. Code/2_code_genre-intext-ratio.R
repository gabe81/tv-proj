### INT/EXT Ratio filtering by genre --------
#subset averages by comedy/drama/fantasy/sci-fi/crime/action/horror
#allows one to plot these averages and see genre breakdowns

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
