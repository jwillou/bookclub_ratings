library(googlesheets4)
library(gargle)
setwd("/Users/jannawilloughby/GDrive/loot/Bookclub/")

#requires tidyverse install and google sheets integration api, or public sheet
gs4_deauth()
data = read_sheet("https://docs.google.com/spreadsheets/d/1oBncOH7I_-keah3mvgpzcTlXhP95vR2SRa6gBlWSd2Y/edit?resourcekey#gid=216442974")
data = as.data.frame(data)
colnames(data) = c("date", "book", "rating", "reader")

books = sort(unique(data$book))
readers = unique(data$reader)

#remove duplicated ratings 
newlist = NULL
for(i in 1:length(readers)){
  temp    = data[data$reader==readers[i],,drop=F]
  revbook = unique(temp$book)
  for(tt in 1:length(revbook)){
    x = temp[temp$book==as.character(revbook[tt]),,drop=F]
    if(nrow(x)==1){
      trow = x
    }else{
      trow = x[x$date==max(x$date),,drop=F]
    }
    newlist = rbind(newlist, trow)   
  }
}
data = as.data.frame(newlist)

#calculate median rating for each book
ratings = data.frame(books = books, median = rep(NA, length(books)), mean = rep(NA, length(books)), min = rep(NA, length(books)), max = rep(NA, length(books)))
for(b in 1:length(books)){
  temp = data[data$book==as.character(books[b]),,drop=F]
  ratings$median[b] = median(temp$rating)
  ratings$mean[b]   = mean(temp$rating)
  ratings$min[b]    = min(temp$rating)
  ratings$max[b]    = max(temp$rating)
}
ratings$seq=seq(1,nrow(ratings),1)

pdf("medianscores.pdf", width=5, height=5)
plot(-100,-100, xlim=c(1,nrow(ratings)), ylim=c(1,10), xlab="book", ylab="score")
colors=c("Dodgerblue3")


dev.off()

