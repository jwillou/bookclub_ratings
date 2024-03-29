library(googlesheets4)
library(gargle)
library(scales)
library(lme4)
#setwd("/Users/jannawilloughby/Google Drive/My Drive/loot/Bookclub/bookclub_ratings/output/")
setwd("/Users/jrw0107/Google Drive/My Drive/loot/Bookclub/bookclub_ratings/output/")

####setup####
#requires tidyverse install and google sheets integration api, or public sheet
gs4_deauth()
data = read_sheet("https://docs.google.com/spreadsheets/d/1oBncOH7I_-keah3mvgpzcTlXhP95vR2SRa6gBlWSd2Y/edit?resourcekey#gid=216442974")
data = as.data.frame(data)
colnames(data) = c("date", "book", "rating", "reader")
meta = read_sheet("https://docs.google.com/spreadsheets/d/1icRZXs63DR6EpW29qzdWFn87jUF0HkJeY-ABaxIX9tQ/edit#gid=0")
meta = as.data.frame(meta)
colnames(meta) = c("book", "short", "reader", "month", "year", "author", "gender")

books = unique(data$book)
readers = sort(unique(data$reader))
colors=c("darkorange1", "firebrick3", "chartreuse3", "dodgerblue3","darkorchid3")

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
ratings = data.frame(books = books, short = rep(NA, length(books)), median = rep(NA, length(books)),mean = rep(NA, length(books)), min = rep(NA, length(books)), max = rep(NA, length(books)))
for(b in 1:length(books)){
  temp = data[data$book==as.character(books[b]),,drop=F]
  ratings$median[b] = median(temp$rating, na.rm=T)
  ratings$mean[b]   = mean(temp$rating, na.rm=T)
  ratings$min[b]    = min(temp$rating, na.rm=T)
  ratings$max[b]    = max(temp$rating, na.rm=T)
  ratings$short[b]  = meta$short[meta$book==books[b]]
}
ratings$seq=seq(1,nrow(ratings),1)

#####plot some shit -- by book####
jpeg("medianscores.jpg", width=680, height=480)
par(mar = c(5, 5, 2, 5), xpd=T)
plot(-100,-100, xlim=c(0.25,(nrow(ratings)+0.25)), ylim=c(0.75,10), xlab="", ylab="", axes=F)
axis(side=1, at=seq(1,nrow(ratings),1), labels=F, pos=0.75)
text(x = 1:length(ratings$short), y = par("usr")[3] - 0.05, labels = ratings$short,xpd = NA,srt = 35,adj = 0.965,cex = .8)
segments(x0=0, x1=0.25+nrow(ratings), y0=0.75, y1=0.75)
axis(side=2, at=seq(1,10,1), labels=T)
text(x=-2, y=5.5, labels = "ratings", xpd = NA, cex = 1.2, srt = 90)
p=1
for(b in 1:length(books)){
  if(p==1){
    polygon(x=c(b-.45, b+.45, b+.45, b-.45), y=c(0.95,0.95,10.1,10.1), col="grey95", border=NA)
    p=0
    next
  }
  if(p==0){
    p=1
  }
}
for(b in 1:length(books)){
  temp = data[data$book==as.character(books[b]),,drop=F]
  for(p in 1:length(readers)){
    r = temp[temp$reader==readers[p],,drop=F]
    if(nrow(r)==0){next}
    if(nrow(r)>0){
      points(jitter(b,0.75), r$rating, pch=21, col=alpha(colors[p],1), bg=alpha(colors[p],0.5), cex=1.5)
    }
  }
  segments(x0=(b-0.25), x1=(b+0.25), y0=ratings$median[b], y1=ratings$median[b], lwd=1.5)
}
legend(x=length(books)+1,y=4, legend=readers, col=alpha(colors,0.5), pch=19, bg=NA, pt.bg=alpha(colors,0.5), cex=0.8, bty="n")
dev.off()

#####are some of us consistently grumpier than others?####
sink("indvscores_lm.txt")
lm.out = lmer(rating~reader+(1|book)-1, data=data)
summary(lm.out)
summary(lm.out)$coeff[, 1]-(1.96*summary(lm.out)$coeff[ ,2])
summary(lm.out)$coeff[, 1]+(1.96*summary(lm.out)$coeff[ ,2])
sink()


#plot some shit -- by reader
jpeg("indvscores.jpg", width=480, height=480)
indvs = sort(unique(data$reader))
par(mar = c(5, 5, 1, 7), xpd=T)
plot(-100,-100, xlim=c(0.25,(length(indvs)+0.25)), ylim=c(0.75,10), xlab="", ylab="", axes=F)
axis(side=1, at=seq(1,length(indvs),1), labels=F, pos=0.75)
text(x = 1:length(indvs), y = par("usr")[3] - 0.5, labels = indvs, xpd = NA, cex = 1.2)
segments(x0=0, x1=0.25+length(indvs), y0=0.75, y1=0.75)
axis(side=2, at=seq(1,10,1), labels=T)
text(x=-0.7, y=5.5, labels = "ratings", xpd = NA, cex = 1.2, srt = 90)
p=1
for(b in 1:length(indvs)){
  if(p==1){
    polygon(x=c(b-.45, b+.45, b+.45, b-.45), y=c(0.95,0.95,10.1,10.1), col="grey95", border=NA)
    p=0
    next
  }
  if(p==0){
    p=1
  }
}
for(b in 1:length(indvs)){
  temp = data[data$reader==as.character(indvs[b]),,drop=F]
  if(nrow(temp)==0){next}
  else{
    temp$rating[is.na(temp$rating)] = -9
    points(jitter(rep(b, nrow(temp[!is.na(temp$rating),]) ), 4), temp$rating, pch=21, col=alpha(colors[b],1), bg=alpha(colors[b],0.5), cex=1.5)
  }
  segments(x0=(b-0.25), x1=(b+0.25), y0=median(temp$rating), y1=median(temp$rating), lwd=1.5)
}
dev.off()

#plot some shit -- by reader but normalized/adjusted score
jpeg("indvscores_adjusted.jpg", width=480, height=480)
indvs = sort(unique(data$reader))
par(mar = c(5, 5, 1, 7), xpd=T)
plot(-100,-100, xlim=c(0.25,(length(indvs)+0.25)), ylim=c(0.75,10), xlab="", ylab="", axes=F)
axis(side=1, at=seq(1,length(indvs),1), labels=F, pos=0.75)
text(x = 1:length(indvs), y = par("usr")[3] - 0.5, labels = indvs, xpd = NA, cex = 1.2)
segments(x0=0, x1=0.25+length(indvs), y0=0.75, y1=0.75)
axis(side=2, at=seq(1,10,1), labels=T)
text(x=-0.7, y=5.5, labels = "ratings", xpd = NA, cex = 1.2, srt = 90)
p=1
for(b in 1:length(indvs)){
  if(p==1){
    polygon(x=c(b-.45, b+.45, b+.45, b-.45), y=c(0.95,0.95,10.1,10.1), col="grey95", border=NA)
    p=0
    next
  }
  if(p==0){
    p=1
  }
}
#setup adjustments
data$adjustrank = rep(NA, nrow(data))
for(b in 1:length(indvs)){
  #calcualte adjusted scale per person (rescaling to 1-10)
  temp = data[data$reader==as.character(indvs[b]),,drop=F]
  mins = min(temp$rating, na.rm=T)
  maxs = max(temp$rating, na.rm=T)
  newscale = mm = 1
  for(m in 1:(maxs-mins)){
    newscale = c(newscale, (mm + (9/(maxs-mins))))
    mm = mm + (9/(maxs-mins))
  }
  newscale = data.frame(newscale = newscale, oldscale = seq(mins, maxs, 1))
  for(rr in 1:nrow(newscale)){
    data$adjustrank[data$reader==as.character(indvs[b]) & data$rating == as.numeric(newscale$oldscale[rr])] = as.numeric(newscale$newscale[rr])
  }
  temp = data[data$reader==as.character(indvs[b]),,drop=F]
  if(nrow(temp)==0){next}
  else{
    temp$adjustrank[is.na(temp$adjustrank)] = -9
    points(jitter(rep(b, nrow(temp[!is.na(temp$adjustrank),]) ), 4), temp$adjustrank, pch=21, col=alpha(colors[b],1), bg=alpha(colors[b],0.5), cex=1.5)
  }
  segments(x0=(b-0.25), x1=(b+0.25), y0=median(temp$adjustrank), y1=median(temp$adjustrank), lwd=1.5)
}
dev.off()

#####do biographical differences between authors relate to our differences in ratings?####
#TBD

#####are some of us  just shitty book-pickers?####
OUT = NULL
for(r in 1:nrow(data)){
  t = data[r,]
  tt = meta[meta$book==as.character(t$book),]
  t$author = tt$author
  t$gender = tt$gender
  t$picker = tt$reader
  OUT = rbind(OUT, t)
}
data = as.data.frame(OUT)

sink("pickscores_lm.txt")
lm.out = lmer(rating~picker+(1|book)-1, data=data)
summary(lm.out)
summary(lm.out)$coeff[, 1]-(1.96*summary(lm.out)$coeff[ ,2])
summary(lm.out)$coeff[, 1]+(1.96*summary(lm.out)$coeff[ ,2])
sink()

#plot some shit -- by reader
jpeg("pickscores.jpg", width=480, height=480)
indvs = sort(unique(data$picker))
par(mar = c(5, 5, 1, 7), xpd=T)
plot(-100,-100, xlim=c(0.25,(length(indvs)+0.25)), ylim=c(0.75,10), xlab="", ylab="", axes=F)
axis(side=1, at=seq(1,length(indvs),1), labels=F, pos=0.75)
text(x = 1:length(indvs), y = par("usr")[3] - 0.5, labels = indvs, xpd = NA, cex = 1.2)
segments(x0=0, x1=0.25+length(indvs), y0=0.75, y1=0.75)
axis(side=2, at=seq(1,10,1), labels=T)
text(x=-0.7, y=5.5, labels = "ratings", xpd = NA, cex = 1.2, srt = 90)
p=1
for(b in 1:length(indvs)){
  if(p==1){
    polygon(x=c(b-.45, b+.45, b+.45, b-.45), y=c(0.95,0.95,10.1,10.1), col="grey95", border=NA)
    p=0
    next
  }
  if(p==0){
    p=1
  }
}
for(b in 1:length(indvs)){
  temp = data[data$picker==as.character(indvs[b]),,drop=F]
  if(nrow(temp)==0){next}
  else{
    temp$rating[is.na(temp$rating)] = -9
    points(jitter(rep(b, nrow(temp[!is.na(temp$rating),]) ), 4), temp$rating, pch=21, col=alpha(colors[b],1), bg=alpha(colors[b],0.5), cex=1.5)
  }
  segments(x0=(b-0.25), x1=(b+0.25), y0=median(temp$rating), y1=median(temp$rating), lwd=1.5)
}
dev.off()


