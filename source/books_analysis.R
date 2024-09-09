library(googlesheets4)
library(gargle)
library(scales)
library(lme4)
setwd("/Users/jannawilloughby/Google Drive/My Drive/loot/Bookclub/bookclub_ratings/output/")
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
pdf("medianscores.pdf", width=10, height=5)
par(mar = c(5, 5, 2, 5), xpd=T)
plot(-100,-100, xlim=c(0.25,(nrow(ratings)+0.25)), ylim=c(0.75,10), xlab="", ylab="", axes=F)
axis(side=1, at=seq(1,nrow(ratings),1), labels=F, pos=0.75)
text(x = 1:length(ratings$short), y = par("usr")[3] - 0.05, labels = ratings$short,xpd = NA,srt = 35,adj = 0.965,cex = .8)
segments(x0=0, x1=0.25+nrow(ratings), y0=0.75, y1=0.75)
axis(side=2, at=seq(1,10,1), labels=T)
text(x=-3, y=5.5, labels = "ratings", xpd = NA, cex = 1.2, srt = 90)
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
      points(jitter(b,0.5), r$rating, pch=21, col=alpha(colors[p],1), bg=alpha(colors[p],0.5), cex=1.5)
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
pdf("indvscores.pdf", width=6, height=5)
indvs = sort(unique(data$reader))
par(bg=NA)
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
pdf("indvscores_boxplot.pdf", width=5, height=5)
boxplot(rating~reader, data=data)
dev.off()

#plot some shit -- by reader but normalized/adjusted score
pdf("indvscores_adjusted.pdf", width=6, height=5)
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
#add adjusted rating data to summary
ratings$adj_rating = rep(NA, nrow(ratings))
for(b in 1:nrow(ratings)){
  temp = data[data$book==as.character(ratings$books[b]),,drop=F]
  ratings$adj_rating[b] = mean(temp$adjustrank, na.rm=T)
}

####compare ratings over time####
mod.meta = cbind(seq(1,nrow(meta),1), meta)
colnames(mod.meta) = c("booknum", colnames(mod.meta)[2:ncol(mod.meta)])
alldata = merge(data, mod.meta, by="book")
allratings = merge(x=ratings, y=mod.meta, by.x="books", by.y="book")
indvs = sort(unique(data$reader))

pdf("allscores_overtime.pdf", width=5, height=5)
plot(-100,-100, xlim=c(1,30), ylim=c(0,10), xlab="book number", ylab="mean book rating", axes=T)
#segments(x0=1,y0=10,x1=30,y1=10,col="grey50", lty=2)
for(i in 1:length(indvs)){
  t = allratings[allratings$reader==indvs[i],]
  points(t$booknum, t$mean, pch=21, col=colors[i], bg=alpha(colors[i], 0.5))
}
tmodel = lm(mean~booknum, data=allratings)
summary(tmodel)
newx = seq(min(allratings$booknum),max(allratings$booknum),by = 0.05)
conf_interval = predict(tmodel, newdata=data.frame(booknum=newx), interval="confidence", level = 0.95)
segments(x0=min(allratings$booknum),y0=(min(allratings$booknum)*tmodel$coefficients[2]+tmodel$coefficients[1]),x1=max(allratings$booknum),y1=(max(allratings$booknum)*tmodel$coefficients[2]+tmodel$coefficients[1]), lwd=1.5, col="black")
polygon(x=c(newx,rev(newx)),y=c(conf_interval[,2],rev(conf_interval[,3])),col=alpha("black",0.1),border=F)
dev.off()

pdf("pickscores_overtime.pdf", width=5, height=5)
plot(-100,-100, xlim=c(1,30), ylim=c(-1,13), xlab="book number", ylab="mean book rating", axes=T)
segments(x0=1,y0=10,x1=30,y1=10,col="grey50", lty=2)
for(i in 1:length(indvs)){
  t = allratings[allratings$reader==indvs[i],]
  points(t$booknum, t$mean, pch=21, col=colors[i], bg=alpha(colors[i], 0.5))
  tmodel = lm(mean~booknum, data=t)
  summary(tmodel)
  newx = seq(min(t$booknum),max(t$booknum),by = 0.05)
  conf_interval = predict(tmodel, newdata=data.frame(booknum=newx), interval="confidence", level = 0.95)
  segments(x0=min(t$booknum),y0=(min(t$booknum)*tmodel$coefficients[2]+tmodel$coefficients[1]),x1=max(t$booknum),y1=(max(t$booknum)*tmodel$coefficients[2]+tmodel$coefficients[1]), lwd=1.5, col=colors[i])
  #lines(newx, conf_interval[,2], col=alpha(colors[i],0.3), lty=2)
  #lines(newx, conf_interval[,3], col=alpha(colors[i],0.3), lty=2)
  polygon(x=c(newx,rev(newx)),y=c(conf_interval[,2],rev(conf_interval[,3])),col=alpha(colors[i],0.1),border=F)
}
dev.off()

#lazy output examination
sink("pickscores_overtime_lm.txt")
i=1 #Aleks
print("Aleks")
t = allratings[allratings$reader==indvs[i],]
tmodel = lm(mean~booknum, data=t)
summary(tmodel)
i=2 #Bob
print("Bob")
t = allratings[allratings$reader==indvs[i],]
tmodel = lm(mean~booknum, data=t)
summary(tmodel)
i=3 #Devin
print("Devin")
t = allratings[allratings$reader==indvs[i],]
tmodel = lm(mean~booknum, data=t)
summary(tmodel)
i=4 #Janna
print("Janna")
t = allratings[allratings$reader==indvs[i],]
tmodel = lm(mean~booknum, data=t)
summary(tmodel)
i=5 #Matt
print("Matt")
t = allratings[allratings$reader==indvs[i],]
tmodel = lm(mean~booknum, data=t)
summary(tmodel)
sink()

pdf("allscores_overtime_adj.pdf", width=5, height=5)
plot(-100,-100, xlim=c(1,30), ylim=c(0,10), xlab="book number", ylab="mean book rating", axes=T)
for(i in 1:length(indvs)){
  t = allratings[allratings$reader==indvs[i],]
  points(t$booknum, t$adj_rating, pch=21, col=colors[i], bg=alpha(colors[i], 0.5))
}
tmodel = lm(adj_rating~booknum, data=allratings)
summary(tmodel)
newx = seq(min(allratings$booknum),max(allratings$booknum),by = 0.05)
conf_interval = predict(tmodel, newdata=data.frame(booknum=newx), interval="confidence", level = 0.95)
segments(x0=min(allratings$booknum),y0=(min(allratings$booknum)*tmodel$coefficients[2]+tmodel$coefficients[1]),x1=max(allratings$booknum),y1=(max(allratings$booknum)*tmodel$coefficients[2]+tmodel$coefficients[1]), lwd=1.5, col="black")
polygon(x=c(newx,rev(newx)),y=c(conf_interval[,2],rev(conf_interval[,3])),col=alpha("black",0.1),border=F)
dev.off()

pdf("pickscores_overtime_adj_.pdf", width=5, height=5)
plot(-100,-100, xlim=c(1,30), ylim=c(-1,13), xlab="book number", ylab="adjusted mean book rating", axes=T)
segments(x0=1,y0=10,x1=30,y1=10,col="grey50", lty=2)
for(i in 1:length(indvs)){
  t = allratings[allratings$reader==indvs[i],]
  points(t$booknum, t$adj_rating, pch=21, col=colors[i], bg=alpha(colors[i], 0.5))
  tmodel = lm(adj_rating~booknum, data=t)
  summary(tmodel)
  newx = seq(min(t$booknum),max(t$booknum),by = 0.05)
  conf_interval = predict(tmodel, newdata=data.frame(booknum=newx), interval="confidence", level = 0.95)
  segments(x0=min(t$booknum),y0=(min(t$booknum)*tmodel$coefficients[2]+tmodel$coefficients[1]),x1=max(t$booknum),y1=(max(t$booknum)*tmodel$coefficients[2]+tmodel$coefficients[1]), lwd=1.5, col=colors[i])
  #lines(newx, conf_interval[,2], col=alpha(colors[i],0.3), lty=2)
  #lines(newx, conf_interval[,3], col=alpha(colors[i],0.3), lty=2)
  polygon(x=c(newx,rev(newx)),y=c(conf_interval[,2],rev(conf_interval[,3])),col=alpha(colors[i],0.1),border=F)
}
dev.off()

#lazy output examination
sink("pickscores_overtime_adj_lm.txt")
i=1 #Aleks
print("Aleks")
t = allratings[allratings$reader==indvs[i],]
tmodel = lm(adj_rating~booknum, data=t)
summary(tmodel)
i=2 #Bob
print("Bob")
t = allratings[allratings$reader==indvs[i],]
tmodel = lm(adj_rating~booknum, data=t)
summary(tmodel)
i=3 #Devin
print("Devin")
t = allratings[allratings$reader==indvs[i],]
tmodel = lm(adj_rating~booknum, data=t)
summary(tmodel)
i=4 #Janna
print("Janna")
t = allratings[allratings$reader==indvs[i],]
tmodel = lm(adj_rating~booknum, data=t)
summary(tmodel)
i=5 #Matt
print("Matt")
t = allratings[allratings$reader==indvs[i],]
tmodel = lm(adj_rating~booknum, data=t)
summary(tmodel)
sink()

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
pdf("pickscores.pdf", width=5, height=5)
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


