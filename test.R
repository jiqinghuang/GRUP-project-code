df = data.frame(date=c(rep(2008:2013, by=1)),
                value=c(303,407,538,696,881,1094))

barplot(df$value, main="TITLE", col="gray", ylab="People", xlab="Years")

plot(df, type = "n")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
plot(df)
points(df)

plot.new()
# polygon(c(-min(df[,1])^2,-min(df[,1])^2,max(df[,1])^2,max(df[,1])^2),c(-min(df[,2])^2,max(df[,2])^2,max(df[,2])^2,-min(df[,2])^2), col="grey")
par(new=T)
plot(df)
# three key steps to create special background
# plot.new()
# rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
# par(new=T)
plot(df)

# key step to make histogram y axis as percentage
# h <- hist(rnorm(100))
# plot(h, freq=FALSE, yaxt="n")
# axis(2, pretty(h$density), sprintf("%0.0f%%", pretty(h$density)*100))
# box("solid")

hist.plot <- function(input, bin, scale.factor){
  find.col <- which(names(ranking) == input)
  total.data <- ranking[ , find.col]/scale.factor
  # par(bg = 'grey')
  # Max <- max(total.data, na.rm = T)
  # Min <- min(total.data, na.rm = T)
  H <- hist(total.data, plot = F, breaks = bin)
  H$density <- with(H, density* diff(breaks)[1])
  labs <- paste(round(H$counts), sep="")
  labs[labs == "0"] <- ""
  plot.new()
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
  par(new=T)
  plot(H, freq = FALSE, ylim=c(0, 1), 
       xlab = "", main = "", ylab = "Percentage", 
       col = "gold", yaxt="n", cex.axis = 0.7, cex.lab = 0.8)
  text(x =  (H$mids), y = H$density + 0.05, labels= labs, cex= 0.8, col = "blue")
  x <-  seq(0, 1, length = 11)
  axis(2, x, sprintf("%0.0f%%", x*100), 
       las = 1, cex.axis = 0.7)
  box(lty = "solid")
}

rb <- boxplot(decrease ~ treatment, data = OrchardSprays, col = "gray")
title("Comparing boxplot()s and non-robust mean +/- SD")
mn.t <- tapply(OrchardSprays$decrease, OrchardSprays$treatment, mean)
sd.t <- tapply(OrchardSprays$decrease, OrchardSprays$treatment, sd)
xi <- 0.3 + seq(rb$n)
points(xi, mn.t, col = "red", pch = 18)
arrows(xi, mn.t - sd.t, xi, mn.t + sd.t,code = 3, col = "red", angle = 75, length = .1)




#### revised
find.col <- which(names(ranking) == input)
total.data <- ranking[ , find.col]/scale.factor
# par(bg = 'grey')
# Max <- max(total.data, na.rm = T)
# Min <- min(total.data, na.rm = T)
H <- hist(total.data, plot = F, breaks = bin)
H$density <- with(H, density* diff(breaks)[1])
labs <- paste(round(H$counts), sep="")
labs[labs == "0"] <- ""
plot.new()
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
par(new=T)
# rect(xleft = 0, ybottom = 0, xright = 11000, ytop = 0.8, density = NULL, angle = 45,
#     border = NULL, col = NA, lty = par("lty"), col = "gray")
# rect(xleft = 0, ybottom = 0, xright = 11000, ytop = 0.8, col = "gray")

# origial different scale plot
# plot(H, freq = FALSE, ylim=c(0, 1.08*max(H$density)), 
#     xlab = "", main = "", ylab = "Percentage", 
#     col = "gold", yaxt="n", cex.axis = 0.7, cex.lab = 0.8)
# text(x =  (H$mids), y = H$density + 0.02, labels= labs, cex= 0.8)
# axis(2, pretty(H$density), sprintf("%0.0f%%", pretty(H$density)*100), 
#     las = 1, cex.axis = 0.7)
# box(lty = "solid")
# new closed fixed scale plot
plot(H, freq = FALSE, ylim=c(0, 0.8), 
     xlab = "", main = "", ylab = "Percentage", 
     col = "gold", axes = F, cex.axis = 0.7, cex.lab = 0.8)
text(x =  (H$mids), y = H$density + 0.05, labels= labs, cex= 0.8, col = "blue")
x <-  seq(0, 0.8, length = 9)
axis(2, x, sprintf("%0.0f%%", x*100), 
     las = 1, cex.axis = 0.7)
axis(1, H$breaks, 
     las = 1, cex.axis = 0.7, pos = 0)
box(lty = "solid")
# rect(xleft = 0, ybottom = 0, xright = 11000, ytop = 0.8, density = NULL, angle = 45,
#        border = NULL, col = NA, lty = par("lty"))
# plot.new()
# rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
# par(new=T)

plot.new()
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
par(new=T)
plot(H, freq = FALSE, ylim=c(0, 0.8), 
     xlab = "", main = "", ylab = "Percentage", 
     col = "gold", axes = F, cex.axis = 0.7, cex.lab = 0.8)
text(x =  (H$mids), y = H$density + 0.05, labels= labs, cex= 0.8, col = "blue")
x <-  seq(0, 0.8, length = 9)
axis(2, x, sprintf("%0.0f%%", x*100), 
     las = 1, cex.axis = 0.7)
axis(1, H$breaks, 
     las = 1, cex.axis = 0.7, pos = 0)
box(lty = "solid")
# rect(xleft = 0, ybottom = 0, xright = 11000, ytop = 0.8, density = NULL, angle = 45,
#     border = NULL, col = NA, lty = par("lty"))


xycompare <- function(xbase, ybase, xygroup){
  x.col <- which(names(ranking) == xbase)
  y.col <- which(names(ranking) == ybase)
  # indicator <- c("Top100", "101to200", "201to300", "301to400", "401to500", "501to", ...)
  index <- which(ranking$G1RR == xygroup)
  xdata <- ranking[index, x.col]
  ydata <- ranking[index, y.col]
  #plot(xdata, ydata, axes = F, xlab = "", ylab = "")
}

information <- function(){
  ranking[ , c(1:4, 81, 86, 87)]
  # use regular expression to search that
}

### try examples
names(ranking)
subset(ranking, grepl("Top100", G1RR), select = c(Code, Institution, R.sum, G1RR))
subset(ranking, select = c(Code, Institution, R.sum, G1RR))
a <- subset(ranking, grepl("USAAU" , G7CT), select = c(Code, Institution, G1RR, G7CT, T.sum))
a[with(a, order(T.sum, na.last = F)), ]    



index1 <- with(ranking, grepl("",  G1RR))   ##  with(ranking, grepl("101to200",  G1RR))
index2 <- with(ranking, grepl("", G7CT))   ##  with(ranking, grepl("UKRussell", G7CT))
dat <- subset( ranking[index1 & index2,   ], select = c(Code, Institution, G1RR, G7CT, T.sum) )
dat <- dat[with(dat, order(T.sum, na.last = F)), ]  
dat
index3 <- which(names(dat) == tail(names(dat), 1) )
boxplot(dat[ , index3], plot = F)$out

get.csv <- function(input){
  testfile <- paste(input, ".csv", sep = "") 
  write.csv(x =  dat, file = testfile, row.names = FALSE)
}

