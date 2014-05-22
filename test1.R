ranking <- read.csv("ranking.csv", head = T)
library(calibrate)
general <- function(input, value, greater = T){
  find.col <- which(names(ranking) == input)
  this.data <- ranking[, find.col]
  if (greater == T){
    index <- which( this.data >= value)
  }
  else{
    index <- which( this.data < value)
  }
  ranking[index, c(1:4, find.col)]
}
find.NA <- function(input){
  find.col <- which(names(ranking) == input)
  this.data <- ranking[, find.col]
  index <- is.na(this.data)
  ranking[index, c(1:4, find.col)]
}
get.table <- function(input, scale.factor){
  testfile <- paste(input, ".txt", sep = "") 
  write.table(x =  ranking.table(input, scale.factor), file = testfile, row.names = FALSE)
}
general <- function(input, value, greater = T){
  find.col <- which(names(ranking) == input)
  this.data <- ranking[, find.col]
  if (greater == T){
    index <- which( this.data >= value)
  }
  else{
    index <- which( this.data < value)
  }
  ranking[index, c(1:4, find.col)]
}
find.NA <- function(input){
  find.col <- which(names(ranking) == input)
  this.data <- ranking[, find.col]
  index <- is.na(this.data)
  ranking[index, c(1:4, find.col)]
}
ranking.table <- function(input, scale.factor){
  input <- as.character(input)
  indicator <- c("Top100", "101to200", "201to300", "301to400", "401to500", "501to")
  # ranking <- read.csv("ranking.csv", head = T)
  N <- rep(0, 7)
  Mean <- rep(0, 7)
  SD <- rep(0, 7)
  Min <- rep(0, 7)
  Max <- rep(0, 7)
  find.col <- which(names(ranking) == input)
  for (i in 1:6){
    index <- which(ranking$G1RR == indicator[i])
    step.data <- ranking[index, find.col]/scale.factor
    N[i] <- sum(complete.cases(step.data))
    Mean[i] <- mean(step.data, na.rm = T)
    SD[i] <- sd(step.data, na.rm = T)
    Min[i] <- min(step.data, na.rm = T)
    Max[i] <- max(step.data, na.rm = T)
  }
  total.data <- ranking[ , find.col]/scale.factor
  N[7] <- sum(complete.cases(total.data))
  Mean[7] <- mean(total.data, na.rm = T)
  SD[7] <- sd(total.data, na.rm = T)
  Min[7] <- min(total.data, na.rm = T)
  Max[7] <- max(total.data, na.rm = T)
  Ranking.range <- c("Top100", "101-200", "201-300", "301-400", "401-500", "500+", "Total")
  if (sum( Mean >=1 ) == 7 ){
    Mean <- round(Mean)
    SD <- round(SD)
    Min <- round(Min)
    Max <- round(Max)
  }
  else{
    Mean <- round(Mean, digits = 2)
    SD <- round(SD, digits = 2)
    Min <- round(Min, digits = 2)
    Max <- round(Max, digits = 2)
  }
  data.frame(Ranking.range, N, Mean, SD, Min, Max)
}
### first plot is trial, and the last is final
#### hist now finish
hist.plot1 <- function(input, scale.factor, bin){
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
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray90")
  par(new=T)
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
       col = "gold", yaxt="n", cex.axis = 0.7, cex.lab = 0.8)
  text(x =  (H$mids), y = H$density + 0.05, labels= labs, cex= 0.8, col = "blue")
  x <-  seq(0, 0.8, length = 9)
  axis(2, x, sprintf("%0.0f%%", x*100), 
       las = 1, cex.axis = 0.7, font = 2)
  box(lty = "solid")
}
hist.plot2 <- function(input, scale.factor, bin, threshold, greater = T){
  find.col <- which(names(ranking) == input)
  total.data <- ranking[ , find.col]/scale.factor
  if (greater == T){
    index <- which( total.data >= threshold)
  }
  else{
    index <- which( total.data <= threshold)
  }
  total.data[index] <- NA
  H <- hist(total.data, plot = F, breaks = bin)
  H$density <- with(H, density* diff(breaks)[1])
  H
  labs <- paste(round(H$counts), sep="")
  labs[labs == "0"] <- ""
  plot.new()
  # this step is important
  rect(par("usr")[1],par("usr")[3]+0.04,par("usr")[2],par("usr")[4],col = "gray90")
  # rect(par("usr")[1],par("usr")[3] , par("usr")[2], par("usr")[4], col = "gray90")
  par(new=T)
  plot(H, freq = FALSE, ylim=c(0, 0.8), xlab = "", main = "", ylab = "Percentage", 
     col = "gold", axes = F, cex.axis = 0.7, cex.lab = 0.8)
  text(x =  (H$mids), y = H$density + 0.05, labels= labs, cex= 0.8, col = "blue")
  x <-  seq(0, 0.8, length = 9)
  axis(2, x, sprintf("%0.0f%%", x*100), las = 1, cex.axis = 0.7, font = 2)
  axis(1, H$breaks, las = 1, cex.axis = 0.7, pos = 0, font = 2)
  # rect(par("usr")[1],par("usr")[3]+0.03,par("usr")[2],par("usr")[4], lwd = 0.5)
}
hist.plot3 <- function(input, scale.factor, bin, threshold, greater = T, Xmax){
  find.col <- which(names(ranking) == input)
  total.data <- ranking[ , find.col]/scale.factor
  if (greater == T){
    index <- which( total.data >= threshold)
  }
  else{
    index <- which( total.data <= threshold)
  }
  total.data[index] <- NA
  H <- hist(total.data, plot = F, breaks = bin)
  H$density <- with(H, density* diff(breaks)[1])
  H
  labs <- paste(round(H$counts), sep="")
  labs[labs == "0"] <- ""
  plot.new()
  # this step is important
  rect(par("usr")[1],par("usr")[3]+0.04,par("usr")[2],par("usr")[4],col = "gray90")
  # rect(par("usr")[1],par("usr")[3] , par("usr")[2], par("usr")[4], col = "gray90")
  par(new=T)
  plot(H, freq = FALSE, ylim=c(0, Xmax), xlab = "", main = "", ylab = "Percentage", 
       col = "gold", axes = F, cex.axis = 0.7, cex.lab = 0.9)
  text(x =  (H$mids), y = H$density + 0.04, labels= labs, cex= 0.8, col = "blue")
  x <-  seq(0, Xmax, by = 0.1)
  axis(2, x, sprintf("%0.0f%%", x*100), las = 1, cex.axis = 0.7, font = 2)
  axis(1, H$breaks, las = 1, cex.axis = 0.7, pos = 0, font = 2)
  # rect(par("usr")[1],par("usr")[3]+0.03,par("usr")[2],par("usr")[4], lwd = 0.5)
}


### now confidence
confidence1 <- function(input, scale.factor){
  x <- 1:6
  this.data <- ranking.table(input, scale.factor)
  avg <- this.data$Mean[1:6]
  if (sum(avg) > 6){
    sdev <- this.data$SD[1:6]/sqrt(this.data$N[1:6])
  }
  else {
    sdev <- sqrt(avg*(1-avg)/this.data$N[1:6])
  }
  y_range <- range(c(avg-3*sdev, avg+3*sdev))
  # par(bg = "grey")
  plot.new()
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray90")
  par(new=T)
  plot(c(x, 7), c(avg, NA), ylim = y_range, pch=20, xlab="", 
       ylab="95% confidence interval of Mean", main="", cex = 0.8, 
       xaxt="n", col = "red", las = 1, cex.axis = 0.7, cex.lab = 0.8)
  textxy(x, avg, labs= as.character(avg), cex= 0.8)
  # text(x, y = NULL, labels= as.character(avg), cex= 0.8, offset = -0.2, col = "blue")
  Ranking.range <- c("Top 100", "101-200", "201-300", "301-400", "401-500", "500+", "")
  axis(side = 1, at = c(x, 7), labels = Ranking.range, cex.axis = 0.7, font = 2)
  t_value <- qt(0.975, df = this.data$N[1:6]-1)
  arrows(x, avg-t_value*sdev, 
         x, avg+t_value*sdev, length=0.05, angle=90, code=3, col = "blue")
}
confidence2 <- function(input, scale.factor, ylimit){
  x <- 1:6
  this.data <- ranking.table(input, scale.factor)
  avg <- this.data$Mean[1:6]
  if (sum(avg) > 6){
    sdev <- this.data$SD[1:6]/sqrt(this.data$N[1:6])
  }
  else {
    sdev <- sqrt(avg*(1-avg)/this.data$N[1:6])
  }
  # y_range <- range(c(avg-3*sdev, avg+3*sdev))
  # par(bg = "grey")
  plot.new()
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray90")
  par(new=T)
  plot(c(x, 7), c(avg, NA), ylim = ylimit, pch = 20, xlab="", 
       ylab="95% confidence interval of Mean", main="", cex = 0.8, 
       xaxt="n", col = "red", las = 1, cex.axis = 0.7, cex.lab = 0.9, font = 2)
  textxy(x, avg, labs= as.character(avg), cex= 0.8)
  # text(x, y = NULL, labels= as.character(avg), cex= 0.8, offset = -0.2, col = "blue")
  Ranking.range <- c("Top 100", "101-200", "201-300", "301-400", "401-500", "500+", "")
  axis(side = 1, at = c(x, 7), labels = Ranking.range, cex.axis = 0.7, font = 2)
  t_value <- qt(0.975, df = this.data$N[1:6]-1)
  arrows(x, avg-t_value*sdev, 
         x, avg+t_value*sdev, length=0.05, angle=90, code=3, col = "blue")
}
### now box
box.plot1 <- function(input, scale.factor){
  find.col <- which(names(ranking) == input)
  # this.data <- ranking[, find.col]
  # indicator <- c("Top100", "101to200", "201to300", "301to400", "401to500", "501to")
  index1 <- which(ranking$G1RR == "Top100")
  data1 <- ranking[index1, find.col]/scale.factor
  index2 <- which(ranking$G1RR == "101to200")
  data2 <- ranking[index2, find.col]/scale.factor
  index3 <- which(ranking$G1RR == "201to300")
  data3 <- ranking[index3, find.col]/scale.factor
  index4 <- which(ranking$G1RR == "301to400")
  data4 <- ranking[index4, find.col]/scale.factor
  index5 <- which(ranking$G1RR == "401to500")
  data5 <- ranking[index5, find.col]/scale.factor
  index6 <- which(ranking$G1RR == "501to")
  data6 <- ranking[index6, find.col]/scale.factor
  plot.new()
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray90")
  par(new=T)
  boxplot(data6, data5, data4, data3, data2, data1, outline = F, 
          horizontal = T, yaxt="n", col = "blue", cex.axis = 0.7, font = 2, cex.lab = 2)
  Ranking.range <- rev(c("Top 100", "101-200", "201-300", "301-400", "401-500", "500+"))
  axis(side = 2, at = 1:6, labels = Ranking.range, las = 2, cex.axis = 0.7, font = 2)
}
box.plot2 <- function(input, scale.factor, x_range){
  find.col <- which(names(ranking) == input)
  # this.data <- ranking[, find.col]
  # indicator <- c("Top100", "101to200", "201to300", "301to400", "401to500", "501to")
  index1 <- which(ranking$G1RR == "Top100")
  data1 <- ranking[index1, find.col]/scale.factor
  index2 <- which(ranking$G1RR == "101to200")
  data2 <- ranking[index2, find.col]/scale.factor
  index3 <- which(ranking$G1RR == "201to300")
  data3 <- ranking[index3, find.col]/scale.factor
  index4 <- which(ranking$G1RR == "301to400")
  data4 <- ranking[index4, find.col]/scale.factor
  index5 <- which(ranking$G1RR == "401to500")
  data5 <- ranking[index5, find.col]/scale.factor
  index6 <- which(ranking$G1RR == "501to")
  data6 <- ranking[index6, find.col]/scale.factor
  plot.new()
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray90")
  par(new=T)
  boxplot(data6, data5, data4, data3, data2, data1, outline = F, 
          horizontal = T, yaxt="n", xaxt = "n", 
          col = "blue", cex.axis = 0.7, font = 2, cex.lab = 2)
  Ranking.range <- rev(c("Top 100", "101-200", "201-300", "301-400", "401-500", "500+"))
  axis(side = 2, at = 1:6, labels = Ranking.range, las = 2, cex.axis = 0.7, font = 2)
  axis(side = 1, at = x_range, labels = as.character(x_range), 
       las = 1, cex.axis = 0.7, font = 2)
}
###
test1 <- function(input, a, b){
  find.col <- which(names(ranking) == input)
  # this.data <- ranking[, find.col]
  # indicator <- c("Top100", "101to200", "201to300", "301to400", "401to500", "501to")
  index1 <- which(ranking$G1RR == a)
  index2 <- which(ranking$G1RR == b)
  data1 <- ranking[index1, find.col]
  data2 <- ranking[index2, find.col]
  n1 <- sum(complete.cases(data1))
  n2 <- sum(complete.cases(data2))
  if (sum(data1, na.rm = T) < n1 && sum(data2, na.rm = T) < n2){
    p1 <- mean(data1, na.rm = T) #sd1 <-  sd(data1, na.rm = T)
    p2 <- mean(data2, na.rm = T) # sd2 <-  sd(data2, na.rm = T)
    p.star <- (p1*n1+p2*n2)/(n1+n2) 
    SE <- sqrt( (1-p.star)*p.star*(1/n1 + 1/n2) )
    z <- abs(p1-p2)/SE
    result <- 2*pnorm(z, lower.tail = F)
  }
  else{
    result <- t.test(data1, data2)
  }
  result
}
test2 <- function(input, a, b){
  find.col <- which(names(ranking) == input)
  # this.data <- ranking[, find.col]
  # indicator <- c("Top100", "101to200", "201to300", "301to400", "401to500", "501to")
  index1 <- which(ranking$G1RR == a)
  index2 <- which(ranking$G1RR == b)
  data1 <- ranking[index1, find.col]
  data2 <- ranking[index2, find.col]
  n1 <- sum(complete.cases(data1))
  n2 <- sum(complete.cases(data1))
  if (sum(data1, na.rm = T) < n1 && sum(data2, na.rm = T) < n2){
    result <- ks.test(data1, data2)
  }
  else{
    result <- t.test(data1, data2)
  }
  result
}


