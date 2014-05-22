ranking <- read.csv("ranking.csv", head = T)
# indicator <- c("T.sum", )
### reproduce table ranking range
ranking.range <- c("Top100", "101to200", "201to300", "301to400", "401to500", "501to")
num <- c()
for (i in ranking.range){
  num <- c(num, sum(ranking$G1RR == i))
}
Ranking.range <- c("Top100", "101-200", "201-300", "301-400", "401-500", "500+")
response.rate <- paste(round(num, 2), "%", sep="")
# data.frame(Ranking.range, NumberofRespondents = num, response.rate)
write.table(x =  data.frame(Ranking.range, NumberofRespondents = num, response.rate)
            , file = "summary1.txt", row.names = FALSE)





### create table for countries
dat <- subset(ranking, select = c(Code, Country, Institution, G1RR) )
Name <- names(table(dat$Country))
N <- c()
above100 <- c()
above500 <- c()
for (j in Name){
  a <- sum(dat$Country == j)
  N <- c(N, a)
  index <- which(dat$Country == j)
  step.dat <- dat[index, ]
  index500 <- sum(step.dat$G1RR == "501to")
  above500 <- c(above500, a-index500)
  index100 <- sum(step.dat$G1RR == "Top100")
  above100 <- c(above100, index100)
}
percentage1 = above500/N; percentage11 <- paste(round(percentage1*100, 2), "%", sep="")
percentage2 = above100/N; percentage22 <- paste(round(percentage2*100, 2), "%", sep="")
dat <- data.frame(Name, N, above500, above100, percentage11, percentage22)
dat <- dat[with(dat, order(above500, na.last = F, decreasing = T)), ] 
write.table(x =  dat, file = "summary2.txt", row.names = FALSE)













