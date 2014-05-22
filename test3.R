library(DMwR)
ranking <- read.csv("ranking.csv", head = T)
index1 <- with(ranking, grepl("",  G1RR))   ##  with(ranking, grepl("101to200",  G1RR))
index2 <- with(ranking, grepl("", G7CT))   ##  with(ranking, grepl("UKRussell", G7CT))
dat <- subset( ranking[index1 & index2,   ], 
               select = c(Code, Region, Country, Institution, G1RR, G7CT, R9IndResInc) )
dat <- dat[with(dat, order(R9IndResInc, na.last = F)), ]  
print(head(dat, 30))
print(tail(dat, 30))
index3 <- which(names(dat) == tail(names(dat), 1) )
# boxplot(dat[ , index3], plot = F)$out
d <- dat[complete.cases(dat), ]
outlier.scores <- lofactor(d[ ,index3], k=20)
ord <- order(outlier.scores, decreasing=T)[1:10]
print(d[ord, ])
testfile <- paste("R9IndResInc", ".csv", sep = "") 
write.csv(x =  dat, file = testfile, row.names = FALSE)






