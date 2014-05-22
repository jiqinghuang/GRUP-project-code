ranking <- read.csv("ranking.csv", head = T)
index1 <- with(ranking, grepl("",  G1RR))   ##  with(ranking, grepl("101to200",  G1RR))
index2 <- with(ranking, grepl("ItalyBest10", G7CT))          ##  with(ranking, grepl("UKRussell", G7CT))
# index3 <- with(ranking, grepl("Univ Bristol", Institution))

## [1]  "FranceBest10"  "CanadaG10"     "AustraliaGo8"  "ChinaC9"       "GermanyEliteU"
## [7] "ItalyBest10"   "JapanN7TIT"    "UKRussell"     "USAAU"  
##

dat.inf <- subset( ranking[index1 & index2,   ], 
                   select = c(Code, Region, Country, Institution, G1RR, G7CT) )

dat.inf1 <- subset(dat.inf, select = c(Institution, Country, G1RR, G7CT) )
dat.inf1 <- as.data.frame(t(dat.inf1))
write.csv(dat.inf1, file = "use.csv")











