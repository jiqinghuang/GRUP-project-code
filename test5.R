ranking <- read.csv("ranking.csv", head = T)
index1 <- with(ranking, grepl("Top100",  G1RR))   ##  with(ranking, grepl("101to200",  G1RR))
index2 <- with(ranking, grepl("", G7CT))          ##  with(ranking, grepl("UKRussell", G7CT))
index3 <- with(ranking, grepl("Univ Bristol", Institution))
#  ranking$Institution
file.name <- "Univ Bristol.csv"
# rank[, 4] gives all the names of universities
write.mytable <- function(){
  dat.inf <- subset( ranking[index1 & index2,   ], 
               select = c(Code, Region, Country, Institution, G1RR, G7CT) )
  
  dat <- subset( ranking[index1 & index2,   ], 
               select = c(Institution, T.sum, F4PerStaTeaDoc, F5PerIntStaTea, 
                          R.sum, S1NStu, S5PerGraStu, S6PerIntStu, S10NDocDeg,
                          F2RatStafTeaStu, R1Income, R1IncomePStu, R6PerPubIncome,
                          R7ResInc, R7ResIncPSta, R10PerPubResInc) )
  
  particular <- subset( ranking[index3,   ], 
                      select = c(Institution, T.sum, F4PerStaTeaDoc, F5PerIntStaTea, 
                                 R.sum, S1NStu, S5PerGraStu, S6PerIntStu, S10NDocDeg,
                                 F2RatStafTeaStu, R1Income, R1IncomePStu, R6PerPubIncome,
                                 R7ResInc, R7ResIncPSta, R10PerPubResInc) )
  Mean <- round(apply(dat[ , 2:16 ], 2, mean, na.rm = T), digits = 2)
  SD <- round(apply(dat[ , 2:16 ], 2, sd, na.rm = T), digits = 2)
  Min <- round(apply(dat[ , 2:16 ], 2, min, na.rm = T), digits = 2)
  Max <- round(apply(dat[ , 2:16 ], 2, max, na.rm = T), digits = 2)
  particular <- round( t(particular[ ,2:16]), digits = 2)
  abbreviation <- c("T.sum", "F4PerStaTeaDoc", "F5PerIntStaTea", 
                  "R.sum", "S1NStu", "S5PerGraStu", "S6PerIntStu", "S10NDocDeg",
                  "F2RatStafTeaStu", "R1Income", "R1IncomePStu", "R6PerPubIncome",
                  "R7ResInc", "R7ResIncPSta", "R10PerPubResInc")
  result <- data.frame(abbreviation, Min, Mean, Max, SD, particular)
  names(result)[6] <- "Univ Bristol" 
  row.names(result) <- c("Number of Academic Staff - Teaching Related", 
                       "Percentage of academic of Staff (teaching related) with Doctoral level degree",
                       "percentage of International Academic Staff (Teaching Related)",
                       "Number of Academic Staff - Research Only",
                       "Number of Students (Degree - seeking)",
                       "Percentage of Graduate Students",
                       "Percentage of International Students",
                       "Number of Doctoral Degree Awarded",
                       "Ratio of Academic Staff (Teaching related) to Students",
                       "Institutional Income (Millions of USD)",
                       "Institutional Income per Student (Thousands of USD)",
                       "Percentage of Income from Public Sectors",
                       "Research Income (Millions of USD)",
                       "Research Income per Academic Staff (Teaching and Research, Thousands of USD)",
                       "Percentage of Research Income From Public Sector"
                       )
  write.csv(result, file = file.name, row.names = T)
}











