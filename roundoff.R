Name <- list.files(path = "./original_csv_file/")
for (j in 1:length(Name)){
  file.name <- paste0("./original_csv_file/", Name[j])
  dat <- read.csv(file.name, head = T)
  dat$X <- as.character(dat$X)
  dat$abbreviation <- as.character(dat$abbreviation)
# str(dat)
  a <- dim(dat)[1]
  b <- dim(dat)[2]
  for (i in 1:a){
    if (dat[i, 4] >= 1){
    dat[i, 3:b] <- round(dat[i, 3:b])
    }
    else {
    dat[i, 3:b] <- round(dat[i, 3:b], 2) 
    } 
  }
  write.csv(dat, Name[j], row.names = FALSE)
}











