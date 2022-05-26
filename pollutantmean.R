pollutantmean <- function(directory, pollutant, id = 1:332){
  directory <- paste(getwd(), "/", directory, "/", sep = "")
  file_list <- list.files(directory)
  data <- NA
  for(i in id){
    file_dir <- paste(directory, file_list[i], sep = "")
    file_data <- read.csv(file_dir, header = TRUE)
    data <- rbind(data, file_data)
  }
  mean(data[[pollutant]], na.rm = TRUE)
}

complete <- function(directory, id = 1:332){
     directory <- paste(getwd(), "/", directory, "/", sep = "")
     file_list <- list.files(directory)
     ids <- c()
     num <- c()
     for (i in id) {
         file_dir <- paste(directory, file_list[i], sep = "")
         file_data <- read.csv(file_dir, header = TRUE)
         ids = c(ids, i)
         num = c(num, sum(complete.cases(file_data)))
       }
     data.frame(id = ids, nobs = num)
   }

corr <- function(directory, threshold = 0){
  directory <- paste(getwd(), "/", directory, "/", sep = "")
  observations <- complete("specdata")
  filtered_observations <- subset(observations, observations$nobs > threshold)
  file_list <- list.files(directory)
  correlation <- c()
  for (i in id){
    file_dir <- paste(directory, file_list[i], sep = "")
    file_data <- read.csv(file_dir, header = TRUE)
    file_data <- subset(file_data, complete.cases(file_data))
    correlation <- c(correlation, cor(file_data$nitrate, file_data$sulfate))
  }
  correlation
}

