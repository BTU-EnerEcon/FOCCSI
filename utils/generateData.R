library(lubridate)
library(data.table)

generate_sparse_data <- function(from = 2010, to = 2019, filename = "sample_sparse.csv") {

  time <- seq(ymd_hm(paste(from, "-01-01 00:00", sep="")), ymd_hm(paste(to, "-12-31 23:45", sep="")), by = '15 mins')

  HR <- sample(1:1000, length(time), replace=TRUE)
  HR[HR %% sample(1:4, 1) != 0] <- NA
  HR[HR %% sample(1:10, 1) != 0] <- ""
  
  A1<- sample(100:1000, length(time), replace=TRUE)
  A1[A1 %% sample(1:10, 1) != 0] <- NA
  
  A2<- sample(100:1000, length(time), replace=TRUE)
  A2[A2 %% sample(1:4, 1) != 0] <- NA
  A2[A2 %% sample(1:10, 1) != 0] <- ""
  
  A3<- sample(100:1000, length(time), replace=TRUE)
  A3[A3 %% sample(1:4, 1) != 0] <- NA
  A3[A3 %% sample(1:7, 1) != 0] <- ""
  
  A5<- sample(100:1000, length(time), replace=TRUE)
  A5[A5 %% sample(1:6, 1) != 0] <- NA
  A5[A5 %% sample(1:5, 1) != 0] <- ""
  
  MW_J = sample(10000:20000, length(time), replace=TRUE)
  MW_J[MW_J %% sample(1:4, 1) != 0] <- NA
  MW_J[MW_J %% sample(1:9, 1) != 0] <- ""
  
  df = data.frame(
    Zeit=time, 
    Jahr=year(time), 
    Monat=month(time), 
    Tag=mday(time),
    HR = HR,
    K = sample(1:1000, length(time), replace=TRUE),
    A1 = A1,

    A2 = A2, 
    A3 = A3, 
    A4 = sample((1000:10000)/10, length(time), replace=TRUE),
    A5 = sample(100:1000, length(time), replace=TRUE),
    A6 = sample(100:1000, length(time), replace=TRUE),
    A7 = sample(100:1000, length(time), replace=TRUE),
    MW_J = MW_J
  ) 
  
  fwrite(df, file=filename)
  
}

generate_complete_data <- function(from = 2010, to = 2019, filename = "sample_complete.csv") {

  time <- seq(ymd_hm(paste(from, "-01-01 00:00", sep="")),ymd_hm(paste(to, "-12-31 23:45", sep="")), by = '15 mins')
  
  df = data.frame(
    Zeit=time, 
    Jahr=year(time), 
    Monat=month(time), 
    Tag=mday(time),
    HR = sample(1:1000, length(time), replace=TRUE),
    K = sample(1:1000, length(time), replace=TRUE),
    A1 = sample(100:1000, length(time), replace=TRUE),
    A2 = sample(100:1000, length(time), replace=TRUE),
    A3 = sample(100:1000, length(time), replace=TRUE),
    A4 = sample(100:1000, length(time), replace=TRUE),
    A5 = sample(100:1000, length(time), replace=TRUE),
    A6 = sample(100:1000, length(time), replace=TRUE),
    A7 = sample(100:1000, length(time), replace=TRUE),
    MW_J = sample(10000:20000, length(time), replace=TRUE)
  ) 
  
  fwrite(df, file=filename)
  
}