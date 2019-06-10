library(dplyr)
library(boot)
data(melanoma)
head(melanoma)
titanic <- read.csv("titanic.csv", sep = ';')
head(titanic)



my_data_function <- function(your_data, rw = 0, clmn = 0, calc_fun) {
  if (rw == 0 && clmn == 0) {
    res <- list(subs = your_data %>% as.data.frame()) 
  } else if (rw == 0) {
    res <- list(subs = your_data[, clmn] %>% as.data.frame()) 
  } else if (clmn == 0) {
    res <- list(subs = your_data[rw,] %>% as.data.frame()) 
  } else {
    res <- list(subs = your_data[rw,clmn] %>% as.data.frame())
  }
  
  res <- c(res, calculus_res = res$subs %>% as.data.frame %>% apply(2,calc_fun))
  res
}


### 

calculation <- function(x) {
  if (is.numeric(x)) {
    calc <- mean(x)
  } else {
    calc <- table(x)
  }
  return(calc)
}

##
#Examples of subsetting and computing

a <- my_data_function(melanoma, rw = 1:10, clmn = c(1,3,6), calc_fun = calculation)
head(a)

#a <- my_data_function(melanoma, clmn = c(1,3,6), calc_fun = calculation)
#head(a)

#a <- my_data_function(melanoma, c(1,2,3), calc_fun = calculation)
#head(a)

#a <- my_data_function(melanoma, clmn  = c(T,F, F, F, T, F, F), calc_fun = calculation)
#head(a)

#a <- my_data_function(melanoma, clmn = c("time", "age"), calc_fun = calculation)
#head(a)

#a <- my_data_function(melanoma, rw = 3:6, calc_fun = calculation)
#head(a)

#a <- my_data_function(melanoma, rw = c(3,4,5,6), calc_fun = calculation)
#head(a)

#a <- my_data_function(melanoma, calc_fun = calculation)
#head(a)

#a <- my_data_function(titanic, rw = 1:1000, clmn = 2, calc_fun = calculation)
#head(a[names(a) != "subs"])

a <- my_data_function(titanic, clmn= 1:3, calc_fun = calculation)
head(a[names(a) != "subs"])

## Play with split()
melanoma$age %>% split(as.factor(melanoma$status)) %>% 
  lapply(mean)
# Here status means: 1 - had died from melanoma, 2 - still alive, 
# 3 - had died from causes unrelated to melanoma




