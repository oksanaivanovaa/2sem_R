library(boot)
data(melanoma)
head(melanoma)
titanic <- read.csv("titanic.csv", sep = ';')
head(titanic)


my_data_function <- function(your_data, rw = 0, clmn = 0, calc_fun) {
  if (rw == 0 && clmn == 0) {
    res <- list(subs = as.data.frame(your_data)) 
  } else if (rw == 0) {
    res <- list(subs = as.data.frame(your_data[, clmn])) 
  } else if (clmn == 0) {
    res <- list(subs = as.data.frame(your_data[rw,])) 
  } else {
    res <- list(subs = as.data.frame(your_data[rw,clmn]))
  }
  
  
  num_subs <- c()
  cat_subs <- c()
  
  for (col in colnames(res$subs)) {
    if (is.numeric(res$subs[[col]])) {
      num_subs <- c(num_subs, col)
    } else {
      cat_subs <- c(num_subs,col)
    }
  }
  
  num_subs <- as.matrix(res$subs[num_subs])
  cat_subs <- as.matrix(res$subs[cat_subs])
  
  calc <- calc_fun(num_subs)
  res <- c(res, calculus_num = calc)
  calc <- calc_fun(cat_subs)
  res <- c(res, calculus_cat = calc)
  res
}


### 

calculation <- function(sub_data) {
  calc = list()
  if (is.numeric(sub_data)) {
    calc <- colMeans(sub_data)
  } else {
    calc <- apply(sub_data, 2, table)
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

a <- my_data_function(titanic, rw = 1:1000, calc_fun = calculation)
head(a[names(a) != "subs"])


## Play with split()
status_age <- split(melanoma$age, as.factor(melanoma$status))
# Here status means: 1 - had died from melanoma, 2 - still alive, 
# 3 - had died from causes unrelated to melanoma
lapply(status_age, mean)





