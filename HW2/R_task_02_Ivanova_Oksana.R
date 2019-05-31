library(boot)
data(melanoma)
head(melanoma)

# Here you need some additional file "titanic.csv"
titanic <- read.csv("titanic.csv", sep = ';')
head(titanic)


# Function
my_data_function <- function(your_data, rw = 0, clmn = 0) {
  if (rw == 0 && clmn == 0) {
    res <- list(subs = as.data.frame(your_data)) 
  } else if (rw == 0) {
    res <- list(subs = as.data.frame(your_data[, clmn])) 
  } else if (clmn == 0) {
    res <- list(subs = as.data.frame(your_data[rw,])) 
  } else {
    res <- list(subs = as.data.frame(your_data[rw,clmn]))
  }
  
  calc = list()
  
  for (col in colnames(res$subs)) {
    if (is.numeric(res$subs[[col]])) {
      tmp <- mean(res$subs[[col]])
      names(tmp) <- col
      calc <- c(calc, tmp)
    } else {
      tmp <- summary(res$subs[[col]])
      names_tmp <- paste0(col, names(tmp))
      names(tmp) <- names_tmp
      calc <- c(calc, tmp)
    }
  }
  
  res <- c(res, calculus = calc)
  res
}



#Examples of subsetting and computing

###1
a <- my_data_function(melanoma, rw = 1:10, clmn = c(1,3,6))
head(a$subs)
a[-1]
###2
a <- my_data_function(melanoma, clmn = c(1,3,6))
head(a$subs)
a[-1]
###3
a <- my_data_function(melanoma, c(1,2,3))
head(a)
###4
a <- my_data_function(melanoma, clmn  = c(T,F, F, F, T, F, F))
head(a$subs)
a[-1]
###5
a <- my_data_function(melanoma, clmn = c("time", "age"))
head(a$subs)
a[-1]
###6
a <- my_data_function(melanoma, rw = 3:6)
head(a$subs)
a[-1]
###7
a <- my_data_function(melanoma, rw = c(3,4,5,6))
head(a$subs)
a[-1]
###8
a <- my_data_function(melanoma)
head(a$subs)
a[-1]
###9
a <- my_data_function(titanic, rw = 1:1000)
head(a$subs)
a[-1]