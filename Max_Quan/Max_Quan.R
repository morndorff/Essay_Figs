# Maximum Quantile Distance Plot
# Will want at least 4 different plots: based on empirical cdf
# One sample: don't want to estimate the 100th quantile

rm(list=ls())
source("functions.R")
x <- rnorm(40); y <- rnorm(40);
Max_Quan_TS(x,y, do.plot=TRUE)
Max_Quan_TS(x,qnorm, do.plot=TRUE)