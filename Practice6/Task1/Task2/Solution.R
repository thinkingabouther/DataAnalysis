d <- array(c(12, 12, 6, 1, 15, 10, 4, 0, 10, 20, 25, 15, 5, 25, 30, 20, 0, 5, 10, 15), dim = c(4, 5))
chisq.test(d)
mosaicplot(d, shade=TRUE)
