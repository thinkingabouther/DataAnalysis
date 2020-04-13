L <- c(1, 0, 2, 3, 6, 8, 12, 15, 0, NA, NA, 9, 4, 16, 2, 0)
L[1]
L[length(L)]
L[3:5]
L[L == 2 & !is.na(L)]
L[L > 4 & !is.na(L)]
L[L %% 3 == 0 & !is.na(L)]
L[L> 4 & L %% 3 == 0 & !is.na(L)]
L[(L < 1 | L > 5) & !is.na(L)]
which(L==0)
L[L >= 2 & L <= 8 & !is.na(L)]

