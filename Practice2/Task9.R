A <- array(c(1,2,3,4,2,7,6,9,3,6,3,8,4,9,8,2), dim=c(4,4))
solve(A)
t(A)
diag(A)
det(A)
det(A[-2,-3])*-1