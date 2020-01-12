R.version.string
A <- matrix(data = c(3, 4, 3, 2, 1, -1, 6, -1, -2), nrow=3, ncol=3)
B <- c(6, 10, -4)
X <- solve(t(A), B)
round(X, digits = 2)
