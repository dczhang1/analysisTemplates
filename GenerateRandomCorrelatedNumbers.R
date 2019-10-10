n     <- 1000                    # length of vector
rho   <- 0.38                  # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
x1    <- rnorm(n, 0, 1)        # fixed given data
x2    <- rnorm(n, 0, 1)      # new random data
X     <- cbind(x1, x2)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)
x1
Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

x <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(x1, x)                                    # check correlation = rho
plot(x1, x)                                   # plot x1,x

xtable <- cbind(x1,x)
write.csv(xtable, file = "correlation.csv")


