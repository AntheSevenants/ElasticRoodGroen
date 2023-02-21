library(Matrix)
library(glmnet)

DIMENSIONS = 1000

i <- 1:1000
j <- 1:1000
x <- rbinom(DIMENSIONS, 1, 0.5)
M1 <- sparseMatrix(i, j, x=x)
dim(M1)

y <- rbinom(DIMENSIONS, 1, 0.5)

fit <- cv.glmnet(M1,
                 y,
                 type.measure="deviance",
                 alpha=0.7,
                 family="binomial")
coef(fit)
