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

boing <- lapply(1:1000, function(value) {
  x <- value
  y <- 1000 - value
  z <- value + 2
  
  return(c(x=x, y=y, value=z))
})

test <- as.data.frame(t(boing))
test_2 <- as.data.frame(do.call(rbind, boing))

# apply test

feature_matrix_list <- apply(df, 1, function(row) { 

  return(c(x=1, y=2, value=3))
})

as.data.frame(t(feature_matrix_list))


source("ElasticToolsR/Dataset.R")

# Read sampled dataframe
df <- read.csv("RG.csv")

# Coerce the response variable column to a factor
df$wvo <- factor(df$wvo)
# Coerce the lexical influence column to a factor
df$deelwoord <- as.factor(df$deelwoord)

# Create an ElasticTools dataset
ds <- dataset(df=df,
              response_variable_column="wvo",
              to_binary_column="deelwoord")
matrix <- ds$as_matrix()

source("ElasticToolsR/ElasticNet.R")

# Elastic Net regression itself
net <- elastic_net(ds=ds,
                   feature_matrix=matrix)
fit <- net$do_lasso_regression()
coef(fit)
