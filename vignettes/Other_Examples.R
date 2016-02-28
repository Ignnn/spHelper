## ----options, echo = FALSE, message = FALSE, warning = FALSE-------------
optDEF <- knitr::opts_chunk$get()
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

knitr::opts_chunk$set(fig.width = 6, fig.align = 'center')

## ----Load packages, message = FALSE, warning = FALSE---------------------
library(spHelper)



## ------------------------------------------------------------------------
nFolds = 5

# Load data
data("DataSet1")

# Explore data
str(DataSet1)

# table(DataSet1[,c("gr","ID")])
# summary(DataSet1)

## ------------------------------------------------------------------------
set.seed(1)
c1 <- createFolds2(ID = DataSet1$ID, k = nFolds)
set.seed(1)
c2 <- stratifiedFolds(ID = DataSet1$ID, k = nFolds)
identical(c1, c2)

## ------------------------------------------------------------------------
Folds1_a <- stratifiedFolds(data = DataSet1, gr = gr, ID = ID, nFolds, returnTrain = FALSE)
# str(Folds1_a)
foldsTets(Folds1_a, DataSet1)


## ------------------------------------------------------------------------
Folds1_b <- stratifiedFolds(gr = DataSet1$gr, ID = DataSet1$ID, k = nFolds, returnTrain = FALSE)
# str(Folds1_b)
foldsTets(Folds1_b, DataSet1)

## ------------------------------------------------------------------------
Folds1_c <- stratifiedFolds(gr = DataSet1$gr, k = nFolds, returnTrain = FALSE)
# str(Folds1_c)
foldsTets(Folds1_c, DataSet1)

## ------------------------------------------------------------------------
Folds1_d <- stratifiedFolds(ID = DataSet1$ID, k = nFolds, returnTrain = FALSE)
# str(Folds1_d)
foldsTets(Folds1_d, DataSet1)


## ------------------------------------------------------------------------
m1 <- matrix(NA, 5, 5)
m1

which.in.diag(m1)

m2 <- matrix(NA, 2, 5)
which.in.diag(m2)

# ================================

which.in(diag,    m1)
which.in(offdiag, m1)

which.in(col, m1, col = 2)
which.in(row, m1, row = 2)

which.in(trilow, m1)
which.in(trilow, m1, diag = TRUE)

which.in(triupp, m1)
 
# ================================
 
r1 <- which.in(trilow, m1)
r2 <- which.in.trilow(m1)
identical(r1, r2)
 

## ------------------------------------------------------------------------
# ------------------------------------------------------------
# Example 1A: Vector into a matrix

vector = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
corr_vec2mat(vector)

# ------------------------------------------------------------

 # Example 1B: Vector is transformed to a matrix by filling
 # it column-wise:

## ------------------------------------------------------------------------
 # ---------------------------------------------------------------------
 # Example 2: inappropriate number of coefficients - warning appears

 # corr_vec2mat(vector[1:5])
    #> Warning message:
    #> Only first 3 coefficient(s) out of 5 will be used to construct symmetric matrix with 3 rows.

 # --------------------------------------------------------------------
 # Example 3: ERROR appears - values of coeffs must be between [-1;1]

 # corr_vec2mat(1:5)
    #> Error in corr_vec2mat(1:5) :
    #> All values in input vector must be between [-1 and 1]


