# #' Example dataset #1.
# #'
# #' A dataset containing example data with variables \code{ID}, \code{gr}
# #' and \code{.row}
# #'
# #' @format A data frame with ... rows and 3 variables:
# #' \describe{
# #'   \item{ID}{An identification code.}
# #'   \item{gr}{A factor variable with severalgroups.}
# #'   \item{.row}{A row number.}
# #'   ...
# #' }
# #' @source Artificially generated in R.
# #' @example
# #'    DataSet1 <- data.frame(ID = gl(n = 20, k = 2),
# #'                          gr = gl(n = 4, labels = LETTERS[1:4], k = 10))
# #'    DataSet1$.row <- 1:nrow(DataSet1)
# #'
# #'    # devtools::use_data(DataSet1)
# #'
# "DataSet1"
#
#
#
#
#  # Load data
#  data("DataSet1")
#
#  # Explore data
#  str(DataSet1)
#  table(DataSet1[,c("gr","ID")])
#  summary(DataSet1)
#
# # Explore functions
#    nFolds = 5
#
# # If variables of data frame are provided:
#    Folds1_a <- stratifiedFolds(data = DataSet1, gr = gr, ID = ID, nFolds, returnTrain=FALSE)
#    # str(Folds1_a)
#    TestFolds(Folds1_a, DataSet1)
#
# # If "free" variables are provided:
#    Folds1_b <- stratifiedFolds(gr = DataSet1$gr, ID = DataSet1$ID, k=nFolds, returnTrain=FALSE)
#    # str(Folds1_b)
#    TestFolds(Folds1_b, DataSet1)
#
# # Not blocked but stratified
#    Folds1_c <- stratifiedFolds(gr = DataSet1$gr, k=nFolds, returnTrain=FALSE)
#    # str(Folds1_c)
#    TestFolds(Folds1_c, DataSet1)
#
#
# # Blocked but not stratified
#    Folds1_d <- stratifiedFolds(ID = DataSet1$ID, k=nFolds, returnTrain=FALSE)
#    # str(Folds1_d)
#    TestFolds(Folds1_d, DataSet1)
#
#  #  Use function `createFolds_stratified`
#    Folds2 <- createFolds_stratified(DataSet1$ID, DataSet1$gr, nFolds)
#
#  #  Use function `createFolds_strat`
#    Folds3 <- createFolds_strat(DataSet1$gr, DataSet1$ID, nFolds)
