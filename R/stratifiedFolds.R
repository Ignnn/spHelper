# Sukurti sluoksniuotus k-folds -------------------------------------------
#
#' @name stratifiedFolds
#' @aliases stratifiedFolds
#' @aliases createFolds_stratified
#'
#' @title [!] Create stratified and blocked folds for k-fold cross-validation
#'
#'
#' @description Create folds that are stratified by levels in variable "gr" in
#' which observations are blocked by "ID" (i.e. observations with the same "ID"
#' are treated as one observation and are always in the same fold). \cr
#'
#' Folds are used for for k-fold cross-validation. \cr
#' In folds spectra are:
#' \enumerate{
#'      \item \strong{grouped} by ID (the same ID appears just in one fold) and
#'      \item \strong{stratified} by levels of factor variable 'gr' (the
#'      proportions of groups with unique IDs belonging to particular class
#'      are kept aproximately the same throughout all folds)
#'}
#'
#' @param returnTrain - returnTrain=TRUE
#' @return Result is a list of folds. In each fold indices observations.
#'         The structure of outpus is the same as if it was created by
#'         \code{\link[caret]{createFolds}}
#'
#'
#' @param df_all - a data frame, that contains variables "ID"  and "gr" \cr
#' \cr
#' names(df_all) \cr
#' ##   [1] "ID" "gr"
#' @param ID - vector
#' @param groups - vector
#' @param k - number of folds, default k = 5
#' @param returnTrain - ....returnTrain=TRUE
#' @examples
#'
#' # Make data with 20 different ID's and 4 different groups:
#'    df <- data.frame(ID = gl(n = 20, k = 2),
#'                     gr = gl(n = 4, labels = LETTERS[1:4], k = 10)
#'                     )
#'
#'   df
#'
#'    nFolds = 5
#'
#'    Folds1 <- createFolds_stratified(df, nFolds)
#'    Folds2 <- stratifiedFolds(df$ID, df$groups, nFolds)
#'
#' @export
#'
stratifiedFolds <- function(df_all, k = 5, returnTrain=TRUE)
{
    nFolds <- k

    # get only unique values
    df <- unique(df_all)

    # Calculations
    df$Fold <- rep(NA, times = nrow(df))
    nGr     <- length(levels(as.factor(df$gr))) # NA's are not included

    df_ByGr      <- split(df, df$gr)
    n_ByGr       <- sapply(df_ByGr, nrow)
    nInFold_ByGr <- ceiling(n_ByGr / nFolds)

    # If Number of observatuions in a group is to small
    if (any(n_ByGr < nFolds))
    {   print(sprintf('nFolds = %d', nFolds))
        print(n_ByGr)
        stop("Number of UNIQUE observations in one of the groups is smaller than number of folds")
    }

    # Assign numbers of fold to each row
    # Split to folds in a stratified way by group 'gr'
    for (gr_i in 1:nGr)
    {          GrSize   <-  n_ByGr[gr_i]
    PossibleFolds   <-  rep(1:nFolds, times = nInFold_ByGr[gr_i])
    BelongsToFoldNr <-  sample(PossibleFolds,GrSize) # permute and make appropriate langth
    df_ByGr[[gr_i]]$Fold = paste0("Fold", BelongsToFoldNr)
    }

    # unsplit the dataframe: NA's removed
    df <- unsplit(df_ByGr, df$gr[!is.na(df$gr)])

    df_all  <- merge(df_all, df)
    Ind_all <- 1:nrow(df_all)
    df_all$Test_ind <- Ind_all

    Test_ind <- split(df_all$Test_ind, df_all$Fold)

    if (returnTrain == FALSE)
    {
        return(Test_ind)
    }
    else {
        Train_ind <- lapply(Test_ind, function(x){setdiff(Ind_all, x)})
        return(Train_ind)
    }
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @rdname stratifiedFolds
#' @export

createFolds_stratified <- function(ID = NULL, groups = NULL, k = 5, returnTrain=TRUE)
{
    # df_all <- data.frame(ID = AMP_obj2$ID, gr = AMP_obj2$gr)
    df_all <- data.frame(ID = ID, gr = groups)
    createFolds_strat(df_all, k = k, returnTrain = returnTrain)
}
