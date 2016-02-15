# Sukurti sluoksninius k-folds -------------------------------------------
#
#' @name stratifiedFolds
#' @aliases stratifiedFolds
#' @aliases createFolds_stratified
#' @aliases createFolds_strat
#'
#' @title [~] Block observations and Create stratified folds for k-fold cross-validation.
#'
#'
#' @description
#' Function devides observations into folds that are used for k-fold cross-validation. In
#' these folds observations are:
#' \enumerate{
#'      \item \bold{blocked} by values in variable \code{ID} (i.e. observations
#'        with the same "ID" are treated as one unit and are always in the same fold)
#'      \item \bold{stratified} by levels of factor variable \code{gr} (the proportions of
#'      these grouped units of observations per each group (level) are kept aproximately
#'      constant throughout all folds).
#'  }
#'
#' @note If \code{k} is such big, that some folds have no observations of a certain group
#'       (i.e. level in \code{gr}), an error is returned. In that case smaller value of
#'        \code{k} is recommended.
#'
#' @param data A data frame, that contains variables denoted by \code{ID} and by \code{gr}.
#'
#' @param gr A vector or a name of factor variable in {data}, which levels
#'                 will be used for stratification. E.g., vector with medical groups.
#'
#' @param ID A vector or a name of variable in {data}, that contains
#'       identification codes/numbers (ID).
#'
#' @param k A number of folds, default k = 5.
#'
#' @param returnTrain Logical. If \code{TRUE}, returns indices of variables in training
#'                    set. If \code{FALSE}, returns indices of variables in
#'                    test set.
#'
#'
#' @return A list of folds. In each fold indices observations.
#'         The structure of outpus is the same as if it was created by
#'         \code{\link[caret]{createFolds}}.
#'
#'
#' @examples
#'
#' TestFolds <- function(Folds)lapply(Folds, function(x)table(df[x,"gr"]))
#'
#'
#' # Make data with 20 different ID's and 4 different groups:
#'    df <- data.frame(gr = gl(n = 4, labels = LETTERS[1:4], k = 10),
#'                     ID = gl(n = 20, k = 2))
#'
#'    table(df)
#'    summary(df)
#'
#'    nFolds = 4
#'
#' # If variables of data frame are provided:
#'    Folds1_a <- stratifiedFolds(df, gr, ID, nFolds, returnTrain=FALSE)
#'    str(Folds1_a)
#'    TestFolds(Folds1_a)
#'    as.data.frame(TestFolds(Folds1_a)) # <------- this line needs further developement
#'
#' # If "free" variables are provided:
#'    Folds1_b <- stratifiedFolds(gr = df$gr, ID = df$ID, k=nFolds, returnTrain=FALSE)
#'    str(Folds1_b)
#'    TestFolds(Folds1_b)
#'
#' # Blocked but not stratified
#'    Folds1_c <- stratifiedFolds(ID = df$ID, k=nFolds, returnTrain=FALSE)
#'    str(Folds1_c)
#'    TestFolds(Folds1_c)
#'
#' # Not blocked but stratified  ---- ERROR
#'    Folds1_d <- stratifiedFolds(gr = df$gr, k=nFolds, returnTrain=FALSE)
#'    str(Folds1_d)
#'    TestFolds(Folds1_d)
#'
#'  #  ---- ERROR
#'    Folds2 <- createFolds_stratified(df$ID, df$gr, nFolds)
#'
#'  #  ---- ERROR
#'    Folds3 <- createFolds_strat(df$gr, df$ID, nFolds)
#'
#' @export
#'
stratifiedFolds <- function(data=NULL, gr=NULL, ID=NULL, k = 5, returnTrain = TRUE)
{
    nFolds <- k
    CALL <- match.call()
    if (!is.null(CALL$data)){ # if `data` is provided:
        ID <- getVarValues(ID, data, CALL)
        gr <- getVarValues(gr, data, CALL)
    }

    if (is.null(ID) & length(gr) > 1) ID <- rep(0,length(gr))
    if (is.null(gr) & length(ID) > 1) gr <- rep(0,length(ID))

    if (length(ID)!=length(gr)) stop("Length of `ID` and `gr` must agree.")
   # -----------------------------------------------------------------------
    data <- data.frame(ID = ID, gr = gr)

    # get unique values only
    df <- unique(data)

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

    data  <- merge(data, df)
    Ind_all <- 1:nrow(data)
    data$Test_ind <- Ind_all

    Test_ind <- split(data$Test_ind, data$Fold)

    if (returnTrain == FALSE){
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

createFolds_stratified <- function(ID = NULL, gr = NULL, k = 5, returnTrain=TRUE)
{
    # data <- data.frame(ID = AMP_obj2$ID, gr = AMP_obj2$gr)
    data <- data.frame(ID = ID, gr = gr)
    createFolds_strat(data, k = k, returnTrain = returnTrain)
}


#' @rdname stratifiedFolds
#' @export
createFolds_strat <- function(gr = NULL, ID = NULL, k = 5, returnTrain=TRUE)
{
    data <- data.frame(ID = ID, gr = gr)
    createFolds_strat(data, k = k, returnTrain = returnTrain)
}

