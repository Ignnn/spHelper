# Sukurti sluoksninius k-folds ------------------------------------------------------------------
#-
#' @name stratifiedFolds
#'
#' @aliases stratifiedFolds
#' @aliases createFolds2
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
#'        \code{k} is recommended. \cr \cr
#'         \code{createFolds2} is a wrapper of \code{stratifiedFolds}.
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
#' @export
#' @examples
#'
#' # Load data
#' data("DataSet1")
#'
#' # Explore data
#' str(DataSet1)
#' table(DataSet1[,c("gr","ID")])
#' summary(DataSet1)
#'
#' # Explore functions
#' nFolds = 5
#'
#' # If variables of data frame are provided:
#' Folds1_a <- stratifiedFolds(data = DataSet1, gr = gr, ID = ID, nFolds, returnTrain=FALSE)
#' # str(Folds1_a)
#' TestFolds(Folds1_a, DataSet1)
#'
#' # If "free" variables are provided:
#' Folds1_b <- stratifiedFolds(gr = DataSet1$gr, ID = DataSet1$ID, k=nFolds, returnTrain=FALSE)
#' # str(Folds1_b)
#' TestFolds(Folds1_b, DataSet1)
#'
#' # Not blocked but stratified
#' Folds1_c <- stratifiedFolds(gr = DataSet1$gr, k=nFolds, returnTrain=FALSE)
#' # str(Folds1_c)
#' TestFolds(Folds1_c, DataSet1)
#'
#'
#' # Blocked but not stratified
#' Folds1_d <- stratifiedFolds(ID = DataSet1$ID, k=nFolds, returnTrain=FALSE)
#' # str(Folds1_d)
#' TestFolds(Folds1_d, DataSet1)
#'
#'
stratifiedFolds <- function(data=NULL, gr=NULL, ID=NULL, k = 5, returnTrain = TRUE)
{
    nFolds <- k

    # Parse input and prepare data ===========================================
    CALL <- match.call()
    if (!is.null(CALL$data)) { # if `data` is provided:
        ID <- getVarValues(ID, data, CALL)
        gr <- getVarValues(gr, data, CALL)
    }
    # If either `ID` or `gr` is not provided:
    if (is.null(ID) & length(gr) > 1) {ID <- 1:length(gr) }      # create unique IDs
    if (is.null(gr) & length(ID) > 1) {gr <- rep(0,length(ID))}  # create one level of `gr`
    # -----------------------------------------------------------------------
    if (length(ID) != length(gr)) stop("Length of `ID` and `gr` must agree.")
    # -----------------------------------------------------------------------
    data <- data.frame(ID = ID, gr = gr)

    # get unique values only
    df <- unique(data)

    # Calculations  ==========================================================
    df$Fold <- rep(NA, times = nrow(df))
    nGr     <- length(levels(as.factor(df$gr))) # NA's are not included

    df_ByGr      <- split(df, df$gr)
    n_ByGr       <- sapply(df_ByGr, nrow)     # unique IDs per class

    # If Number of observatuions in a group is to small
    if (any(n_ByGr < nFolds)) {
        print(sprintf('nFolds = %d', nFolds))
        print(n_ByGr)
        stop("Number of UNIQUE observations in one of the groups is smaller than number of folds.")
    }

    # Assign numbers of fold to each row
    # Split to folds in a stratified way by group 'gr'
    for (gr_i in 1:nGr) {
        GrSize     <-  n_ByGr[gr_i]
        TimesEach  <-  GrSize %/% nFolds  # modulus - how many times observations are devided
                                          #           proportionally to each fold.

        nRem   <-  GrSize %%  nFolds    # reminder - number of observations, that cannot
                                        # be devided proportionally.

        # Separate permutations ensures more proportional distribution when
        # number of observations is small:
        Proportionals <-  rep(1:nFolds, times = TimesEach)   # create a list of proportionally
                                                             # distributed per fold
        Proportionals <-  sample(Proportionals, GrSize - nRem) # permute the list of proportionally distributed
         Reminders    <-  sample(1:nFolds, nRem)             # permute reminders separately

        BelongsToFoldNr <-  c(Proportionals,Reminders) # Merge

        df_ByGr[[gr_i]]$Fold = paste0("Fold", BelongsToFoldNr)
    }

    # unsplit the dataframe: NA's removed
    df <- unsplit(df_ByGr, df$gr[!is.na(df$gr)])

    data    <- merge(data, df, sort = FALSE)

    if (!all(data$ID == ID)) {warning(paste("Order of indices is wrong.",
        "IDs might be incorrectrly sorted inside function 'stratifiedFolds'"))}


    Ind_all <- 1:nrow(data)  # Additional column with row numbers
    data$Test_ind <- Ind_all

    Test_ind <- split(data$Test_ind, data$Fold)

    if (returnTrain == FALSE) {
        return(Test_ind)
    }
    else {
        Train_ind <- lapply(Test_ind, function(x){setdiff(Ind_all, x)})
        return(Train_ind)
    }
}

# [END]

#================================================================================
#' @rdname stratifiedFolds
#' @export
createFolds2 <- function(...,k = 5){
    stratifiedFolds(..., k = k)
}
#================================================================================
