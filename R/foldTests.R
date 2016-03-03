
#' [+] Test if data in folds is stratified and blocked
#'
#' This function run tests, that help to evaluate if data in folds is
#'  (a) stratified and (b) blocked.
#'
#' @param FoldsOBJ A list with validation/test set indices in folds.
#'        \bold{Note:} If indices are from training set, the result will be
#'        incorrect.
#' @param DATA A data frame, for wich \code{FoldsOBJ} was created.
#' @param stratBy A name of variable used for stratification.
#' @param blockBy A name of variable used for blocking.
#' @param nColShow Number of blocking variable cross-tabulation's columns to
#'        be shown. Default is 10.
#'
#' @return Print tables that help to evaluate if data is \strong{(a)} stratified,
#'  \strong{(b)} blocked.
#' @export
#'
#' @examples
#' data(DataSet1)
#' FoldsOBJ <- stratifiedFolds(data = DataSet1, gr = gr, ID = ID, returnTrain = FALSE)
#' foldsTets(FoldsOBJ, DataSet1)
#'
#' #>  ************************************************************
#' #>      Test for STRATIFICATION
#' #>
#' #>        A B C D      <<<     >>>              A    B    C    D
#' #>  Fold1 2 2 2 2  <-Counts | Proportions->  0.25 0.25 0.25 0.25
#' #>  Fold2 2 2 2 2  <-Counts | Proportions->  0.25 0.25 0.25 0.25
#' #>  Fold3 2 2 2 2  <-Counts | Proportions->  0.25 0.25 0.25 0.25
#' #>  Fold4 2 2 2 2  <-Counts | Proportions->  0.25 0.25 0.25 0.25
#' #>  Fold5 2 2 2 2  <-Counts | Proportions->  0.25 0.25 0.25 0.25
#' #>
#' #>  If stratified, the proportions of each group in each fold
#' #>  (row) should be (approximately) equal and with no zero values.
#' #>  ____________________________________________________________
#' #>  Test for BLOCKING: BLOCKED
#' #>
#' #>        1 2 3 4 5 6 7 8 9 10 ..
#' #>  Fold1 0 0 0 0 2 0 0 2 0  0 ..
#' #>  Fold2 2 0 0 0 0 2 0 0 0  0 ..
#' #>  Fold3 0 0 2 0 0 0 0 0 2  0 ..
#' #>  Fold4 0 0 0 2 0 0 2 0 0  0 ..
#' #>  Fold5 0 2 0 0 0 0 0 0 0  2 ..
#' #>
#' #>  Number of unique IDs in each fold (first 10 columns).
#' #>  If blocked, the same ID appears just in one fold.
#' #>  ************************************************************
#'
#'
#'
#'
#'
#' @family \code{spHelper} fold creation functions
#' @author Vilmantas Gegzna
#'
foldsTets <- function(FoldsOBJ, DATA, stratBy = "gr", blockBy = "ID",
                      nColShow = 10) {

    # Calculate:
    funS <- function(x)table(DATA[x, stratBy])
    funB <- function(x)table(DATA[x, blockBy])
    lS   <- lapply(FoldsOBJ, funS)
    lB   <- lapply(FoldsOBJ, funB)
    rezS <- do.call("rbind", lS)
    rezB <- do.call("rbind", lB)

    # BLOCKING Test
    # Teat all columns, even those, that are not displayed.
    B <- if (any(colSums(rezB != 0) != 1) == FALSE) {
        "BLOCKED"# "The same ID appears just in one fold"
        } else "NOT BLOCKED"

    # BLOCKING visualozation
    nColToShow <- min(nColShow,ncol(rezB))
    B2 <- (rezB[,1:nColToShow])
    if (nColShow < ncol(rezB)) {
        `..` <- rep("..",nrow(rezB))
        B2 <- as.data.frame(cbind(B2, `..`))
    }

    # STRATIFICATION visualozation
    S <- as.data.frame(round(prop.table(rezS,1),2))
    SS <- cbind(rep(" <-Counts | Proportions-> ", nrow(S)), S)
    names(SS)[1] <- " <<<     >>>          "
    SS <- cbind(rezS, SS)

    # Print the results:
    bru("*");
    cat("                Test for STRATIFICATION \n\n")
    print(SS)
    cat("\nIf stratified, the proportions of each group in each fold\n")
    cat("(row) should be (approximately) equal and with no zero values.\n")

    bru("_");
    cat(sprintf("                Test for BLOCKING: %s\n\n", B))
    print(B2);
    cat(sprintf("\nNumber of unique IDs in each fold (first %d columns displayed).\n",
                nColToShow))
    cat("If blocked, the same ID appears just in one fold.\n")
       bru("*")
}
