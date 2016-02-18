
#' [+] Evaluate if data in folds is stratified and blocked
#'
#' Evaluate if data in folds is stratified and blocked.
#'
#' @param FoldsOBJ A list with validation/test set indices in folds.
#' @param DATA A data set, for wich \code{FoldsOBJ} was created.
#' @param stratBy A name of variable used for stratification.
#' @param blockBy A name of variable used for blocking.
#'
#' @return Print tables that help to evaluate if data is \strong{(A)} stratified,
#'  \strong{(B)} blocked.
#' @seealso \code{\link{stratifiedFolds}}
#' @export
#'
#' @examples
#' data(DataSet1)
#' FoldsObj <- stratifiedFolds(data = DataSet1, gr = gr, ID = ID, returnTrain=FALSE)
#' TestFolds(FoldsObj, DataSet1)
#'
TestFolds <- function(FoldsOBJ, DATA, stratBy = "gr", blockBy = "ID") {
    # Calculate:
    funS <- function(x)table(DATA[x, stratBy])
    funB <- function(x)table(DATA[x, blockBy])
    lS   <- lapply(FoldsOBJ, funS)
    lB   <- lapply(FoldsOBJ, funB)
    rezS <- do.call("rbind", lS)
    rezB <- do.call("rbind", lB)

    # Print:
    bru("*");
    cat("Number of observations per class in each fold:\n")
    bru("_");
    cat("If stratified, the proportions in each fold should\nbe equal/similar:\n\n")
    print(rezS);
    bru("_");
    cat("If blocked, the same ID appears just in one fold:\n\n")
    print(rezB);
    bru("*")
}
