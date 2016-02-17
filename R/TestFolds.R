
#' [TMP] Test if data in folds is stratified and blocked.
#'
#' @param FoldsOBJ list with indices in folds.
#' @param DATA A data set, for wich \code(FoldsOBJ) was created.
#' @param stratBy A name of variable used for stratification.
#' @param blockBy A name of variable used for blocking.
#'
#' @return Print tables to evaluate if data is (A) stratified, (B) blocked.
#' @export

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
    cat("If stratified - proportions in each fold should be similar:\n\n")
    print(rezS);
    bru("_");
    cat("If blocked - same ID appears in one fold:\n\n")
    print(rezB);
    bru("*")
}
