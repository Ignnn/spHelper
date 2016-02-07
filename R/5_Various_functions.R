
# ~~~~~~ Įvairios funkcijos ~~~~~~ ----------------------------------------

# Padaryti pirmą raidę didžiąja -------------------------------------------

#' @name makeFirstCapital
#' @aliases makeFirstCapital
#' @aliases fCap
#' @title [!] Convert the first letter to capital.
#'
#' @description   [!] Convert the first letter to capital.
#' @details \code{fCap} is a wrapper of \code{makeFirstCapital}
#'
#' @param x - a string or vector of strings
#'
#' @return The same string with all words starting with capital letter.
#'
#' @examples
#'
#' makeFirstCapital('laa laa laa')
#'
#' ##[1] "Laa Laa Laa"
#'
#' @export

makeFirstCapital <- function(x)
{
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}


#' @rdname makeFirstCapital
#' @export
fCap <- function(x){makeFirstCapital(x)}


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


# Regular expression (named tokens) ------------------------------------------------------------------
#' [!] Capture substrings to dataframe (regular expression)
#'
#' Capture substrings that match regular expression's named tokens
#' and convert the result to a data frame.
#'
#'
#' @param strings - strings to be parsed.
#' @param pattern - Perl-like regular expression.
#'
#' @return A data frame.
#'
#' @details The syntax how to use named tokens is: \cr
#' '\emph{...}\bold{(?<}\emph{...}>\emph{...})\emph{...}' \cr
#' '\emph{expr1}\bold{(?<}\emph{Name}\bold{>}\emph{expr-to-capture}\bold{)}\emph{expr2}'
#'
#' \itemize{
#'   \item \bold{Name} - the name of token. Any spaces or other special symbols,
#'   inappropriate for variable names, are not allowed and will result in error.
#'   \item \bold{expr-to-capture} - regular expression to be captured as a
#'   value of a variable.
#'   \item \bold{expr1, expr2} - (optional) expressions, that must match, but
#'   that are not captured.
#' }
#'
#' @export
#'
#' @seealso \code{\link{gregexpr}},
#'          \code{\link{regcapturedmatches}},
#'          \code{\link{\%>\%}}
#'
#' @examples
#' strings1 <- c("A_111  B_aaa",
#'               "A_222  B_bbb",
#'               "A_333  B_ccc",
#'               "A_444  B_ddd",
#'               "A_555  B_eee")
#'
#' pattern1 <- 'A_(?<Part_A>.*)  B_(?<Part_B>.*)'
#'
#' regexpr2df(strings1, pattern1)
#'
#' ##     Part_A Part_B
#' ## 1    111    aaa
#' ## 2    222    bbb
#' ## 3    333    ccc
#' ## 4    444    ddd
#' ## 5    555    eee
#'
#' #----------------------------------------------------------
#' # Wrong. There must NOT be any SPACES in token's name:
#'
#' pattern2 <- 'A (?<Part A>.*)  B (?<Part B>.*)'
#' regexpr2df(strings1, pattern2)
#'
#' ## Error ...
#'
#' #----------------------------------------------------------
#' strings3 <- c("sn555 ID_O20-5-684_N52_2_Subt2_01.",
#'               "sn555 ID_O20-5-984_S52_8_Subt10_11.")
#'
#' pattern3 <- paste0('sn(?<serial_number>.*) ID_(?<ID>.*)_(?<Class>[NS])',
#'                    '(?<Sector>.*)_(?<Point>.*)_[Ss]ubt.*\\.');
#'
#' regexpr2df(strings3, pattern3)
#'
#' ##   serial_number    ID       Class Sector Point
#' ## 1      555      O20-5-684     N     52     2
#' ## 2      555      O20-5-984     S     52     8
#'
#' #----------------------------------------------------------
#' # List all .R files in your working directory:
#'
#' regexpr2df(dir(),'(?<R_file>.*\\.[rR]$)')
#'
#'
#' # Do the same by using chaining operator %>%:
#'
#' dir() %>% regexpr2df('(?<R_file>\\.*[rR]$)')
#'
#' #----------------------------------------------------------
#' # Capture several types of files:
#'
#' expr <- paste0('(?<R_file>.*\\.[rR]$)|',
#'                '(?<Rmd_file>.*\\.[rR]md$)|',
#'                '(?<CSV_file>.*\\.[cC][sS][vV]$)')
#' dir() %>% regexpr2df(expr)
#'
#' @import dplyr
#'
regexpr2df <- function(strings, pattern)
{
    ParsedData <- gregexpr(pattern,strings, perl = TRUE);
    as_a_list  <- regcapturedmatches(strings,ParsedData)
    df <- do.call(rbind.data.frame, as_a_list)
    return(df)
}
# list.functions -------------------------------------------------------
#' [!] List all functions in a package.
#'
#' List all functions in a package.
#'
#' @param Package - name of package. Default \code{Package = "spHelper"}.
#' @param print.table - print the result as a table using
#' \code{\link[pander]{pander}}.
#' Default is true.
#'
#' @return A list of functions in a package.
#' @export
#'
#' @importFrom pander pander
#' @examples
#'
#' list.functions()
#' list.functions(Package = "tidyr", print.table = F)
#'
list.functions <- function(Package = "spHelper", print.table = TRUE)
{
    FunctionList <- unclass(lsf.str(envir = asNamespace(Package),
                                    all = TRUE))
    if (print.table){
        pander::pander(as.data.frame(FunctionList))
        invisible(FunctionList)
    } else return(FunctionList)
}


#' Print a line made of symbols.
#'
#' @param symbol - desired symbol or sequence of symbols. Default is "="
#' @param len    - length of a line \cr default is 60.
#' @param after - number of new (empty) lines/rows to be added afterwards.
#'        \code{0} means that following text continues in the same row.
#'        Default is \cr\code{if (print==TRUE) 1 else 0}.
#' @param before - number of peceeding emptyrows. Default is 0.
#' @param print - if \code{TRUE} (defailt) - print,
#'                if \code{FALSE} - return as a string
#'
#' @return
#' @export
#'
#' @examples
#'
#' bru
#' bru("-")
#' bru("= ")
#'
bru <- function(symbol = "=",
                len = 60,
                after  = {if (print) 1 else 0},
                before = 0,
                print  = TRUE){
    # Create sequences of symbols
      nlA <- paste0(rep('\n', after), collapse = "")
      nlB <- paste0(rep('\n', before),collapse = "")
    lineC <- paste0(rep(symbol,len),  collapse = "")
     # Adjust the length
    lineC <- substr(lineC,1,len)
    # Join all symbols
    lineC <- paste0(nlB, lineC, nlA)
    # Either print or return the result
    if (print)  cat(lineC) else return(lineC)
}


# ***** Parse input of function ***** -------------------------------------

#
#
# #' [!] Parse function's arguments: hheck if \code{variable} is inside \code{data} and return values
# #'
# #' Parse function's arguments: Check if \code{variable} is inside \code{data}
# #'  and return values of either \code{data[,"variable"]} (if TRUE) or
# #'  \code{variable} (if FALSE)
# #'
# #' @use Use inside of a function.
# #'
# #' @param data
# #' @param variable
# #'
# #' @return
# #' @export
# #'
# #' @examples
# varValues <-  function(data, variable) {
#     varName <- as.character(match.call()$variable)
#     output  <- if (varName %in% ls(data)) data[,varName] else variable
#     return(output)
# }


