
# ~~~~~~ Įvairios funkcijos ~~~~~~ ----------------------------------------

# Padaryti pirmą raidę didžiąja -------------------------------------------

#' Convert the first letter to capital.
#'
#' @param x - a string
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


# Sukurti sluoksniuotus k-folds -------------------------------------------
#
#' Create stratified and blocked folds for k-fold cross-validation
#'
#' Create folds that are stratified by levels in variable "gr" in which
#' observations are blocked by "ID" (i.e. observations with the same "ID"
#' are treated as one observation and are always in the same fold).
#'
#' Folds are used for for k-fold cross-validation.
#'
#' \code{\link{createFolds_strat}}
#'
#'
#' @param df_all - a data frame, that contains variables "ID"  and "gr"
#'
#' names(df_all)
#' ##   [1] "ID" "gr"
#'
#' @param k - number of folds, default k = 5
#' @param returnTrain - returnTrain=TRUE
#' @return Result is the same as produced by function \code{\link{caret::createFolds}}
#' @examples
#'
#'
#'    df_all <- Object$..[,c("ID","gr")]
#'    nFolds  = 5
#'    createFolds_strat(df_all, nFolds)
#'
#'    @export
#'
createFolds_strat <- function(df_all, k = 5, returnTrain=TRUE)
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

#' Wraper of the function \code{\link{createFolds_strat}}
#'
#' @param ID - vector
#' @param groups - vector
#' @param k - number of folds k = 5
#' @param returnTrain - returnTrain=TRUE
#' @return Result is the same as produced by function \code{\link{createFolds}}
#' @examples createFolds_stratified
#'
#' @export

createFolds_stratified <- function(ID = NULL, groups = NULL, k = 5, returnTrain=TRUE)
{
    # df_all <- data.frame(ID = AMP_obj2$ID, gr = AMP_obj2$gr)
    df_all <- data.frame(ID = ID, gr = groups)
    createFolds_strat(df_all, k = k, returnTrain = returnTrain)
}




# Apskaičiuoti Komponenų amplitudes matricų daugybos būdu ------------------------------
#
#' Calculate component amplitudes (a.k.a scores) by matrix multiplication
#'
#' @details
#'  \deqn{k_amp = y * k_sp * inv(k_sp' * k_sp)}
#'
#'  formula is taken  and adapted from [1]
#' @references [1] M. Brydegaard et al. IEEE Photonics J 2011:3(3);406-21.
#'
#' @param y - matrix with experimental spectra
#' @param k_sp - matrix with components' spectra
#'
#' @return k_amp - amplitudes of the components
#' @examples
#' # e.g.:
#'     y = Object
#'     k_sp = loadings
#'
#' getScores(y, k_sp)
#'
#' @export
#'
#' @import hyperSpec
#'
getScores <- function(y, k_sp)
{
    y2 <- hy2mat(y)
    k_sp2 <- hy2mat(k_sp)

    if (dim(y2)[2] == dim(k_sp2)[2])   k_sp2 <- t(k_sp2)

    k_amp <- y2 %*% (k_sp2 %*% solve(crossprod(k_sp2)))

    if (class(y) == "hyperSpec"){
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Komponentų amplitudes (išrikiuotas) paverčiam į "HyperSpec"" objektą

        k_amp <- decomposition(y, k_amp,
                               label.wavelength = "Komponentai",
                               label.spc = "Amplitudė, a.u.")
        # Suteikiam pavadinimus
        if("kNames" %in% colnames(k_sp)){
            kNames <- gsub("max: ","k_", k_sp$kNames)
        }else {kNames <- paste0("Nr", 1:min(dim(k_sp2)))}


        colnames(k_amp$spc) <- kNames
    }
    # ======================================================================
    return(k_amp)
}

# GaussAmp ------------------------------------------------------------------
#
# Funkcija GaussAmp skirta vienai ar kelioms gausinėms kreivėms braižyti.
#
# *PARAMETRAI:*
# x  - x ašies reikšmių vektorius;
# xc - vektorius su centro padėtimis;
# w  - vektorius su vidutiniais kvadratiniais nuokrypiais, sigma;
# A  - vektorius su amplitudėmis;
# y0 - kreivės pagrindo aukštis virš x ašies (konstanta, vienoda visoms
#      kreivėms).
#
# *IŠVESTIS:*
# k_sp - matrica su kreivių reikšmėmis ties atitinkamais x.
#
# *SINTAKSĖ:*
#        GaussAmp; # Funkcijos demonstracija
# k_sp = GaussAmp(x,xc,w,A)
# k_sp = GaussAmp(x,xc,w,A,y0)
#
#
# Autorius Ignas Čiplys       2014-10-28
# Modifikavo Vilmantas Gėgžna 2014-12-03

#' Generate Gausian curve(s) (GaussAmp)
#'
#' @param x vector of x values
#' @param xc vector with centers of Gaussian curves
#' @param w  vector with parameter w, which determines the width of Gaussian curves
#' @param A  vector with Amplitudes of Gaussian curves
#' @param y0 vector with offsets on y axis
#'
#' @note The number of curves is determined by maximal length of any of 4
#' Gausian curve parameters' (xc, w, A, y0) vector. Other parameters are
#' recycled as shown in example 2 (parameter "A")
#'
#' @return y values of Gaussian curve
#' @export
#'
#' @examples
#' # Example 1
#' x <- seq(-9.9, 10, 0.2)
#' y <- GaussAmp(x)
#'
#' plot(x,y, type = "l", col = "green3"); grid()
#'
#' # Example 2
#'
#' require(hyperSpec)
#'
#' # Make 7 lines
#' y <- GaussAmp(x, xc = 1:7,A = c(1,2))
#'
#' dim(y)
#' ##[1]   7 100
#'
#' Obj <- new("hyperSpec",spc = y,    wavelength = x,
#'          label = list (spc = "y", .wavelength = "x"))
#' plot(Obj, col = 1:nrow(Obj)); grid()
#'
GaussAmp <- function(x, xc = 0, w = 1, A = 1, y0 = 0){
    P <- max(length(xc),length(w),length(A),length(y0))

    xc <- rep_len(xc, P)
    w  <- rep_len(w,  P)
    A  <- rep_len(A,  P)
    y0 <- rep_len(y0, P)
    # Prealocate y
    y = matrix(NA ,P,length(x))
    # Generate the curves
    for (i in 1:P){ y[i,] <- y0[i]+A[i]*exp(-(((x-xc[i])^2)/(2*w[i]^2)))}

    ## Output
    return(y)
}

# Regular expression (named tokens) ------------------------------------------------------------------
#' Capture substrings to dataframe (regular expression)
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
#'   \item \bold{expr-to-capture} - regular expression to be captured as a value of a variable.
#'   \item \bold{expr1, expr2} - (optional) expressions, that must match, but that are
#'   not captured.
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
#' @importFrom tidyr %>%
#'
#'
regexpr2df <- function(strings, pattern)
{
    ParsedData <- gregexpr(pattern,strings, perl = TRUE);
    as_a_list  <- regcapturedmatches(strings,ParsedData)
    df <- do.call(rbind.data.frame, as_a_list)
    return(df)
}
# list.functions -------------------------------------------------------
#' List all functions in a package.
#'
#' List all functions in a package.
#'
#' @param Package - name of package. Default \code{Package = "spHelper"}
#' @param print.table - print the result as a table using \code{\link{Pander}}.
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
