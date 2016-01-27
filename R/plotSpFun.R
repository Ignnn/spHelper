#' spHelper: Various functions by V. Gegzna, et al.
#'
#'
#' @section Functions in spHelper:
#' addLabels_TD2009
#' createFolds_strat
#' createFolds_stratified
#' getScores
#' hy2mat
#' info_dim
#' makeFirstCapital
#' plot_kAmp
#' plot_kSp
#' plot_kSpFacets
#' PlotConfusion
#' read3csv2hy
#' regcapturedmatches
#'
#'
#' @examples
#' # List all functions in package:
#'
#' FunctionList <- unclass(lsf.str(envir = asNamespace("spHelper"), all = T))
#' pander::pander(as.data.frame(FunctionList))
#'
#' # Other things to remember
#' devtools::install('D:/Data/R/spHelper')
#' library(spHelper)
#'
#'
#' plotc(ObjectName[,,500],model = spc~Integration_time)
#'
#' @docType package
#' @name spHelper
NULL
#> NULL

# require(dplyr)
# require(tidyr)
# require(pander)
# require(hyperSpec)
# require(ggplot2)
# require(lattice)
# require(reshape2)

# Package: spHelper
# Type: Package
# Title: spHelper - VARIOUS FUNCTIONS
# Version: 0.1
# Date: 2016-01-27
# Author: Vilmantas Gegzna
# Maintainer: Vilmantas <GegeznaV@gmail.com>
#     Description: Functions to make life easier
# License: GPL-3
# LazyData: TRUE
# RoxygenNote: 5.0.1
# Depends:
#     ggplot2,
# hyperSpec
# Imports:
#     lattice,
# reshape2,
# pander,
# tidyr,
# dplyr


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


# Komponentų spektrai grupėmis (kartu) ---------------------------------------

#' Plots spectra of components (a.k.a loadings) on one plot.
#'
#' @param loadings - \code{\link{hyperSpec}} object
#' @param Title    - the title of graph
#' @param ylab     - label of ordinate axis
#'
#' @return ggplot2 object
#' @examples
#' plot_kSp(loadings)
#'
#' @export
#'
#' @import hyperSpec
#' @import ggplot2


plot_kSp <- function(loadings,
                     Title = "Komponentai (kartu)",
                      ylab = "I, sant.vnt")
{
    hyperSpec::chk.hy(loadings)

    l <- loadings
# Create names of components, if they do not exist
    if (!('kNames' %in% colnames(l))) l$kNames = rownames(l)

    l$rows = 1:nrow(l)               # Create variable with row numbers
    l <- l[,c('spc','kNames','rows')]# Rename variables

    l <- as.long.df(l)

# Define the limits
    limMIN <- ifelse(min(l$spc)>=0, 0, min(l$spc)*1.03)
    limMAX <- ifelse(max(l$spc)<=0, 0, max(l$spc)*1.03)
# Plot
    ggplot(l, aes (x = .wavelength,
                   y = spc,
                   group = rows,
                   color = kNames,
                   fill  = kNames)) +
        geom_hline(yintercept = 0, size = 1,linetype=1, alpha = .5)+
        geom_line(size=1) +
        geom_density(stat="Identity", alpha= .1) +
        theme_bw() +
        theme(legend.title=element_blank()) +
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0),
                           limits = c(limMIN, limMAX)
                           # limits = c(0, max(l$spc)*1.03)
                          # , breaks = round(seq(0, max(l$spc),length.out = 3))
                                          ) +
        ggtitle(Title) +
        xlab(labels(loadings, ".wavelength")) +
        ylab(label = ylab)
}

# Komponentų spektrai grupėmis (atskirai) ---------------------------------

#' Plots spectra of components (a.k.a loadings) in separate graphs
#'
#'
#' @param loadings - \code{\link{hyperSpec}} object
#' @param Title    - the title of graph
#' @param ylab     - label of ordinate axis
#' @param normalize - flag if plot normalized components
#' default: normalize, if needed:
#'   Below0 <- any(loadings$spc < 0)
#'   Above0 <- any(loadings$spc > 0)
#'
#'   if (Above0 - Below0):
#'   `0` - do not normalize;
#'  `+1` - normalize to max value;
#'  `-1` - normalize to min value
#'
#' @return object of class "ggplot2"
#' @examples
#' ...
#'
#' @export
#'
#' @import hyperSpec

plot_kSpFacets <- function(loadings,
                           Title = "Komponentai (atskirai)",
                            ylab = "I, sant.vnt.",
                       normalize = any(loadings$spc<0)-any(loadings$spc>0)
                       )
{


    loadings_norm <- switch(as.character(as.numeric(normalize)),
         `0` =  loadings,
         `1` =  sweep(loadings, 1, max, `/`),# `+1` normalize to max value
        `-1` = -sweep(loadings, 1, min, `/`),# `-1` normalize to min value
         stop("'normalize' is incorrect. Must be either -1, 0 or 1.")
        )

    # plot
    plot_kSp(loadings = loadings_norm,
             Title = Title,
             ylab  = ylab) +
         facet_grid(kNames ~., scales = "free")
}

# Komponentų amplitudės ---------------------------------------------------

#' Plot component amlitudes
#' @param scores - object of class \code{\link{hyperSpec}} with scores after factorisation/decomposition
#' @return object of class "ggplot"
#' @examples plot_kAmp(scores)
#' @export
#'
#' @import hyperSpec
#' @import dplyr
#' @importFrom tidyr gather
#' @import ggplot2

plot_kAmp <- function(scores, Title = "Komponentų amplitudžių skirstiniai grupėmis")
{
    hyperSpec::chk.hy(scores)

#     library(dplyr)
#     library(tidyr)

    kNames <- colnames(scores$spc)
    sc     <- scores
    AMP2   <- as.data.frame(sc$spc)
    names(AMP2) <- kNames # paste0("Komp_", names(AMP2))

    sc <-AMP2   %>%
        cbind(sc$..["gr"])   %>%
        dplyr::mutate(row = row_number())  %>%
        tidyr::gather(Komponentas,Amplitude, -gr,  -row) %>%
        dplyr::mutate(Komponentas = factor(Komponentas,sort(kNames),sort(kNames)))

    # Braižomas paveikslas
    ggplot(sc, aes(y = Amplitude, x = Komponentas, fill = gr), size = 1)+
        geom_violin(alpha=.2)  +
        #geom_point(alpha=.05,size = 2,
        #         position = position_jitterdodge(dodge.width=0.9)) +
        geom_boxplot(alpha=.6,
                     position = position_dodge(width = 0.9))     +
        facet_grid( ~ Komponentas, scales="free") +
        # labs(x="Komponento numeris")+
        ggtitle(Title) +
        theme(axis.text.x=element_blank(),
              legend.title=element_blank()) +
        geom_hline(yintercept = 0, size = .5,linetype=2, alpha = .5)
}


# Sukurti sluoksniuotus k-folds -------------------------------------------

#' Creates folds for k-fold cross-validation stratified by levels in "gr"
#'
#'
#' Observations are blocked by "ID" (i.e. observations with the same "ID" are treated as one observation and are always in the same fold)
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




# # Plot confusion matrix TMP ---------------------------------------------------
#
# library(ggplot2)
# library(plyr) # might be not needed here anyway it is a must-have package I think in R
# library(reshape2) # to "melt" your dataset
# library (scales) # it has a "rescale" function which is needed in heatmaps
# library(RColorBrewer) # for convenience of heatmap colors, it reflects your mood sometimes
# nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")
# head(nba)
# nba$Name <- with(nba, reorder(Name, PTS))
# nba.m <- melt(nba)
# nba.m <- ddply(nba.m, .(variable), transform,rescale = rescale(value))
#
#
#
#
#
# # Spalvinam įstrižainę ****************************************
# summary(nba.m)
#
# ind <- seq(1,nrow(nba.m),50+1)
#
# nba.m$rescale      <- nba.m$rescale*(-1)
# nba.m$rescale[ind] <- nba.m$rescale[ind]*(-1)
# # *************************************************************
#
# p <-
#     ggplot(nba.m, aes(variable, Name)) +
#     scale_fill_gradient2(high = "#006400", mid="#f2f6c3",
#                          low  = "#cd0000",midpoint=0)+
#     geom_tile(aes(fill = rescale),colour = "white") +
#     geom_text(aes(label=value),size=2)
# p



# Plot confusion matrix ---------------------------------------------------

#' Plots a confusion matrix, \code{PlotConfusion}
#'
#' @param conf - table, a square matrix
#' @return ...
#' @examples
#' Prediction <- sample(c("A","B","C"),1000, replace = T)
#' Reference  <- sample(c("A","B","C"),1000, replace = T)
#' conf <- table(Prediction,Reference)
#'
#'
#' PlotConfusion(conf)
#' PlotConfusion(prop.table(conf,1))
#' PlotConfusion(prop.table(conf,2))
#'
#' @export
#'
#' @importFrom reshape2 melt
#' @import ggplot2

PlotConfusion <- function(conf)
{

#     library(ggplot2)
#     library(reshape2) # to "melt" your dataset

    conf <- round(conf,2)

    conf.m <- reshape2::melt(conf)
    names(conf.m)[1:2] <- c("Actual","Predicted")
    conf.m$Actual <- factor(conf.m$Actual, levels = rev(levels(conf.m$Actual)))

    # Spalvinam įstrižainę ****************************************
    nRows <- nrow(conf.m)
    nCols <- length(unique(conf.m[,2]))
    ind <- seq(1,nRows,nCols+1)

    #     conf.m$ColValue      <- conf.m$value*(-1)
    #     conf.m$ColValue[ind] <- conf.m$ColValue[ind]*(-1)

    conf.m$ColValue      <- -1
    conf.m$ColValue[ind] <- 1
    # *************************************************************

    p <-
        ggplot(conf.m, aes(Actual, Predicted)) +
        scale_fill_gradient2(high = "#006400", mid="#f2f6c3",
                             midpoint=0,
                             low  = "#cd0000")+
        geom_tile(aes(fill = ColValue),colour = "white") +
        geom_text(aes(label=value),size=6)
    return(p)
}


# Komponenų amplitudės matricų daugybos būdu ------------------------------

#' Calculate component amplitudes (a.k.a scotes) by matrix multiplication
#'
#'  $k_amp = y * k_sp * inv(k_sp' * k_sp)$;
#'  formula is taken from [1] and adapted
#'  [1] M. Brydegaard et al. IEEE Photonics J 2011:3(3);406-21.
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


getScores <- function(y, k_sp)
{
    # require(hyperSpec)

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
# ********* hyoerSpec ***********------------------------------------------
# Convert hyperSpec object to matrix --------------------------------------

#' Extract matrix from either hyperSpec object or a matrix
#'
#' hy2mat
#'
#' @param Object - object of classes either  \code{\link{hyperSpec}} or  \code{\link{matrix}}
#' @return A matrix or an error if it's impossible to extract a matrix.
#' @examples
#' hy2mat(Object)
#'
#' @export

hy2mat <- function(Object)
{
    switch(class(Object),
           "hyperSpec" = Object$spc,
           "matrix"    = Object,
           stop('The class of the input must be either "hyperSpec" or "matrix"')
    )
}

# Read CSV spectra (from Matlab) --------------------------------------------

#' Function reads spectra, exported from Matlad to 3 CSV files.
#' Data columns, that have unique values are removed.
#'
#' @param FileName_base - base of file name (a string). See more in section "Details"
#'
#' @return Object of class \code{\link{hyperSpec}}
#'
#' @details
#' If base name is "TD_2009", then CSV files must be:
#' [1] "TD_2009 (wavelengths).csv"
#' [2] "TD_2009 (spectra).csv"
#' [3] "TD_2009 (data).csv"
#'
#' Base name can be a path, e.g.  "D:/spectra/TD_2009"
#'
#' @export
#'
#' @import hyperSpec
#' @importFrom  pander pander
#'
read3csv2hy <- function(FileName_base)
{
    # require(hyperSpec)

    requiredFiles <- paste0(FileName_base,
                            c(" (wavelengths)", " (spectra)", " (data)"),
                            '.csv')
    #  ------------------------------------------------------------------------
    fileIsFound <- basename(requiredFiles) %in% dir(dirname(requiredFiles))
    if (!all(fileIsFound))
        stop("Some of requitred files are not found in the working directory")
    #  ------------------------------------------------------------------------
    # Read wavelengths        # "(wavelengths).csv"
    wl_x   <- as.numeric(read.csv(requiredFiles[1],header = FALSE))
    # Read spectra            # "(spectra).csv"
    sp_y   <-  as.matrix(read.csv(requiredFiles[2],header = FALSE))
    # Read data               # "(data).csv"
    data   <-            read.csv(requiredFiles[3],sep = "|",
                                  na.strings = c("<netirta>","<undefined>","?")
    )
    #  ------------------------------------------------------------------------
    findNonSingles <- function(x)(length(unique(x))>1)
    findSingles    <- function(x)(length(unique(x))==1)

    #  ------------------------------------------------------------------------
    uniqueInfo <- unique(Filter(findSingles, data))
    message(pander::pander(uniqueInfo))

    #  ------------------------------------------------------------------------
    data      <- Filter(findNonSingles, data)
    if ("ID" %in% data)   data$ID   <- as.factor(data$ID)

    #  Create hyperSpec object ------------------------------------------------
    sp <- new('hyperSpec', spc = sp_y, wavelength = wl_x, data = data)
    return(sp)
    #  ------------------------------------------------------------------------
}

# addLabels_TD2009 --------------------------------------------------------

#' Add labels to "TD_2009" an transform it.
#'
#' Select data columns and add labels to \code{\link{hyperSpec}} object of
#' TD_2009 data
#'
#'
#' @param sp - \code{\link{hyperSpec}} object of TD_2009 data, created by using
#'              function \code{\link{read3csv2hy}}
#'
#' @return labeled object with reduced number of data columns
#'
#' @examples
#' addLabels_TD2009(sp)
#'
#' # hyperSpec object
#' # 1020 spectra
#' # 11 data columns
#' # 2048 data points / spectrum
#' # wavelength: paste(lambda, ", ", nm) [numeric] 348.37 348.66 ... 895.92
#' #  data:  (1020 rows x 11 columns)
#' #  1. ID: Spektro ID [factor] D1 D1 ... S9
#' #  2. spID: Mėginio ID [factor] D1__1_a1 D1__1_a2 ... S9__4_c2
#' #  3. taskas: Taško numeris mėginy [factor] 1\\a1 1\\a2 ... 4\\c2
#' #  4. fileName: Bylos pavadinimas [factor] \\TD_2009\\D\\D1\\1\\a1.txt \\TD_2009\\D\\D1\\1\\a2.txt ... \\TD_2009\\S\\S9\\4\\c2.txt
#' #  5. gr: Grupavimas (S, P, D) [factor] D D ... S
#' #  6. Boos: Boos index [integer] 18 18 ... 21
#' #  7. Safranin: Safranin index [integer] 2 2 ... 2
#' #  8. coll_1: Collagen 1, % [integer] 0 0 ... 10
#' #  9. coll_2: collagen 2, %  [integer] 80 80 ... 40
#' #  10. coll_oth: Other collagens, % [integer] 20 20 ... 50
#' #  11. spc: I, sant.vnt. [matrix2048] 15422 17841 ... 211.7
#'
#' @import hyperSpec
#' @import dplyr
#'
#' @export

addLabels_TD2009 <- function(sp)
{
    # require(hyperSpec)

    data = sp$..
    # Pasirenkami tik reikalingi stulpeliai (Pašalinami nereikalingi stulpeliai):

    data <- data %>%
        dplyr::mutate(fileName = file_name_with_path) %>%
        dplyr::select(ID,
                   spID,
                   taskas,
                   fileName,
                   gr,
                   Boos,
                   Safranin,
                   coll_1,
                   coll_2,
                   coll_oth
        )

    Object <- new('hyperSpec', spc = sp$spc,  wavelength = wl(sp), data = data)

    # add Labels ------------------------------------------------------------
    Var.Names <- colnames(Object)
    Var.Labels <- c("Spektro ID",
                    "Mėginio ID" ,
                    "Taško numeris mėginy"    ,
                    "Bylos pavadinimas"       ,
                    "Grupavimas (S, P, D)"    ,
                    "Boos index"              ,
                    "Safranin index"          ,
                    "Collagen 1, %"           ,
                    "collagen 2, % "          ,
                    "Other collagens, %"      ,
                    "I, sant.vnt."
    )
    labels(Object)[Var.Names] <- Var.Labels

    # Labels, specific to Fluorescence spectra
    labels(Object, ".wavelength") <- expression(paste(lambda, ", ", nm))

    # ----------------------------------------------------------------------
    return(Object)
}


# ****** Komp: informacijos dimensija ******* -----------------------------------

#' Calculate information dimension of a matix.
#'
#' The function calculates a measure, called "information dimension"
#'
#'
#' @param  y - data matrix (rows = observations, columns = variables)
#'
#' @return
#'  \itemize{
#'  \item{"$"}{Rounded towards positive infinitive}
#'  \item{"parameter 2"}{Stuff}
#' }
#'      info - struktūra su laukais:
#'              dim_exact: informacijos dimensijos reikšmė (nesuapvalinta).
#'                 eigval: tikrinės reikšmės
#'
#' @references [1]	R. Cangelosi and A. Goriely, Component retention in principal
#'       component analysis with application to cDNA microarray data.
#'       Biol Direct, 2, 2 (2007), \url{http://dx.doi.org/10.1186/1745-6150-2-2}
#'
#' @note
#' Prieš pradedant vykdyti operaciją, svarbus žingsnis pasirinkti tinkamą
#' normavimo buda. To nepadarius gausime klaidingą atsakymą.
#' y=sp_normuok(y,x,'1',495);
#'
#' Taip pat labai svarbus ir triukšmo lygis. Didėjant triukšmui atitinkamai
#' padidinamas maksimalus dimensijų skaičius.
#'
#'
#' @note
#' eigenvalues - Tikrines reiksmes / Singular values
#' pk - tikimybines dimensiju vertes, skirtos entropijos ivertinimui.
#' explain = pk;
#'
#' @export
#' @importFrom  lattice xyplot

info_dim <- function(y, makePlot = FALSE)
{
     eigenval   <- svd(y)$d
      explain   <- eigenval / sum(eigenval);
    exact_dim   <- prod(sapply(explain,function(x){x^-x}));
    dim         <- ceiling(exact_dim);    # % Round tovards infinitive

    output <- list(      dim   = dim,
                   exact_dim   = exact_dim,
                   explained   = explain,
                   eigenvalues = eigenval,
                   n.comp      = 1:length(explain))


  # Plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      n.comp.SHOW = 20;
      # Adjust n.comp.SHOW
      n.comp.SHOW = min(max(n.comp.SHOW,dim+5), length(eigenval))
      output$PlotExplained <- lattice::xyplot(
              log(100*explained[1:n.comp.SHOW])~n.comp[1:n.comp.SHOW],
                 data = output,
                 type = c("b","g"),
                 cex = 1.2,
                 xlim = c(0,20.5),
                 xlab = "Number of components",
                 ylab = "Explained, log %",
                 abline = list(v = output$exact_dim, lty = "dotted", col = "red"),
                 key=list(space="top",
                          lines=list(col=c("#0080ff","red"), lty=c(1,2), lwd=1),
                          text=list(c("Normalized eigenvalues","Info.dimension")),
                          columns = 2
                 )
          )
      if (showPlot)  PlotExplained
      # Output ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      return(output)
}

