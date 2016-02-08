# ~~~~~hyperSpec funkc ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ********* hyperSpec ***********------------------------------------------

# Convert hyperSpec object to matrix --------------------------------------
#
#' Extract matrix from either hyperSpec object or a matrix
#'
#' hy2mat
#'
#' @param Object - object of classes either \code{\link[=hyperSpec-class]{hyperSpec}}
#' or \code{\link[base]{matrix}}
#' @return A matrix or an error if it's impossible to extract a matrix.
#' @examples
#' hy2mat(Object)
#'
#' @export

hy2mat <- function(Object)
{    switch(class(Object),
            "hyperSpec" = Object$spc,
            "matrix"    = Object,
            stop('The class of the input must be either "hyperSpec" or "matrix"')
)
}

# Read 3 CSV spectra (from Matlab) to hyperSpec --------------------------------------------
#
#' [!] Read spectroscopic data from 3 CSV files ("data", "wavelengths" and "spectra") to hyperSpec object
#'
#' Read spectroscopic data from 3 CSV files ("data", "wavelengths" and
#'  "spectra") to hyperSpec object. These fileas are usually exported
#'  from Matlab.
#'
#' @note Data columns, that have unique values are removed.
#'
#' @param FileName_base - base of file name (a string). See more in section "Details"
#'
#' @return Object of class \code{\link[=hyperSpec-class]{hyperSpec}}
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
read3csv2hy <- function(FileName_base){
    # require(hyperSpec)

    requiredFiles <- paste0(FileName_base,
                            c(" (wavelengths)", " (spectra)", " (data)"),
                            '.csv')
    #  ------------------------------------------------------------------------
    fileIsFound <- basename(requiredFiles) %in% dir(dirname(requiredFiles))
    if (!all(fileIsFound))
        stop("Some of requitred files are not found in the working directory")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Read wavelengths        # "(wavelengths).csv"
    wl_x   <- as.numeric(read.csv(requiredFiles[1],header = FALSE))
    # Read spectra            # "(spectra).csv"
    sp_y   <-  as.matrix(read.csv(requiredFiles[2],header = FALSE))
    # Read data               # "(data).csv"
    data   <-            read.csv(requiredFiles[3],sep = "|",
                                  na.strings = c("<netirta>","<undefined>","?")
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    findNonSingles <- function(x)(length(unique(x))>1)
    findSingles    <- function(x)(length(unique(x))==1)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    uniqueInfo <- unique(Filter(findSingles, data))
    message("Variables with constanant values are eliminated:")
    row.names(uniqueInfo)<- c("Value_of_eliminated_variable")
    message(pander::pander(t(uniqueInfo)))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    data      <- Filter(findNonSingles, data)
    if ("ID" %in% data)   data$ID   <- as.factor(data$ID)

    #  Create hyperSpec object ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    sp <- new('hyperSpec', spc = sp_y, wavelength = wl_x, data = data)
    return(sp)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

# addLabels_TD2009 --------------------------------------------------------
#
#' [!] Add labels to "TD_2009" and transform.
#'
#' Function is designed for data from investigation, called "TD_2009".
#'
#' Select data columns, that are not removed, and add labels to
#'  \code{\link[=hyperSpec-class]{hyperSpec}} object of "TD_2009" data.
#'
#'
#' @param sp - \code{\link[=hyperSpec-class]{hyperSpec}} object of TD_2009 data, created by using
#'              function \code{\link{read3csv2hy}}
#' @param language - language of labels. Possible \code{EN} and \code{LT}. Default \code{EN}
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
#'
addLabels_TD2009 <- function(sp,language = "EN")
{
    # require(hyperSpec)
    data         = sp$..

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
    Var.LabelsLT <- c("Spektro ID",
                    "Mėginio ID",
                    "Taško numeris mėginy",
                    "Bylos pavadinimas",
                    "Grupavimas (S, P, D)",
                    "Boos indeksas",
                    "Safranin0 indeksas",
                    "Kolageno 1 kiekis, %",
                    "Kolageno 2 kiekis, % ",
                    "Kitų kolagenų kiekis, %",
                    "I, sant.vnt."
    )

    Var.LabelsEN <- c("Spectrum ID",
                      "Specimen ID" ,
                      "Point number in a specimen",
                      "File name",
                      "Groups (S, P, D)",
                      "Boos index",
                      "Safranin index",
                      "Collagen 1, %",
                      "collagen 2, % ",
                      "Other collagens, %",
                      "I, units"
    )

    labels(Object)[Var.Names] <- switch(language,
                                            LT = Var.LabelsLT,
                                            EN = Var.LabelsEN,
                                            NULL)

    # Labels, specific to Fluorescence spectra
    labels(Object, ".wavelength") <- expression(paste(lambda, ", ", nm))

    # ----------------------------------------------------------------------


    # Define colors ------------------------------------------------------
    UsedColors <- c("#377EB8","#4DAF4A","#984EA3")# RColorBrewer::brewer.pal(8,"Dark2")
                                                  # trellis.par.get("superpose.symbol")$col

    ColorNumbers <- unclass(Object$gr); # ColorNumbers[is.na(ColorNumbers)] <- nlevels(Object$gr) + 1;

    colorList  <- UsedColors[ColorNumbers]
    # Add column for colors
    Object$.color  <- colorList

    # Labels is vector with color names
    labels(Object, ".color") <- UsedColors
    # ---------------------------------------------------------------------
    return(Object)
}



# remove empty columns ----------------------------------------------------

#' Remove colums of hyperSpec object with all NA values
#'
#' @param Spectra - hyperSpec object
#'
#' @return
#' @export
#'
#' @examples
#'
#'  dropCol(Spectra)
dropCol <- function(Spectra){
        NAcols <- colSums(is.na(Spectra$..)) == nrow(Spectra)
    dropNames  <- names(NAcols)[NAcols]
       Spectra <- Spectra[,!(colnames(Spectra) %in% dropNames)]
    return(Spectra)
}


#' [!] Calculate summary statistic by group and for all data
#'
#' @param Spectra - hyperSpec object.
#' @param  by - grouping variable (either variable name of \code{Spectra},
#'              or vector (factor) of length \code{length(Spectra)}
#' @param FUN - function to apply.
#'
#' @return
#' @export
#'
#' @examples
#'
#' spStat(Spectra, by=Spectra$gr, FUN = IQR)
#'
spStat <- function(Spectra, by = gr, FUN = IQR){
    varName <- as.character(match.call()$by)
    by <- if (varName %in% colnames(Spectra)) Spectra[[,varName]] else by


    stat_by_gr  <- aggregate(Spectra, by = by, FUN)
    stat_all    <-     apply(Spectra, 2,       FUN)
    stat_all$.aggregate <- factor(".All")
    sp <- collapse(stat_by_gr, stat_all)

    sp <- dropCol(sp)
    return(sp)
}

# ***** calculate spectra and specimens by group ***** ------------------------------------------
#' [!] Calculate number and percentage of specimens and their spectra.
#'
#' Calculate number and percentage of specimens and their spectra.
#'
#' @param data - a data frame with variables, that names are denoted by \code{ID} and \code{gr}
#' @param ID -...
#' @param gr -...
#'
#' @return
#' @export
#'
#' @examples
nID_nSp <- function(data, ID, gr){

    # Function to calculate numbers and percentages specimens and their spectra
    #
    # data - a data frame with variables denoted by \code{ID} and \code{gr}
    # ID, gr   - varable names in \code{data} with variables, tad contain IDs and group names (factor levels) respectively.
    #
    #
    #
    # @examples
    #
    # nID_nSp(data, ID, gr)
    # pander::pander(nID_nSp(data, ID, gr))
    #
    # # For hyperSpec object
    # nID_nSp(Spectra$.., ID, gr)

    percents <- function(TABLE) {paste0(round(prop.table(TABLE),3)*100,"%")}

    nID <- table(unique(data[,c("ID", "gr")])[ ,"gr"]) # Number of unique medical samples per grpup
    nSp <- table(data[ ,"gr"]) # Number of spectra per group


    tbl <- rbind(nID, percents(nID),  nSp, percents(nSp))

    rownames(tbl) <- c("Number of medical specimens",
                       "Percentage of medical specimens",
                       "Number of spectra",
                       "Percentage of spectra")
    return(tbl)
}

