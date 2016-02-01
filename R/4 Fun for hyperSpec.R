# ~~~~~hyperSpec funkc ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ********* hyperSpec ***********------------------------------------------

# Convert hyperSpec object to matrix --------------------------------------
#
#' Extract matrix from either hyperSpec object or a matrix
#'
#' hy2mat
#'
#' @param Object - object of classes either \code{\link[hyperSpec]{hyperSpec}}
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
#' Read spectroscopic data in 3 CSV files ("data", "wavelengths" and "spectra") to hyperSpec object
#'
#' These fileas are usually exported from Matlab.
#'
#' @note Data columns, that have unique values are removed.
#'
#' @param FileName_base - base of file name (a string). See more in section "Details"
#'
#' @return Object of class \code{\link[hyperSpec]{hyperSpec}}
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
#' Add labels to "TD_2009" and transform.
#'
#' Function is designed for data from investigation, called "TD_2009".
#'
#' Select data columns, that are not removed, and add labels to
#'  \code{\link[hyperSpec]{hyperSpec}} object of "TD_2009" data.
#'
#'
#' @param sp - \code{\link[hyperSpec]{hyperSpec}} object of TD_2009 data, created by using
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
#'
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






