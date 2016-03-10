# hyAdd.Labels_TD2009 --------------------------------------------------------
#
#' [+] Add labels to "TD_2009" dataset
#'
#' Function is designed to label data collected during investigation called "TD_2009".
#' Select data columns, that are not removed, and add labels to
#'  \code{\link[=hyperSpec-class]{hyperSpec}} object of "TD_2009" data.
#'
#' @param sp A \code{\link[=hyperSpec-class]{hyperSpec}} object of TD_2009 data,
#'             created by function \code{\link{read3csv2hy}}.
#' @param language A string, indicating a language of labels. Possible entries are
#' \code{EN} - English and \code{LT} - Lithuanian. Default is \code{EN}.
#'
#' @return A labeled object with reduced number of data columns.
#'
#' @export
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @author Vilmantas Gegzna
#'
hyAdd.Labels_TD2009 <- function(sp,language = "EN")  {
    ColsInitial <- colnames(sp) # save initial column names

    data         = sp$..

    # Only required columns are sellected:
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
    Var.LabelsLT <- c("Meginio ID",
                      "Spektro ID",
                      "Tasko numeris meginy",
                      "Bylos pavadinimas",
                      "Grupavimas (S, P, D)",
                      "Boos indeksas",
                      "Safranin0 indeksas",
                      "Kolageno 1 kiekis, %",
                      "Kolageno 2 kiekis, % ",
                      "Kitu kolagenu kiekis, %",
                      "I, sant.vnt."
    )

    Var.LabelsEN <- c("Specimen ID",
                      "Spectrum ID" ,
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
    labels(Object, ".wavelength") <- expression(list(lambda, nm))

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
    # CHECK if any columns were added or deleted
    ColsFinal   <- colnames(Object)
    print(listAddRm(ColsInitial, ColsFinal))
    # ---------------------------------------------------------------------

    return(Object)
}
# @examples
# # hyAdd.Labels_TD2009(sp)
#
# ## hyperSpec object
# ## 1020 spectra
# ## 11 data columns
# ## 2048 data points / spectrum
# ## wavelength: paste(lambda, ", ", nm) [numeric] 348.37 348.66 ... 895.92
# ##  data:  (1020 rows x 11 columns)
# ##  1. ID: Spektro ID [factor] D1 D1 ... S9
# ##  2. spID: Meginio ID [factor] D1__1_a1 D1__1_a2 ... S9__4_c2
# ##  3. taskas: Tasko numeris meginy [factor] 1\\a1 1\\a2 ... 4\\c2
# ##  4. fileName: Bylos pavadinimas [factor]
# ##             \\TD_2009\\D\\D1\\1\\a1.txt
# ##             \\TD_2009\\D\\D1\\1\\a2.txt
# ##             ...
# ##             \\TD_2009\\S\\S9\\4\\c2.txt
# ##  5. gr: Grupavimas (S, P, D) [factor] D D ... S
# ##  6. Boos: Boos index [integer] 18 18 ... 21
# ##  7. Safranin: Safranin index [integer] 2 2 ... 2
# ##  8. coll_1: Collagen 1, % [integer] 0 0 ... 10
# ##  9. coll_2: collagen 2, %  [integer] 80 80 ... 40
# ##  10. coll_oth: Other collagens, % [integer] 20 20 ... 50
# ##  11. spc: I, sant.vnt. [matrix2048] 15422 17841 ... 211.7
#
#
# *************************************************************
