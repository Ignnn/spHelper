# hyAdd.Labels_TD2015 --------------------------------------------------------
#
#' [+] Add labels to spectroscopic dataset of certain experiment
#'
#' Function is designed to label data collected during investigation called "TD_2009".
#' Select data columns, that are not removed, and add labels to
#'  \code{\link[=hyperSpec-class]{hyperSpec}} object of "TD_2015" data.
#'
#' @param sp A \code{\link[=hyperSpec-class]{hyperSpec}} object of TD_2009 data,
#'             created by function \code{\link{read3csv2hy}}.
#' @param modeX Mode of x axis. A string, that indicates either "Fluorescence",
#'       "wavelength", "Raman" or "IR".
#' @param language A string, indicating a language of labels. Possible entries are
#' \code{EN} - English and \code{LT} - Lithuanian. Default is \code{EN}.
#'
#' @return A labeled object with reduced number of data columns.
#'
#' @export
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @author Vilmantas Gegzna
#'
hyAdd.Labels_TD2015 <- function(sp, modeX, language = "EN")  {
    ColsInitial <- colnames(sp) # save initial column names

    data         = sp$..

    # Only required columns are sellected:
    data <- data %>%
        dplyr::mutate(fileName   = file_name_with_path,
                      notes_med  = pastabos_med,
                      notes_tech = pastabos_tech,
                      thickness  = storis,
                      medGr_main = MedGr_pagrindines,
                      medGr_sub  = MedGr_smulkesnes,
                      sp_type    = tyrimas,
                      exp_code   = tyrimo_kodas,
                      Date       = Data,
                      point      = taskas
        ) %>%

        dplyr::select(ID,
                      spID,
                      point,
                      fileName,
                      Date,
                      exp_code,
                      sp_type,
                      medGr_main,
                      medGr_sub,
                      thickness,
                      notes_med,
                      notes_tech
        )

    Object <- new('hyperSpec', spc = sp$spc,  wavelength = wl(sp), data = data)

    # add Labels ------------------------------------------------------------
    Var.Names <- colnames(Object)

    Var.Labels <- switch(language,
           LT = c("Meginio ID",
                  "Spektro ID",
                  "Tasko numeris meginy",
                  "Bylos pavadinimas",
                  "Data",
                  "Eksperimento kodas",
                  "Spektroskopijos tipas",
                  "Medicinines grupes",
                  "Med. pogrupiai",
                  expression(list(Storis, paste(mu, "m"))),
                  "Medicinines pastabos",
                  "Technines pastabos",
                  "I, sant.vnt."),

           EN = c("Specimen ID",
                  "Spectrum ID" ,
                  "Point number in a specimen",
                  "File name",
                  "Date",
                  "Code of Experiment",
                  "Type of Spectroscopy",
                  "Medical groups",
                  "Medical subgroups",
                  expression(list(Thickness, paste(mu, "m"))),
                  "Medical notes",
                  "Technical notes",
                  "I, units"),

           stop("The value of `language` is not supported."))

    labels(Object)[Var.Names] <- Var.Labels

    # x axis labels
    Object <- hyAdd.Label.wl(Object, modeX)

     # ----------------------------------------------------------------------
    # Add column for colors
    Object <- hyAdd.color(Object,medGr_main, RColorBrewer::brewer.pal(8,"Dark2"))

    # ---------------------------------------------------------------------
    # CHECK if any columns were added or deleted
    ColsFinal   <- colnames(Object)
    message("These columns were:")
    print(listAddRm(ColsInitial, ColsFinal))
    # ---------------------------------------------------------------------

    return(Object)
}

# *************************************************************
