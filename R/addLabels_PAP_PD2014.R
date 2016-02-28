# addLabels_... --------------------------------------------------------
#
#' [+] Add labels to "PAP_PD_2014" and transform the dataset
#'
#' Function is designed to label data collected during investigation called
#'  "PAP_PD_2014".
#'
#' Select data columns, that are not removed, and add labels to
#'  \code{\link[=hyperSpec-class]{hyperSpec}} object of "PAP_PD_2014" data.
#'
#'
#' @param sp A \code{\link[=hyperSpec-class]{hyperSpec}} object of PAP_PD_2014
#'  data, created by function \code{\link{read3csv2hy}}.
#' @param language A string, indicating a language of labels. Possible
#' entries are \code{EN} - English and \code{LT} - Lithuanian.
#' Default is \code{EN}.
#'
#' @return A labeled object with reduced number of data columns.
#'
#' @export
#'
#' @family \code{spHelper} functions for \code{hyperSpec}

addLabels_PAP_PD2014 <- function(sp, language = "EN")  {
    ColsInitial <- colnames(sp) # save initial column names

    data         = sp$..

    # colnames(sp)
    # [1] "ID"
    # [2] "Dziov"
    # [3] "taskas"
    # [4] "spID"
    # [5] "file_names"
    # [6] "file_name_with_path"
    # [7] "Date"
    # [8] "Time"
    # [9] "Integration_time"
    # [10] "Boxcar_width"
    # [11] "Temperature"
    # [12] "ID2"
    # [13] "CitoGr"
    # [14] "HistGr"
    # [15] "HibridGr"
    # [16] "spc"


    data$Time <- strptime(paste(data$Date, data$Time), "%m-%d-%Y %H:%M:%S")
    data$Date <- as.Date(data$Date,format = "%m-%d-%Y")

    # Only necessary columns are selected:
    data <- data %>%
        dplyr::mutate(fileName = file_name_with_path,
                      Integration_time = Integration_time/1e3) %>%
        dplyr::select(ID,
                      ID2,
                      spID,
                      taskas,
                      fileName,

                      Date,
                      Time,
                      Integration_time,

                      Dziov,
                      CitoGr,
                      HistGr,
                      HibridGr,
                      Boxcar_width
        )

    Object <- new('hyperSpec', spc = sp$spc, wavelength = wl(sp), data = data)

    # add Labels ------------------------------------------------------------
    Var.Names <- colnames(Object)
    Var.LabelsLT <- c("Meginio ID",
                      "Meginio ID2",
                      "Spektro ID",
                      "Tasko numeris meginy",
                      "Bylos pavadinimas",
                      "Data (Spektometre)",
                      "Registravimo laikas",
                      "Integracijos laikas",
                      "Slapias/Isdziuves",
                      "Ciltologines grupes",
                      "Histologines grupes",
                      "Hibridines grupes",
                      "Boxcar width",
                      "I, sant.vnt."

                     )

    Var.LabelsEN <- c("Specimen ID",
                      "Specimen ID2",
                      "Spectrum ID" ,
                      "Point number in a specimen",
                      "File name",
                      "Date (Spectometer)",
                      "Time of registration",
                      "Integration time",
                      "Liquid/Dry",
                      "Cytological groups",
                      "Histological groups",
                      "Hybrid groups",
                      "Boxcar width",
                      "I, units"
                    )

    labels(Object)[Var.Names] <- switch(language,
                                        LT = Var.LabelsLT,
                                        EN = Var.LabelsEN,
                                        NULL)

    # Labels, specific to Fluorescence spectra
    labels(Object, ".wavelength") <- expression(paste(lambda, ", ", nm))

    # ----------------------------------------------------------------------
    # Add `.color`: variable with colors

    Object <- hyAdd.color(Object, "HibridGr")

    # ---------------------------------------------------------------------
    ColsFinal   <- colnames(Object)
    ColsREMOVED <- ColsInitial[!(ColsInitial %in% ColsFinal)]
    if (length(ColsREMOVED) > 0 ) {
        message("These columns were removed from the `hyperSpec` object:")
        cat(ColsREMOVED, sep = '\n')
    }
    # ---------------------------------------------------------------------

    return(Object)
}
