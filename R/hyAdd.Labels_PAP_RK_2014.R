# hyAdd.Labels_... --------------------------------------------------------
#
#' [+] Add labels to "PAP_RK_2014" and transform the dataset
#'
#' Function is designed to label data collected during investigation called
#'  "PAP_RK_2014".
#'
#' Select data columns, that are not removed, and add labels to
#'  \code{\link[=hyperSpec-class]{hyperSpec}} object of "PAP_RK_2014" data.
#'
#'
#' @param sp A \code{\link[=hyperSpec-class]{hyperSpec}} object of PAP_RK_2014
#'  data, created by function \code{\link{read3csv2hy}}.
#' @param language A string, indicating a language of labels. Possible
#' entries are \code{EN} - English and \code{LT} - Lithuanian.
#' Default is \code{EN}.
#'
#' @return A labeled object with reduced number of data columns.
#'
#' @export
#'
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @author Vilmantas Gegzna
#'
hyAdd.Labels_PAP_RK_2014 <- function(sp, language = "EN")  {
    warning("Function is not complete yet.")

    ColsInitial <- colnames(sp) # save initial column names

    data         = sp$..

    data$Time <- strptime(paste(data$Date, data$Time), "%m-%d-%Y %H:%M:%S")
    data$Date <- as.Date(data$Date,format = "%m-%d-%Y")

    # # Only necessary columns are selected:
    # Dates       <- as.character(data$Date)
    # no_timezone <- paste(substr(Dates, 1, 19), substr(Dates, nchar(Dates) - 3, nchar(Dates)))
    # data$Date   <- strptime(no_timezone,format = "%a %b %d %H:%M:%S %Y", tz = 'Europe/Vilnius')

    data$birth_date <- as.Date(data$Gimimo_data, format = "%Y.%m.%d")
    data$age <- 2014 - lubridate::year(data$birth_date)

    data <- data %>%
        dplyr::mutate(ID          = as.factor(ID),
                      spID        = as.factor(gsub('_{1,3}','|', spID)),
                      point       = taskas,
                      exp_code    = tyrimo_kodas,
                      sp_type     = tyrimas,
                      fileName    = file_name_with_path,

                      excitation  = Zadinimas,
                      ZPV_ar      = ZPV_kiti_onkogeniniai,
                      CitoGr      = CitGr_2015,
                      HistGr      = HistGr_2015,
                      t.int       = Integration_time/1000,
                      t.int.units = as.factor(paste0("1000*", Integration_time_Units))
        ) %>%

        dplyr::select(ID,
                      # ID2,
                      spID,
                      point,
                      exp_code,
                      sp_type,
                      fileName,

                      Date,
                      Time,
                      # Electric_dark_correction,
                      t.int,
                      t.int.units,
                      excitation,

                      Dziov,
                      birth_date,
                      age,

                      CitoGr,
                      HistGr,
                      ZPV_16,
                      ZPV_18,
                      ZPV_ar,
                      p16_Ki67,
                      Boxcar_width
        )

    Object <- new('hyperSpec', spc = sp$spc, wavelength = wl(sp), data = data)

    # add Labels ------------------------------------------------------------
#     Var.Names <- colnames(Object)
#
#     Var.Labels <- switch(language,
#            LT =   c(
#                "Zadinimo spinduliuote, nm",
#                "Zadinimo spinduliuote, nm",
#                "Meginio ID",
#                "Meginio ID (2014)",
#                "Meginio ID (2015)",
#                "Spektro ID",
#                "Tasko numeris meginy",
#                "Laikas, s",
#                "Bylos aplankas",
#                "Bylos pavadinimas",
#                "Spektru registravimo data",
#                "Tyrejas",
#                "Integracijos laikas, s",
#                "Integracijos laikas (perskaiciuotas), s",
#                "Electric_dark_correction",
#                "Nonlinearity_correction",
#                "Meginiu partija",
#                "Gimimo data",
#                "Hibridine dignoze 2014",
#                "Citologine diagnoze 2014",
#                "Citologine diagnoze 2015",
#                "Histologine diagnoze 2014",
#                "Histologine diagnoze 2015",
#                "ZPV 16",
#                "ZPV 18",
#                "ZPV kiti onkogeniniai",
#                "p16/Ki67",
#                "Informacijos dimensija",
#                "InfoDim_2",
#                "Spalva",
#                "Meginio kiekis",
#                "Vienalytis",
#                "Nuosedu tipas",
#                "pastabos 2014",
#                "eil_nr_2015",
#                "I, sant.vnt."),
#
#            # EN =   c( "Specimen ID",
#            #            "Specimen ID2",
#            #            "Spectrum ID" ,
#            #            "Point number in a specimen",
#            #            "Code of Experiment",
#            #            "Type of Spectroscopy",
#            #            "File name",
#            #            "Date (Spectometer)",
#            #            "Time of Registration Beginning",
#            #            "Electric dark correction",
#            #            "Integration time, s",
#            #            "Units of Integration time",
#            #            "Excitation wavelength, nm",
#            #            "Liquid/Dry",
#            #            "Cytological groups",
#            #            "Histological groups",
#            #            "Hybrid groups",
#            #            "Boxcar width",
#            #            "I, units"),
#
# 		   stop("The value of `language` is not supported."))
#
#     labels(Object)[Var.Names] <- Var.Labels

    labels(Object, "spc") <- "I, units"

    # x axis labels
    Object <- hyAdd.Label.wl(Object, "wavelength")

    # ---------------------------------------------------------------------
	# Reorder levels correctly
    Object$CitoGr <- factor(Object$CitoGr,
    	               levels = c("IPPN", "LSIL", "HSIL"),
    	               labels = c("IPPN", "LSIL", "HSIL"))

    # Object$HistGr <- factor(Object$HistGr,
    #                    levels = c("Cervicitas", "CIN1", "CIN2", "CIN3+"),
    #                    labels = c("Cervicitas", "CIN1", "CIN2", "CIN3/CIS"))

    Object$HistGr <- factor(Object$HistGr,
                            levels = c("Norma", "Cervicitas", "CIN 1", "CIN 2", "CIN 3",      "CIN 3/CIS", "Karcinoma", "Karcinoma G3"),
                            labels = c("Normal","Cervicitis", "CIN1",  "CIN2",  "CIN3/CIS",   "CIN3/CIS",     "Cancer", "Cancer"   ))
    Object$HistGr <- droplevels(Object$HistGr)

    Object$ZPV_16 <- factor(Object$HistGr,levels = c("+", "-"),labels = c("+", "-" ))
    Object$ZPV_18 <- factor(Object$HistGr,levels = c("+", "-"),labels = c("+", "-" ))
    Object$ZPV_ar <- factor(Object$HistGr,levels = c("+", "-"),labels = c("+", "-" ))


    # ----------------------------------------------------------------------
    # Add `.color`: variable with colors
    # Object <- hyAdd.color(Object, "HistGr")
	# ---------------------------------------------------------------------
    # CHECK if any columns were added or deleted
    ColsFinal   <- colnames(Object)
    message("These columns were:")
    print(listAddRm(ColsInitial, ColsFinal))
    # ---------------------------------------------------------------------

    return(Object)
}
