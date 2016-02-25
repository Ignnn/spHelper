## ----options, echo = FALSE, message = FALSE, warning = FALSE-------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      fig.align = 'center',
                      fig.width = 5 )

## ----Load packages, message = FALSE, warning = FALSE---------------------
library(spHelper)


## ---- fig.align='center',fig.width= 5, fig.cap = c("**Fig1.** Spectra of Components (_hyperSpec_ package)", "**Fig2.** Spectra of Components (_spHelper_ package)","**Fig3.** Spectra of Components (_spHelper_ and _plotly_ packages)", "**Fig4.** Amplitudes by groups (_spHelper_ package)")----
data("Loadings")
data("Scores")

qplotspc(Loadings) 

plot_kSpFacets(Loadings)

plot_kSpFacets(Loadings) %>% ggplotly()

plot_kAmp(Scores)


## ----fig.width= 5, fig.cap = "**Fig2.** 'Plotly' example."---------------

dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
qplot(carat, price, data = dsamp, colour = clarity) %>% ggplotly()


## ----plot_kSpFacets------------------------------------------------------
library(hyperSpec)
plot_kSpFacets(flu, Title = "Flu dataset")
plot_kSpFacets(flu, Title = "Flu dataset", normalize = 1)
plot_kSpFacets(flu, Title = "Flu dataset", normalize = FALSE)
plot_kSpFacets(flu, Title = "Flu dataset", normalize = -1)


## ------------------------------------------------------------------------
flu$c2 <- as.factor(flu$c)
plot_kSp(flu, Title = "Flu dataset", names = 'c2', filled = F)

## ------------------------------------------------------------------------
plot_kSp(flu, Title = "Flu dataset", names = 'c2', legendName = FALSE)
plot_kSp(flu, Title = "Flu dataset", names = 'c2', legendName = TRUE)
plot_kSp(flu, Title = "Flu dataset", names = 'c2', legendName = "Concentration")

