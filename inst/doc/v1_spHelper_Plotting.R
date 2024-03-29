## ----options1, echo = FALSE, message = FALSE, warning = FALSE------------
knitr::opts_chunk$set(collapse = TRUE,
                      comment = "#>",
                      fig.align = 'center')
optDEF <- knitr::opts_chunk$get()

## ----Load main package, message = FALSE, warning = FALSE-----------------
library(spHelper)

## ----options2, echo = FALSE----------------------------------------------
knitr::opts_chunk$set(fig.width = 6, fig.align = 'center')

## ------------------------------------------------------------------------
data(flu, package = "hyperSpec")

qplot_sp(flu)
qplot_kSp(flu)
qplot_kSpFacets(flu)

## ----qplot_kSpFacets 1, fig.height= 4------------------------------------
qplot_sp(flu, Title = "Flu dataset", facets = TRUE)
qplot_kSpFacets(flu, Title = "Flu dataset")

## ----qplot_kSpFacets 2, fig.height= 4------------------------------------
qplot_kSpFacets(flu, Title = "Flu dataset", normalize = 1)
qplot_kSpFacets(flu, Title = "Flu dataset", normalize = FALSE)
qplot_kSpFacets(flu, Title = "Flu dataset", normalize = -1)
qplot_sp(flu, Title = "Flu dataset", normalize = 1)

## ------------------------------------------------------------------------
flu$c2 <- as.factor(flu$c)

# `qplot_sp` uses no fill by default
p <- qplot_sp(flu, Title = "Flu dataset", names.in = 'c2')

# Otherwise set parameter `filled = FALSE`
p <- qplot_kSp(flu, Title = "Flu dataset", names.in = 'c2', filled = FALSE)
p

## ------------------------------------------------------------------------
# No name
qplot_kSp(flu, Title = "Flu dataset", names.in = 'c2', legendName = FALSE)
# Automatic name
qplot_kSp(flu, Title = "Flu dataset", names.in = 'c2', legendName = TRUE)
qplot_kSp(flu, Title = "Flu dataset", names.in = 'c2', legendName = "Concentration")

## ----fig.width= 5, fig.cap = "**Fig2.** 'Plotly' example."---------------

dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
qplot(carat, price, data = dsamp, colour = clarity) %>% ggplotly()


## ---- fig.align='center',fig.width= 5, fig.cap = c("**Fig1.** Spectra of Components (_hyperSpec_ package)", "**Fig2.** Spectra of Components (_spHelper_ package)","**Fig3.** Spectra of Components (_spHelper_ and _plotly_ packages)", "**Fig4.** Amplitudes by groups (_spHelper_ package)")----
data("Loadings")

qplot_kSpFacets(Loadings)
qplot_kSpFacets(Loadings) %>% ggplotly()

## ----qplot_kAmp----------------------------------------------------------
data(Scores)

qplot_kAmp(Scores)
qplot_scores(Scores)

qplot_kAmp(Scores, by = "class")

p <- qplot_scores(Scores, add.jitter = TRUE)
p

p + theme_bw()

## ----qplolt_spStat 1-----------------------------------------------------
qplolt_spStat(chondro,clusters,mean)
qplolt_spStat(chondro,clusters,mean,All = FALSE)
qplolt_spStat(chondro,clusters,mean_sd,All = FALSE) +
    facet_grid(.~clusters) +
    nTick_x()

qplolt_spStat(chondro,clusters,median,All = FALSE, fixed.colors = FALSE)
qplolt_spStat(chondro,clusters,median, "My Title")

qplolt_spStat(chondro,clusters,mean_pm_sd) +
    facet_grid(.~clusters) +
    nTick_x()


## ----qplolt_spStat 2-----------------------------------------------------
qplolt_spStat(Spectra,gr,mean)
qplolt_spStat(Spectra,gr,mean,All = FALSE)
qplolt_spStat(Spectra,gr,mean_sd,All = FALSE) +
    facet_grid(.~gr) +
    nTick_x()

qplolt_spStat(Spectra,gr,median,All = FALSE, fixed.colors = FALSE)
qplolt_spStat(Spectra,gr,median, "My Title")

qplolt_spStat(Spectra,gr,mean_pm_sd) +
    facet_grid(.~gr) +
    nTick_x()

## ----qplot_prediction 1--------------------------------------------------
 data(Scores)
 Scores$Prediction <- sample(Scores$gr)
 Scores <- hyAdd.color(sp = Scores, by = "gr", palette = c("tan3", "green4","skyblue"))

 qplot_prediction(Scores,Prediction = "Prediction", Reference = "gr")
 qplot_prediction(Scores,Prediction = "Prediction", Reference = "gr", type = "ref")

 qplot_prediction(Scores,"Prediction","gr", type.stat = "ref", MDS = "isoMDS")

## ----qplot_prediction 2--------------------------------------------------
 sc <- Scores[,,c(1,3),wl.index = TRUE]

 qplot_prediction(sc,"Prediction","gr", type = "reference")
 qplot_prediction(sc,"Prediction","gr", type = "prediction")
 qplot_prediction(sc,"Prediction","gr", type = "prediction", type.stat = "ref")
 qplot_prediction(sc,"Prediction","gr", type = "prediction", type.stat = "ref", stat = "ellipse")

 set.seed(1)
 sc <- sample(Scores[,,c(1,2),wl.index = TRUE],50)
 ID <- rownames(sc)

## ----qplot_proximity 1---------------------------------------------------
 
 qplot_proximity(sc, "class")
 qplot_proximity(sc, "class",  plot.scatter = FALSE) + geom_text(aes(label = ID))


## ----qplot_proximity 2---------------------------------------------------
 Clusters <- as.factor(kmeans(sc,3)$cluster)
 qplot_proximity(sc, "class", stat = FALSE) + stat_chull(aes(fill = Clusters), color = NA, alpha = .2)

## ----qplot_crosstab------------------------------------------------------
# Generate data: Random guess  ============================
 N <- 1000 # number of observations

Prediction <- sample(c("A","B","C","D"), N, replace = TRUE)
Reference  <- sample(c("A", "B","C","D","E"),N, replace = TRUE)

tabl <- table(Prediction,Reference)

qplot_crosstab(tabl)
qplot_crosstab_sort(tabl,subTitle = "Columns and rows sorted by the best match")   # different order of columns and rows
qplot_crosstab0(tabl,subTitle = "Without highlighting")    # no colors
qplot_crosstab0s(tabl,subTitle = "Sorted by the best match, no highlighting")   # no colors, different order of columns and rows


## ----options 3, echo=FALSE-----------------------------------------------
# knitr::opts_chunk$set(optDEF)
knitr::opts_chunk$set(fig.width = 5, fig.show = "hold")

## ----qplot_confusion 1---------------------------------------------------
d <- 5 # number of rows/columns
Mat <- matrix(sample(0:100,d ^ 2,T),d)
colnames(Mat) <- paste0("gr",1:d)
rownames(Mat) <- colnames(Mat)
class(Mat)
qplot_confusion(Mat, subTitle = "Input is a matrix (1)")

diag(Mat)[2:3] <- c(1000,250)
qplot_confusion(Mat, subTitle = "Input is a matrix (2)")

## ----qplot_confusion 2---------------------------------------------------
set.seed(165)

N <- 1000 # number of observations

Prediction <- sample(c("A","B","C","D"),N, replace = TRUE)
Reference  <- sample(c("A", "B","C","D"),N, replace = TRUE)

# Random guess  =====================
conf <- table(Prediction,Reference)
class(conf)

qplot_confusion(conf, subTitle = "Correct by chance")

# At least 40% of the cases agree =====================
ind2 <- sample(1:N,round(N*.50))
Reference[ind2] <- Prediction[ind2]
conf2 <- table(Prediction,Reference)

qplot_confusion(conf2, subTitle = "Correct >50%")

# Most of the cases agree =============================
ind3 <- sample(1:N,round(N*.80))
Reference[ind3] <- Prediction[ind3]
conf3 <- table(Prediction,Reference)

qplot_confusion(conf3, subTitle = "Correct >80%")

## ----qplot_confusion 2B--------------------------------------------------
# Sort rows and columns by the best match ============
conf3_sorted <- sortDescOnDiag(conf2)
qplot_confusion(conf3_sorted,subTitle = "Columns and rows sorted by the best match")

## ----qplot_confusion 3---------------------------------------------------
# Proportions =========================================

qplot_confusion(conf3              , subTitle = "Counts")
qplot_confusion(prop.table(conf3),   subTitle = "Proportions (total sum = 1)")


## ----qplot_confusion 4---------------------------------------------------
# Shades: proportional ================================

qplot_confusion(conf,shades = "prop",  subTitle = "Shades: 'prop', Correct by chance");
qplot_confusion(conf,shades = "max",   subTitle = "Shades: 'max', Correct by chance")

qplot_confusion(conf2,shades = "prop", subTitle = "Shades: 'prop', Correct >50%");
qplot_confusion(conf2,shades = "max",  subTitle = "Shades: 'max', Correct >50%")

qplot_confusion(conf3,shades = "prop", subTitle = "Shades: 'prop', Correct >80%");
qplot_confusion(conf3,shades = "max",  subTitle = "Shades: 'max', Correct >80%")

## ----qplot_confusion 5---------------------------------------------------
# Shades: constant and none ===========================

qplot_confusion(conf3,shades = "const",subTitle = "Shades: constant");
qplot_confusion(conf3,shades = "none", subTitle = "Shades: none")


## ----qplot_confusion 6---------------------------------------------------
n <- round(N/6)
Prediction[sample(which(Prediction == "A"),n,replace = TRUE)] <-
    sample(c("B","C"), n,replace = TRUE)

conf4 <- table(Prediction,Reference)

qplot_confusion(conf4, subTitle = "Imbalanced class proportions")

## ----options 5-----------------------------------------------------------
knitr::opts_chunk$set(fig.width = 6, fig.show = "asis")

## ----InfoDim 1-----------------------------------------------------------
# Example 1 =============================================================
my_matrix <- matrix(rexp(2000, rate = .1), ncol = 20)
my_result <- infoDim(my_matrix)

# Investigate the result
str(my_result)
my_result$exactDim
my_result$dim

#Plot
qplot_scree(my_result) # Object of class "infoDim"


qplot_scree(my_matrix) # Object of class "matrix"
qplot_infoDim(my_matrix)


# Example 2 =============================================================

p1 <- qplot_infoDim(Spectra) # Object of class "hyperSpec"
p1

# Possition of a legend changes, if theme applied in this way:
p1 + theme_grey()

# A better way to use non-default theme:
p2 <- qplot_infoDim(Spectra, ggtheme = theme_grey())
p2

# Numbes of selected components can be indicated
p3 <- qplot_infoDim(Spectra, selected = 4)
p3
ggplotly(p3)

## ----InfoDim 5-----------------------------------------------------------
# Numbes of selected components can be indicated
p4 <- qplot_infoDim(Spectra, y.log = FALSE)

p4
ggplotly(p4)

## ----unipeak-------------------------------------------------------------
#  Example 1 -------------------------------------------------------

x     <- seq(-10,20,.1)
y0    <- GaussAmp(x, c = 0, A = 1) + GaussAmp(x, c = 10, A = 2) - .5
y0NEW <- unipeak(y0)

# Plot the results
par(mfrow = c(1,1))
plot( x, y0,    type = "l", lty = 3,
      main = "'unipeak' keeps positive part \n of highest peak only" );
lines(x, y0NEW, type = "l", lty = 1, lwd = 3);
legend("topleft", legend = c("Before","After"), lty = c(3,1))


## ----unipeak 2, fig.height=10--------------------------------------------
#  Example 2 -------------------------------------------------------

x  = seq(-10,20,.1)
y1 = (sin(x/4) + GaussAmp(x))
y2 = (2*sin(x) + sin(x/5) + GaussAmp(x, c = 5))
y  = base::rbind(y1,y2)

yNEW <- apply(y,1,unipeak)

par(mfrow = c(3,1))

# plot 1
matplot(x, t(y), type = "l", lty = 3,
        main = "A - Initial curves");
abline(h = 0)

# plot 2
matplot(x,yNEW, type = "l", lty = 1,lwd = 3,
        main = "B - Only the highest positive\n peaks per curve");
abline(h = 0)

# plot 3: both plots together
matplot(x, t(y), type = "l", lty = 3, main = "A and B together");
matlines(x,yNEW, type = "l", lty = 1,lwd = 3);
abline(h = 0)

par(mfrow = c(1,1))

## ----subt, fig.height=3--------------------------------------------------
 subt("Cars")
 ## bold("Cars")

 subt("Cars","Distance vs. speed")
 ## atop(bold("Cars"), atop(italic("Distance vs. speed")))

 # ----------------------------------------------------------------

 plot(cars[,1:2], main = "Cars")
 plot(cars[,1:2], main = subt("Cars")) # the same as in previous line
 plot(cars[,1:2], main = subt("Cars","Distance vs. speed"))
 plot(cars[,1:2], main = subt(subTitle = "Distance vs. speed"))

 # ----------------------------------------------------------------

 library(ggplot2)

 g <- qplot(mpg, wt, data = mtcars)
 g + ggtitle("Cars") # non-bold title
 g + ggtitle(subt("Cars")) # bold title
 g + ggtitle(subt("Cars","Distance vs. speed"))
 g + ggtitle(subt(subTitle = "Distance vs. speed"))


 # ----------------------------------------------------------------

 library(lattice)

 xyplot(eruptions~waiting, data = faithful)

 xyplot(eruptions~waiting, data = faithful,
  main = "Old Faithful Geyser Data")

 xyplot(eruptions~waiting, data = faithful,
  main = subt("Old Faithful Geyser Data"))

 xyplot(eruptions~waiting, data = faithful,
  main = subt("Old Faithful Geyser", "Data"))

 xyplot(eruptions~waiting, data = faithful,
  main = subt(subTitle = "Old Faithful Geyser Data"))

