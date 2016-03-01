## ----options, echo = FALSE, message = FALSE, warning = FALSE-------------
optDEF <- knitr::opts_chunk$get()
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

knitr::opts_chunk$set(fig.width = 6, fig.align = 'center')

## ----Load packages, message = FALSE, warning = FALSE---------------------
library(spHelper)
library(plotly)


## ----plot_kSpFacets, fig.height= 4---------------------------------------
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

## ----fig.width= 5, fig.cap = "**Fig2.** 'Plotly' example."---------------

dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
qplot(carat, price, data = dsamp, colour = clarity) %>% ggplotly()


## ---- fig.align='center',fig.width= 5, fig.cap = c("**Fig1.** Spectra of Components (_hyperSpec_ package)", "**Fig2.** Spectra of Components (_spHelper_ package)","**Fig3.** Spectra of Components (_spHelper_ and _plotly_ packages)", "**Fig4.** Amplitudes by groups (_spHelper_ package)")----
data("Loadings")

plot_kSpFacets(Loadings)
plot_kSpFacets(Loadings) %>% ggplotly()

## ------------------------------------------------------------------------
data(Scores)
plot_kAmp(Scores)

data(Scores3)
plot_kAmp(Scores3, by = "class")


## ------------------------------------------------------------------------
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


## ---- fig.height=10------------------------------------------------------
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

## ------------------------------------------------------------------------
 plot_stat(chondro,clusters,mean)
 plot_stat(chondro,clusters,mean,All = FALSE)
 plot_stat(chondro,clusters,mean_sd,All = FALSE) + facet_grid(.~clusters)

 plot_stat(chondro,clusters,median,All = FALSE, fixed.colors = FALSE)
 plot_stat(chondro,clusters,median, "My Title")


 # Use `.aggregate` in making facets, to avoid facet called "NA":

 plot_stat(chondro,clusters,mean_pm_sd) + facet_grid(.~clusters)
 plot_stat(chondro,clusters,mean_pm_sd) + facet_grid(.~.aggregate)

## ------------------------------------------------------------------------
 plot_stat(Spectra,gr,mean)
 plot_stat(Spectra,gr,mean,All = FALSE)
 plot_stat(Spectra,gr,mean_sd,All = FALSE) + facet_grid(.~gr)

 plot_stat(Spectra,gr,median,All = FALSE, fixed.colors = FALSE)
 plot_stat(Spectra,gr,median, "My Title")


 # Use `.aggregate` in making facets, to avoid facet called "NA":

 plot_stat(Spectra,gr,mean_pm_sd) + facet_grid(.~gr)
 plot_stat(Spectra,gr,mean_pm_sd) + facet_grid(.~.aggregate)

## ------------------------------------------------------------------------
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

## ---- echo=FALSE---------------------------------------------------------
# knitr::opts_chunk$set(optDEF)
# 
knitr::opts_chunk$set(fig.width = 5, fig.show = "hold")

## ------------------------------------------------------------------------
d <- 5 # number of rows/columns
Mat <- matrix(sample(0:100,d ^ 2,T),d)
colnames(Mat) <- paste0("gr",1:d)
rownames(Mat) <- colnames(Mat)
class(Mat)
plot_confusion(Mat, subTitle = "Input is a matrix (1)")

diag(Mat)[2:3] <- c(1000,250)
plot_confusion(Mat, subTitle = "Input is a matrix (2)")

## ------------------------------------------------------------------------
set.seed(165)

N <- 1000 # number of observations

Prediction <- sample(c("A","B","C","D"),N, replace = TRUE)
Reference  <- sample(c("A", "B","C","D"),N, replace = TRUE)

# Random guess  =====================
conf <- table(Prediction,Reference)
class(conf)

plot_confusion(conf, subTitle = "Correct by chance")

# At least 40% of the cases agree =====================
ind2 <- sample(1:N,round(N*.50))
Reference[ind2] <- Prediction[ind2]
conf2 <- table(Prediction,Reference)

plot_confusion(conf2, subTitle = "Correct >50%")

# Most of the cases agree =============================
ind3 <- sample(1:N,round(N*.80))
Reference[ind3] <- Prediction[ind3]
conf3 <- table(Prediction,Reference)

plot_confusion(conf3, subTitle = "Correct >80%")

## ------------------------------------------------------------------------
# Proportions =========================================

plot_confusion(conf3              , subTitle = "Counts")
# plot_confusion(prop.table(conf3),   subTitle = "Proportions (total sum = 1)")
# plot_confusion(prop.table(conf3,1), subTitle = "Proportions (row sums = 1)")
# plot_confusion(prop.table(conf3,2), subTitle = "Proportions (column sums = 1)")

## ------------------------------------------------------------------------
# Shades: proportional ================================

plot_confusion(conf,shades = "prop",  subTitle = "Shades: 'prop', Correct by chance");
plot_confusion(conf,shades = "max",   subTitle = "Shades: 'max', Correct by chance")

plot_confusion(conf2,shades = "prop", subTitle = "Shades: 'prop', Correct >50%");
plot_confusion(conf2,shades = "max",  subTitle = "Shades: 'max', Correct >50%")

plot_confusion(conf3,shades = "prop", subTitle = "Shades: 'prop', Correct >80%");
plot_confusion(conf3,shades = "max",  subTitle = "Shades: 'max', Correct >80%")

## ------------------------------------------------------------------------
# Shades: constant and none ===========================

plot_confusion(conf3,shades = "const",subTitle = "Shades: constant");
plot_confusion(conf3,shades = "none", subTitle = "Shades: none")


## ------------------------------------------------------------------------
n <- round(N/6)
Prediction[sample(which(Prediction == "A"),n,replace = TRUE)] <- 
    sample(c("B","C"), n,replace = TRUE)

conf4 <- table(Prediction,Reference)

plot_confusion(conf4, subTitle = "Imbalanced class proportions")



## ---- fig.width= 6-------------------------------------------------------
 knitr::opts_chunk$set(fig.width = 4.5, fig.show = "asis")

## ------------------------------------------------------------------------
# Example 1 ==================================================================
     my_matrix <- matrix(rexp(2000, rate = .1), ncol = 20)
     my_result <- infoDim(my_matrix)
    
     # Investigate the result
     str(my_result)
     my_result$exactDim
     my_result$dim
    
     #Plot
     plot_infoDim(my_result) # Object of class "infoDim"
     
     plot_infoDim(my_matrix) # Object of class "matrix"
     
 
 
# Example 2 ==================================================================
   
     p1 <- plot_infoDim(Spectra) # Object of class "hyperSpec"
     p1
     
     # Possition of a legend changes, if theme applied in this way:
     p1 + theme_grey() 
     
     # A better way to use non-default theme:
     p2 <- plot_infoDim(Spectra, ggtheme = theme_grey())
     p2
     
     # Numbes of selected components can be indicated
     p3 <- plot_infoDim(Spectra, selected = 4)
     p3     

## ------------------------------------------------------------------------
    # Numbes of selected components can be indicated
     p4 <- plot_infoDim(Spectra, y.log = FALSE)
     ggplotly(p4)  
   

