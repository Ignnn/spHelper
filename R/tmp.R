simSpectra <- function(){
     x = 300:800

     nInGr = c(50,50,50)
     nGr = 2
     N = sum(nInGr)
     nDim  = 4
     w.possile = 5:150



    # generate parameters -----------------------------------------------------

    c <- c(390, 450, 510, 535)# sort(sample(x,         nDim, replace = TRUE))    # c(390, 450, 510, 535)
    w <-      sample(w.possile, nDim, replace = TRUE)



    A = matrix(NA, nrow = N, ncol = nDim)

    # A[,1] = rnorm(N, mean = 100, sd=10)
    # A[,2] = rnorm(N, mean = 120, sd=20)
    # A[,3] = c(rnorm(nInGr[1],mean = 100, sd=10),
    #             rnorm(nInGr[2],mean = 120, sd=10),
    #             rnorm(nInGr[3],mean = 150, sd=10)
    #         )






    # Create correlated data --------------------------------------------------
    ACor <- corrVec2Mat(c(.3,-.6,.2)) # c(.3,-.6,.7,.3,.6,.1)
    det(ACor)
    A <- MASS::mvrnorm(N, Sigma = ACor , mu = rep(0,nrow(ACor)),empirical = T)
    A_mu     <- runif(nrow(ACor),50,200)
    A_delta  <- runif(nrow(ACor),5,30)

    A <- sweep(A,2,A_delta,'*')
    A <- sweep(A,2,A_mu,'+')

    Agr <- A[,3]


    A[,3] <- c(rnorm(nInGr[1],mean = 100, sd=10),
              rnorm(nInGr[2],mean = 120, sd=10),
              rnorm(nInGr[3],mean = 150, sd=10)
    )





    A <- cbind(A,rnorm(N, mean = 100, sd=30))


    # cor(A)
    #  ------------------------------------------------------------------------
    y <- matrix(NA,N,length(x))

    for (i in 1:N){
        y[i,] <- GaussAmp(x, c, w, A[i,]) %>% apply(2,sum) + rnorm(length(x),sd = runif(1,0,5))

    }
    #  ------------------------------------------------------------------------
    class = as.factor(c("N","S1","K","l")[kmeans(Agr, nGr+1)$cluster])
    group = as.factor(LETTERS[kmeans(A[,c(3)], nGr)$cluster])


    Spectra <- new ("hyperSpec",
              wavelength = x,
              spc = y,
              data = data.frame(gr = group, class = class),
              label = list (spc = "I / a.u.",
                            .wavelength = expression (lambda / nm))
    )

#     plot(Spectra,spc.nmax=200, col = Spectra$class)
#     plot(Spectra,spc.nmax=200, stacked = Spectra$gr, col = Spectra$class)


# hyperspectra objects ----------------------------------------------------


    Loadings <- sortLoadings(GaussAmp(x, c, w),sp = Spectra, sort = T)
    Scores   <- getScores(sp = Spectra,loadings = Loadings, scores = A)

plot_kAmp(Scores)
plot_kAmp(Scores,by = "class")
plot_kSp(Loadings)
plot(Spectra,spc.nmax=200, col = Spectra$class)

Spectra4 <- Spectra
Loadings4 <- Loadings
Scores4   <- Scores

devtools::use_data(Spectra4, Loadings4, Scores4, overwrite = T)

    save.image("simSpectrum4.RData")

# ERROR sortLoadings(GaussAmp(x, c, w),sp = Spectra, sort = F)    <<<<<<<<<<<<<<<<<<<<

}


