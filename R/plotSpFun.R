# colCode <- unclass(ObjectName$Dziov)
# colCode[is.na(colCode)] <- max(unclass(ObjectName$Dziov),na.rm = TRUE)+1;
# UsedColors <- c("tomato2","skyblue","orange","tan","darkgreen","green")
# colorList  <- UsedColors[colCode]
#
# plotspc(ObjectName,
#         title.args=list(main = FileName_base),
#         spc.nmax = 4e3,
#         col = colorList, # 1:nrow(ObjectName),
#         stacked = ObjectName$HibridGr)
#
# legend("topright",   levels(ObjectName$Dziov), lty = 1,  col = UsedColors)
# # plotc(ObjectName[,,500],model = spc~Integration_time)


# library(devtools)
# install('D:\\Data\\R\\spHelper')

# Padaryti pirmą raidę didžiąja -------------------------------------------


makeFirstCapital <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}

# Paruošiama braižyt komponenus -------------------------------------------



# Komponentų spektrai grupėmis (kartu) ---------------------------------------
plot_kSp <- function(loadingsNMF,
                     Title = "MNF komponentai (kartu)",
                     ylab = "I, sant.vnt")
{
    hyperSpec::chk.hy(loadingsNMF)
    l <- loadingsNMF
    l$rows = as.numeric(rownames(l))
    l <- l[,c('spc','kNames','rows')]

    l <- as.long.df(l)

    ggplot(l, aes (x = .wavelength,
                   y = spc,
                   group = rows,
                   color = kNames,
                   fill  = kNames)) +
        geom_hline(yintercept = 0, size = 1,linetype=1, alpha = .5)+
        geom_line(size=1) +
        geom_density(stat="Identity", alpha= .1) +
        theme_bw() +
        theme(legend.title=element_blank()) +
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0),
                           limits = c(0, max(l$spc)*1.03)
                          # , breaks = round(seq(0, max(l$spc),length.out = 3))
                                          ) +
        ggtitle(Title) +
        xlab(labels(loadingsNMF, ".wavelength")) +
        ylab(label = ylab)
}

# Komponentų spektrai grupėmis (atskirai) ---------------------------------
plot_kSpFacets <- function(loadingsNMF, Title = "MNF komponentai (atskirai)",
                           ylab = "I (normuotas)")
{   loadingsNMF_norm <- sweep(loadingsNMF, 1, max, `/`)
    plot_kSp(loadingsNMF = loadingsNMF_norm,
             Title = Title,
             ylab = ylab) +
        facet_grid(kNames ~., scales = "free")
}

# Komponentų amplitudės ---------------------------------------------------
plot_kAmp <- function(scoresNMF)
{
    hyperSpec::chk.hy(scoresNMF)

    library(dplyr)
    library(tidyr)

    kNames <- colnames(scoresNMF$spc)
    sc     <- scoresNMF
    AMP2   <- as.data.frame(sc$spc)
    names(AMP2) <- kNames # paste0("Komp_", names(AMP2))

    sc <-AMP2   %>%
        cbind(sc$..["gr"])   %>%
        mutate(row = row_number())  %>%
        gather(Komponentas,Amplitude, -gr,  -row) %>%
        mutate(Komponentas = factor(Komponentas,sort(kNames),sort(kNames)))

    # Braižomas paveikslas
    ggplot(sc, aes(y = Amplitude, x = Komponentas, fill = gr), size = 1)+
        geom_violin(alpha=.2)  +
        #geom_point(alpha=.05,size = 2,
        #         position = position_jitterdodge(dodge.width=0.9)) +
        geom_boxplot(alpha=.6,
                     position = position_dodge(width = 0.9))     +
        facet_grid( ~ Komponentas, scales="free") +
        labs(x="Komponento numeris")+
        ggtitle("NMF komponentų amplitudžių skirstiniai grupėmis") +
        theme(axis.text.x=element_blank(),
              legend.title=element_blank()) +
        geom_hline(yintercept = 0, size = .5,linetype=2, alpha = .5)
}


# Sukurti sluoksniuotus k-folds -------------------------------------------
createFolds_strat <- function(df_all, k = 5, returnTrain=TRUE)
{    # names(df_all)
    ##   [1] "ID" "gr"
    #
    # k - number of folds

    # example:
    # # # Inputs
    # df_all <- Object$..[,c("ID","gr")]
    # nFolds  = 5
    # createFolds_strat(df_all, nFolds)


    nFolds <- k

    # get only unique values
    df <- unique(df_all)

    # Calculations
    df$Fold <- rep(NA, times = nrow(df))
    nGr     <- length(levels(as.factor(df$gr))) # NA's are not included

    df_ByGr      <- split(df, df$gr)
    n_ByGr       <- sapply(df_ByGr, nrow)
    nInFold_ByGr <- ceiling(n_ByGr / nFolds)

    # If Number of observatuions in a group is to small
    if (any(n_ByGr < nFolds))
    {   print(sprintf('nFolds = %d', nFolds))
        print(n_ByGr)
        stop("Number of UNIQUE observations in one of the groups is smaller than number of folds")
    }

    # Assign numbers of fold to each row
    # Split to folds in a stratified way by group 'gr'
    for (gr_i in 1:nGr)
    {          GrSize   <-  n_ByGr[gr_i]
    PossibleFolds   <-  rep(1:nFolds, times = nInFold_ByGr[gr_i])
    BelongsToFoldNr <-  sample(PossibleFolds,GrSize) # permute and make appropriate langth
    df_ByGr[[gr_i]]$Fold = paste0("Fold", BelongsToFoldNr)
    }

    # unsplit the dataframe: NA's removed
    df <- unsplit(df_ByGr, df$gr[!is.na(df$gr)])

    df_all  <- merge(df_all, df)
    Ind_all <- 1:nrow(df_all)
    df_all$Test_ind <- Ind_all

    Test_ind <- split(df_all$Test_ind, df_all$Fold)

    if (returnTrain == FALSE)
    {
        return(Test_ind)
    }
    else {
        Train_ind <- lapply(Test_ind, function(x){setdiff(Ind_all, x)})
        return(Train_ind)
    }
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
createFolds_stratified <- function(ID, groups, k = 5, returnTrain=TRUE){
    # df_all <- data.frame(ID = AMP_obj2$ID, gr = AMP_obj2$gr)
    df_all <- data.frame(ID = ID, gr = groups)
    createFolds_strat(df_all, k = k, returnTrain = returnTrain)
}




# # Plot confusion matrix ---------------------------------------------------
#
# library(ggplot2)
# library(plyr) # might be not needed here anyway it is a must-have package I think in R
# library(reshape2) # to "melt" your dataset
# library (scales) # it has a "rescale" function which is needed in heatmaps
# library(RColorBrewer) # for convenience of heatmap colors, it reflects your mood sometimes
# nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")
# head(nba)
# nba$Name <- with(nba, reorder(Name, PTS))
# nba.m <- melt(nba)
# nba.m <- ddply(nba.m, .(variable), transform,rescale = rescale(value))
#
#
#
#
#
# # Spalvinam įstrižainę ****************************************
# summary(nba.m)
#
# ind <- seq(1,nrow(nba.m),50+1)
#
# nba.m$rescale      <- nba.m$rescale*(-1)
# nba.m$rescale[ind] <- nba.m$rescale[ind]*(-1)
# # *************************************************************
#
# p <-
#     ggplot(nba.m, aes(variable, Name)) +
#     scale_fill_gradient2(high = "#006400", mid="#f2f6c3",
#                          low  = "#cd0000",midpoint=0)+
#     geom_tile(aes(fill = rescale),colour = "white") +
#     geom_text(aes(label=value),size=2)
# p



# Plot confusion matrix ---------------------------------------------------
PlotConfusion <- function(conf){
    #
    # PlotConfusion(prop.table(conf,1))
    # PlotConfusion(conf)
    library(ggplot2)
    library(dplyr) # might be not needed here anyway it is a must-have package I think in R
    library(reshape2) # to "melt" your dataset


    conf <- round(conf,2)

    conf.m <- melt(conf)
    names(conf.m)[1:2] <- c("Actual","Predicted")
    conf.m$Actual <- factor(conf.m$Actual, levels = rev(levels(conf.m$Actual)))
    # Spalvinam įstrižainę ****************************************
    nRows <- nrow(conf.m)
    nCols <- length(unique(conf.m[,2]))
    ind <- seq(1,nRows,nCols+1)

#     conf.m$ColValue      <- conf.m$value*(-1)
#     conf.m$ColValue[ind] <- conf.m$ColValue[ind]*(-1)

    conf.m$ColValue      <- -1
    conf.m$ColValue[ind] <- 1
    # *************************************************************

    p <-
        ggplot(conf.m, aes(Actual, Predicted)) +
        scale_fill_gradient2(high = "#006400", mid="#f2f6c3",
                             midpoint=0,
                             low  = "#cd0000")+
        geom_tile(aes(fill = ColValue),colour = "white") +
        geom_text(aes(label=value),size=6)
    return(p)
}

