# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Drop columns with only one unique value
# findNonSingles <- function(x)(length(unique(x))  > 1)
# findSingles    <- function(x)(length(unique(x)) == 1)
#
# uniqueInfo <- unique(Filter(findSingles, data))
# message("Variables with constanant values are eliminated:")
# row.names(uniqueInfo) <- c("Value_of_eliminated_variable")
# message(pander::pander(t(uniqueInfo)))
#
# data      <- Filter(findNonSingles, data)
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# gg <- ggplot(msleep, aes(bodywt, brainwt)) +
#     geom_point(na.rm = TRUE) +
#     scale_x_log10() +
#     scale_y_log10() +
#     theme_bw() +
#      annotation_logticks()
# gg
#
# ggplotly(gg)
#
#
# plot_ly(x = c(1, 2, 3, 4), y = c(1, 4, 9, 16),
#         name = "$\\alpha_{1c} = 352 \\pm 11 \\text{ km s}^{-1}$") %>%
#     add_trace(x = c(1, 2, 3, 4), y = c(0.5, 2, 4.5, 8),
#               name = "$\\beta_{1c} = 25 \\pm 11 \\text{ km s}^{-1}$") %>%
#     layout(xaxis = list(title = "$\\sqrt{(n_\\text{c}(t|{T_\\text{early}}))}$"),
#            yaxis = list(title = "$d, r \\text{ (solar radius)}$"))

