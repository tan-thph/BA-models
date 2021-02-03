library(GGally)
data("diamonds")
x = diamonds[,c(1,5:10)]
ggcorr(x, nbreaks = 10, mid = "white",
       high = "light green", label = T,
       method = c("pairwise", "spearman"))
