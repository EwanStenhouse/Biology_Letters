library(econullnetr)
hts <- read.csv(file.choose(), header = T)
abundance <- read.csv(file.choose(), header = T)
null.1 <- generate_null_net(hts[,2:24], abundance[,2:23], #generates null_model
                            sims = 1000, data.type = "names",
                            summary.type = "sum", c.samples = hts[,1],
                            r.samples = abundance[,1], prog.count = FALSE)

plot_preferences(null.1, node = "Hawfinch",  signif.level = 0.95, style = "dots", type = "counts",
                 xlab = "Number of samples", ylab = "Tree genera", p.cex = 1.5, lwd = 5)

edgelist <- generate_edgelist(null.1,
  signif.level = 0.95,
  export.null = FALSE,
  edge.cols = c("#67A9CF", "#F7F7F7", "#EF8A62"))

test <- test_interactions(null.1, signif.level = 0.95) #produces standardised error sizes
write.csv(test, "SES_UK.csv")

