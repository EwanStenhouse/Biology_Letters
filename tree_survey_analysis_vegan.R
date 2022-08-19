set.seed(123)
library(janitor);library(vegan);library(cluster);library(tidyverse);library(mvabund)
surveys <- read.csv(file.choose(), header=T, na.strings = c(""))

clean_surveys <- surveys %>%
  clean_names()
View(clean_surveys)
com <- clean_surveys[,3:29]
m_com <- as.matrix(com)
nmds <- metaMDS (comm = m_com, distance = "bray", trymax=999, k=2, trace = FALSE, autotransform = FALSE)
plot(nmds$points); text(nmds, row.names(nmds)) #plot the mds with sample points. Can remove outliers if needed
scrs <- scores(nmds, display = 'sites') #keep this as site (it refers to the sample points not actual sites) even when changing the variable from site to age etc.
scrs <- cbind(as.data.frame(scrs), survey = clean_surveys$n_c) #your dataframe and variable e.g. here it is site, could be age, sex ect. 
cent <- aggregate(cbind(NMDS1, NMDS2) ~ survey, data = scrs, FUN = mean) #survey - refers to the variable you want to colour code - in this case it is site
segs <- merge(scrs, setNames(cent, c('survey', 'oNMDS1', 'oNMDS2')), by = 'survey', sort = FALSE)
survey.site <- ggplot(scrs, aes(x = NMDS1, y = NMDS2, colour = survey)) + scale_fill_brewer(2,  "Accent") + geom_segment(data = segs, mapping = aes(xend = oNMDS1, yend = oNMDS2)) + geom_point(data = cent, size = 5) + geom_point() + coord_fixed() + theme_bw()
survey.site #you can fiddle with the ggplot code above to change axis names, colours, labels etc. using the usual ggplot functions
nmds$stress
add <- adonis2(m_com~site, 
               data = clean_surveys, 
               permutations = 999, 
               method = "bray")
add
add$`Pr(>F)`
add$R2
add
