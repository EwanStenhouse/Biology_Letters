uk_its2 <- read.csv(file.choose(), header=TRUE)
uk_its2$Year <- as.factor(uk_its2$Year)
summary(uk_its2)
ncol (uk_its2)
library(mvabund)
uk_spp <- mvabund(uk_its2[,7:56])  #rows of your diet matrix - makes a new data frame with just diet data
library(vegan)
library(ggplot2)
diet_uk.mds <- metaMDS(comm = uk_spp, distance = "jaccard", trymax=999, k=3, trace = FALSE, autotransform = FALSE) #makes the mds
plot(diet_uk.mds$points); text(diet_uk.mds, row.names(diet_uk.mds)) #plot the mds with sample points. Can remove outliers if needed
diet_uk.mds$points <- diet_uk.mds$points [-c(93),]
uk_its2 <- uk_its2 [-c(93),]
scrs <- scores(diet_uk.mds, display = 'sites') #keep this as site (it refers to the sample points not actual sites) even when changing the variable from site to age etc.
scrs <- cbind(as.data.frame(scrs), Site = uk_its2$Site) #your dataframe and variable e.g. here it is site, could be age, sex ect. 
cent <- aggregate(cbind(NMDS1, NMDS2) ~ Site, data = scrs, FUN = mean) #Site - refers to the variable you want to colour code - in this case it is site
segs <- merge(scrs, setNames(cent, c('Site', 'oNMDS1', 'oNMDS2')), by = 'Site', sort = FALSE)
diet.site <- ggplot(scrs, aes(x = NMDS1, y = NMDS2, colour = Site)) + scale_fill_brewer(2,  "Accent") + geom_segment(data = segs, mapping = aes(xend = oNMDS1, yend = oNMDS2)) + geom_point(data = cent, size = 5) + geom_point() + coord_fixed() + theme_bw()
diet.site #you can fiddle with the ggplot code above to change axis names, colours, labels etc. using the usual ggplot functions
diet_uk.mds$stress
ggsave(diet.site, filename = "bl_nmds.png", dpi = 300)
