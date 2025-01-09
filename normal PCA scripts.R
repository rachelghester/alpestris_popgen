library(adegenet)
library(hierfstat)
library(reshape2)
library(scales)
library(ggplot2)
library(RColorBrewer)

newt <- read.genepop(file.choose())

x1 = tab(newt, NA.method = "mean")

pca.newts <- dudi.pca(x1, center=TRUE, scale=FALSE)

pca.newts 

pca.newts = dudi.pca(x1, center = TRUE, scannf = FALSE, scale = FALSE, nf = 2)

# Lets extract the variance associated with the top 4 PCs, so we can use them in our plots.

# first we sum all the eigenvalues
eig.total <- sum(pca.newts$eig)

# sum the variance
PC1.variance <- formatC(head(pca.newts$eig)[1]/eig.total * 100)
PC2.variance <- formatC(head(pca.newts$eig)[2]/eig.total * 100)
PC3.variance <- formatC(head(pca.newts$eig)[3]/eig.total * 100)
PC4.variance <- formatC(head(pca.newts$eig)[4]/eig.total * 100)


# Lets check that this has worked

PC1.variance 
PC2.variance 
PC3.variance

s.label(pca.newts$li)

s.class(pca.newts$li, fac=pop(newt), col=funky(15))

pca.newts

s.class(pca.newts$li, fac=pop(newt),
        col=transp(funky(15),.6),
        axesel=FALSE, cstar=0, cpoint=3)
add.scatter.eig(pca.newts$eig[1:50],3,1,2, ratio=.3)

# Create a data.frame containing individual coordinates
ind_coords = as.data.frame(pca.newts$li)

# Rename columns of dataframe
colnames(ind_coords) = c("Axis1","Axis2")

# Add a column containing individuals
ind_coords$Ind = indNames(newt)

# Add a column with the site IDs
ind_coords$Site = newt$pop

ind_coords$Site

# Calculate centroid (average) position for each population
centroid = aggregate(cbind(Axis1, Axis2) ~ Site, data = ind_coords, FUN = mean)

library(dplyr)

# Add centroid coordinates to ind_coords dataframe
ind_coords = left_join(ind_coords, centroid, by = "Site", suffix = c("",".cen"))

# Define colour palette
nb.cols <- 11
cols = c("skyblue4", "lightblue2", "dodgerblue4", "darkolivegreen", "darkkhaki", "aquamarine4", "chocolate3", "sandybrown", "hotpink3", "plum4", "thistle3")
  
  colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)


# Custom theme for ggplot2
ggtheme = theme(axis.text.y = element_text(colour="black", size = 12),
                axis.text.x = element_text(colour="black", size =12),
                axis.title = element_text(colour="black", size = 12),
                panel.border = element_rect(colour="black", fill=NA, linewidth=1),
                panel.background = element_blank(),
                plot.title = element_text(hjust=0.5, size=15) 
)

# Scatter plot axis 1 vs. 2
newtPCA <- ggplot(data = ind_coords, aes(x = Axis1, y = Axis2))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  # spider segments
  geom_segment(aes(xend = Axis1.cen, yend = Axis2.cen, colour = Site), show.legend = FALSE)+
  # points
  geom_point(aes(fill = Site), shape = 21, size = 3, show.legend = FALSE)+
  stat_ellipse(aes(x=Axis1, y=Axis2,color=Site),type = "norm")+
  # centroids
  geom_label(data = centroid, aes(label = Site, fill = Site), size = 4, show.legend = FALSE)+
  # colouring
  scale_fill_manual(values = cols)+
  scale_colour_manual(values = cols)+
  # custom labels
  ggtitle("Newt PCA 11 sites")+ 
  # custom theme
  ggtheme

newtPCA 

