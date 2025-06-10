rm(list = ls())
graphics.off()

library(vegan)
library(MASS)
library(svglite)

# load the pollen data
poln.full <- read.csv("./data_input/20200722_pollen.csv")

# linearly interpolated data:
lake.full <- read.csv("./data_output/20200722_lake_linterp.csv")
char.full <- read.csv("./data_output/20200722_char_linterp.csv")

# split the loaded data to mix and match as needed
age <- lake.full[,"age"] # everything should have the same ages
char <- char.full[,"charcoal"]
lake <- -1 * lake.full[,"lake"]
poln <- poln.full[,c("Poaceae", "Podocarpus", "miombo", "Olea")]
astr <- poln.full[,"Asteraceae"]

# a data.frame of data to be used in the vector fit
to.be.fit <- data.frame("age" = age,
                        "charcoal" = char,
                        "lake" = lake,
                        "asters" = astr)

# scale and center the datasets
poln.std <- vegan::decostand(x = poln,
                             method = "chi.square") # approximates chisquare distance
to.be.fit.std <- scale(x = to.be.fit)

# make distance matrices
poln.dist <- dist(poln.std)

# make the pollen ordination
poln.ord <- cmdscale(d = poln.dist, k = 4, eig = T, add = F, x.ret = F)
write.table(x = data.frame("age" = poln.full$Pollen.Age,
                           poln, 
                           "PCoA_Component" = poln.ord$points), 
            file = "./data_output/pollen_ordination.csv", 
            row.names = F,
            sep = ",")

# get the proportion of the variation explained by each component
var.expl <- poln.ord$eig[1:3] / sum(poln.ord$eig[1:3])

##########################################
# do the vector fit(s) of the data to our ordination

poln.fit <- vegan::envfit(ord = poln.ord,
                          env = poln.std,
                          permutations = 10000)
add.fit <- vegan::envfit(ord = poln.ord,
                         env = to.be.fit.std,
                         permutations = 10000)

##########################################
# plot the results of the vector fits

## color ages > 85 in red
col <- c("gray", "red")[1 + (age >= 85)]

main <- "PCoA of Pollen: pollen and independent vectors fitted onto ordination"
plot(poln.ord$points, pch = 21, type = "p",
     xlab = paste0("PCoA I (", round(var.expl[1] * 100, 1), "%)"),
     ylab = paste0("PCoA II (", round(var.expl[2] * 100, 1), "%)"), main = main,
     col = "black", bg = col, cex = 1.5, xaxt = "n", yaxt = "n", bty = "n")

axis(1, at = seq(-0.5, 1, by = 0.25))
axis(2, at = seq(-1, 0.25, by = 0.25))
legend(0.5, -0.85, 
       pch = c(21, 21, NA), 
       lty = c(NA, NA, 1), 
       lwd = c(NA, NA, 2), 
       col = c("black", "black", "light blue"), 
       pt.bg =  c("gray","red", NA), 
       cex = 1.25, 
       legend = c("Post 85k", "Pre 85k", "Lake Level"))

## ordination surfaces
lake.surf <- vegan::ordisurf(x = poln.ord, y = lake,
                             main = "Lake Level", 
                             plot = FALSE, 
                             col = "orange", 
                             nlevels = 50)
plot(lake.surf, add = T, col = "light blue", nlevels = 50, knots = 30)
plot(poln.fit, col = "dark green", add = T, lwd = 3)
plot(add.fit, col = "orange", add = T)
text(poln.ord$points, labels = round(age), cex = 0.5, col = col)

##########################################
# MANOVA

# our raw data are the pollen counts' PCoA scores and lake level
# EOC: we use the n-1 column of scores of the PCoA, because the nth eigenvalue is 0, and the scores are redundant
data.raw <- data.frame(poln.ord$points[,1:3], lake)

# scale the raw data
data.scl <- scale(data.raw)

# binary indicator of age
age.gt.85 <- 0 + (age > 85)

# perform the MANOVA
poln.manova <- vegan::adonis(formula = data.raw ~ age.gt.85,
                             method = "euclidean", 
                             permutations = 10000)

poln.manova

####################################
#### plot components separately ####
####################################

## Title and axes
svglite::svglite(file = "./vector_graphics/axes.svg")

main <- "PCoA of Pollen: pollen and independent vectors fitted onto ordination"
plot(poln.ord$points, pch = 21, type = "n",
     xlab = paste0("PCoA I (", round(var.expl[1] * 100, 1), "%)"),
     ylab = paste0("PCoA II (", round(var.expl[2] * 100, 1), "%)"), main = main,
     col = "black", bg = col, cex = 1.5, xaxt = "n", yaxt = "n", bty = "n")

axis(1, at = seq(-0.5, 1, by = 0.25))
axis(2, at = seq(-1, 0.25, by = 0.25))
dev.off()

## Legend
svglite::svglite(file = "./vector_graphics/legend.svg")

plot(x = 0, y = 0, type = "n", 
     xlab = NA, ylab = NA, 
     xlim = c(-1,1), ylim = c(-1,1),
     axes = F)
legend(-1, 1, 
       pch = c(21, 21, NA), 
       lty = c(NA, NA, 1), 
       lwd = c(NA, NA, 2), 
       col = c("black", "black", "light blue"), 
       pt.bg =  c("gray","red", NA), 
       cex = 1.5, 
       legend = c("Post 85k", "Pre 85k", "Lake Level"))
dev.off()

## Ordination surface
svglite::svglite(file = "./vector_graphics/ordination_surface.svg")

plot(poln.ord$points, type = "n",
     xlab = NA, ylab = NA,
     axes = F)
lake.surf <- vegan::ordisurf(x = poln.ord, y = lake,
                             main = "Lake Level", 
                             plot = FALSE, 
                             col = "light blue", 
                             nlevels = 50)
plot(lake.surf, add = T, col = "light blue", nlevels = 50, knots = 30)
dev.off()

## Pollen vector fit
### with labels
svglite::svglite(file = "./vector_graphics/pollen_vec_fit.svg")

plot(poln.ord$points, type = "n",
     xlab = NA, ylab = NA,
     axes = F)
plot(poln.fit, col = "dark green", add = T, lwd = 3)
dev.off()

### without labels
svglite::svglite(file = "./vector_graphics/pollen_vec_fit_noLabels.svg")

plot(poln.ord$points, type = "n",
     xlab = NA, ylab = NA,
     axes = F)
plot(poln.fit, col = "dark green", add = T, lwd = 3, labels = NA)
dev.off()

## Additional variable vector fits
### with labels
svglite::svglite(file = "./vector_graphics/additional_vec_fit.svg")

plot(poln.ord$points, type = "n",
     xlab = NA, ylab = NA,
     axes = F)
plot(add.fit, col = "orange", add = T)
dev.off()

### without labels
svglite::svglite(file = "./vector_graphics/additional_vec_fit_noLabels.svg")

plot(poln.ord$points, type = "n",
     xlab = NA, ylab = NA,
     axes = F)
plot(add.fit, col = "orange", add = T, labels = NA)
dev.off()

## Data points
### as ages
svglite::svglite(file = "./vector_graphics/data_pts_asAges.svg")

plot(poln.ord$points, type = "n",
     xlab = NA, ylab = NA,
     axes = F)
text(poln.ord$points, labels = round(age), cex = 0.5, col = col)
dev.off()

### as dots 1
svglite::svglite(file = "./vector_graphics/data_pts_asDots1.svg")

plot(poln.ord$points, type = "n",
     xlab = NA, ylab = NA,
     axes = F)
points(poln.ord$points, pch = 21, cex = 0.5, col = "black", bg = col)
dev.off()

### as dots 2
svglite::svglite(file = "./vector_graphics/data_pts_asDots2.svg")

plot(poln.ord$points, type = "n",
     xlab = NA, ylab = NA,
     axes = F)
points(poln.ord$points, pch = 19, cex = 0.5, col = col)
dev.off()
