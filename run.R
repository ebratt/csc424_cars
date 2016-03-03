##########################
# PROBLEM 1              #
#########################
# setup
# clear the environment
rm(list=ls())

DATA_DIR <- './data'
IMAGES_DIR <- './images'
OUTPUT_DIR <- './output'

make_dir <- function(d) {
  if (file.exists(d)) unlink(d, recursive=TRUE, force=TRUE)
  dir.create(d)
}
lapply(c(IMAGES_DIR, OUTPUT_DIR),make_dir)


## function that concatenates strings (useful for directory paths)
concat <- function(x1,x2) {
  result <- paste(x1,x2,sep="")
  return(result)
}

## function that checks to see if a package is installed and,if not,installs it
## portions of this code came from http://stackoverflow.com/questions/9341635/how-can-i-check-for-installed-r-packages-before-running-install-packages
load_package <- function(x) {
  if (x %in% rownames(installed.packages())) { 
    print(concat("package already installed: ", x))
  }
  else { 
    install.packages(x) 
  }
  library(x, character.only=TRUE)
}

# get the data
data <- read.csv(concat(DATA_DIR, "/cars.tsv"), sep="\t")  # read text file 
# how many are n/a?
sum(is.na(data))
head(which(is.na(data)))
# how many are NULL?
sum(is.null(data))
# how many are blank?
length(which(data == ""))
str(data)
summary(data)
load_package("psych")
describe(data)

scale(data$Cylinders)
str(data)

# Principal Component Analysis
fit <- prcomp(data[,3:8], center=TRUE, scale=TRUE)
summary(fit)
# rotation on PC1
a1 <- fit$rotation[,1]
a1
(center <- fit$center)
(scale <- fit$scale)
dm <- as.matrix(data[3:8])
drop(scale(dm, center=center, scale=scale) %*% fit$rotation[,1])
describe(dm)
predict(fit)[,1]
plot(fit)
cor(data[3:8]$Cylinders, fit$x[,1])
plot(data[3:8]$Cylinders, fit$x[,1])
plot(fit$x[,1], rep(0, length(fit$x[,1])), abline(0,0))

# bi-directional plot
load_package('ggplot2')
models <- data[,2]
PCbiplot <- function(PC, rownames, x="PC1", y="PC2") {
  # code is modified but mostly borrowed from:
  #    http://stackoverflow.com/questions/6578355/plotting-pca-biplot-with-ggplot2
  #    posted by http://stackoverflow.com/users/577462/crayola
  # PC being a prcomp object
  data <- data.frame(obsnames=rownames, PC$x)
  plot <- ggplot(data, aes_string(x=x, y=y)) + geom_text(alpha=.4, size=3, aes(label=obsnames))
  plot <- plot + geom_hline(alpha=0.4, size=.2, yintercept=0) + geom_vline(alpha=0.4, size=.2, xintercept=0)
  datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y))
  )
  plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color="red")
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="red")
  plot
}
PCbiplot(fit, models)

# calculate and plot the weighted sum of squares
wss <- (nrow(data[,3:8])-1)*sum(apply(data[,3:8],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data[,3:8], centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis for k=3 on original data
fit3 <- kmeans(data[,3:8], 3) # 3 cluster solution
# cluster plot with ellipses
load_package("cluster")
clusplot(data[,3:8], 
         fit3$cluster, 
         color=TRUE, 
         shade=TRUE, 
         labels=4, 
         lines=0,
         main="Cluster Plot of Car Data with k=3")

# Centroid Plot against 1st 2 discriminant functions
load_package("fpc")
plotcluster(data[,3:8], 
            method="dc",
            fit3$cluster,
            main="Cluster Plot of Car Data with k=3")

# hierarchical clustering
d <- dist(data[,3:8], method="euclidean")
# Option "ward.D2" implements Ward's (1963) clustering criterion 
# (Murtagh and Legendre 2014). With the latter, the dissimilarities 
# are squared before cluster updating.
fit <- hclust(d, method="ward.D2")
load_package("sparcl")
ColorDendrogram(fit, 
                y = fit3$cluster, 
                main = "Hierarchical Clustering of Car Data", 
                xlab = "Euclidean Distance",
                sub = "with Ward D2 Clustering",
                branchlength = 50)
# draw red borders around the 5 clusters 
rect.hclust(fit, k=3, border="red")

clvecd <- as.integer(data[,"Cylinders"])
plotcluster(x=data[,3:8], 
            clvecd=fit3$cluster,
            method="dc",
            clnum=clvecd,
            main="Cluster Plot of Cars Data with k=3")

plotcluster(x=data[,3:8], 
            clvecd=clvecd,
            method="dc",
            clnum=fit3$cluster,
            main="Cluster Plot of Cars Data with k=3")

