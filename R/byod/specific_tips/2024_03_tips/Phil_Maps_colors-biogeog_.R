# Project -----------------------------------------------------------------

##Mapa de la filogenia
library(phytools)
library(ape)
library(mapdata)
library(raster)
library(stringr)
library(viridis)

getwd()

setwd("/Users/marco/Desktop/R script/Introduction to Data Analysis with R/Myproject/Data")

library(phytools)
tree1 <- ape::read.nexus("R/byod/2024_03_tips/data/Philo_old-tree_2024.nex")

#Laderizar el árbol hacia arriba
#tree_ladderized_up <- ladderize(tree, right = TRUE)

#Laderizar el árbol hacia abajo
tree_ladderized_down <- ladderize(tree1, right = FALSE)

#Especifica el nombre del archivo y el tamaño del gráfico
png("tree_ladderized_dow.png", width = 1800, height = 1600)

#Visualizar el árbol laderizado hacia abajo
plot(tree_ladderized_down)

#Finaliza la creación del gráfico y guarda el archivo PNG
dev.off()


#creation of the association matrix:
library(dplyr)
dft1 <- as.data.frame(tree_ladderized_down$tip.label)
colnames(dft1) <- "tip"

#clean_tree es un árbol que contiene los mismos tips que la base de datos
#clean_tree <- tree_ladderized_down para laderizar los nodos que el outgroup quede abajo
#clean_tree <- tree1 para laderizar los nodos que el outgroup quede arriba

clean_tree <- tree_ladderized_down

#phy_cord es un data.frame que contiene los mismos tips (como rownames) que el árbol
Samples <- list(tree1$tip.label)

#set your .csv as phy_coord
phy_coord <- read_csv2("R/byod/2024_03_tips/data/Philo_coord-countries.csv")


phy_coord$Y <- str_replace_all(phy_coord$Y, ",", ".")
phy_coord$X <- str_replace_all(phy_coord$X, ",", ".")
phy_coord$Y <- as.numeric(phy_coord$Y)
phy_coord$X <- as.numeric(phy_coord$X)

#Definir paleta de colores manualmente
df_cols<-data.frame(country=c("Mexico","Guatemala","Costa Rica","Panama","Bolivia","Peru","Ecuador","Colombia","Venezuela","Brazil","Guyana","French Guiana","Surinam","Trinidad & Tobago","Guadalupe","Cuba",
                              "unknown"),
                    col=c("blue","blue","blue","blue","green","green","green","green","green","green","green","green","green","red","red","red","white"))


#col=sample(magma(n=Nspp)))
phy_coord <- merge(phy_coord, df_cols, by="country")

#Create a new column in df2 to represent the order
phy_coord$order <- match(phy_coord$Taxa, dft1$tip)

#Sort df2 based on the order column
phy_coord <- phy_coord[order(phy_coord$order), ]

#Remove the order column if you no longer need it
phy_coord$order <- NULL

#switch the taxa names from taxa-column to row names
rownames(phy_coord) <- phy_coord$Taxa
phy_coord <- phy_coord[,-2]

#remove unused columns
phy_coord <- phy_coord[c(2, 3, 4)]

#set colours
cols<- setNames(phy_coord$col, clean_tree$tip.label)


#initiate plot object
obj <- phylo.to.map(clean_tree, phy_coord, database="worldHires",
                  regions=(phy_coord$country),plot=FALSE, rotate=T)


#Especifica el nombre del archivo y el tamaño del gráfico
png("Philodendron.png", width = 1800, height = 1600)

#make the plot
plot(obj, direction = "rightwards", ylim = c(-30, 40), xlim = c(-99, -10), lwd = 0.5, type = "phylogram", ftype = "off", fsize = 0.3, cex.points = c(0.1, 1.3), colors = cols, pts = FALSE)


#Finaliza la creación del gráfico y guarda el archivo PNG
dev.off()

