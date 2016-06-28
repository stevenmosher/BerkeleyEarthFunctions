###################  Libraries used by Functions#########
PACKAGES <- c("RSQLite","dplyr","data.table","zoo","raster","ncdf4","stringr","pacman")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(char=PACKAGES)
 