
# INSPIRATION:
# https://www.datanovia.com/en/lessons/k-means-clustering-in-r-algorith-and-practical-examples/
# https://www.datanovia.com/en/blog/types-of-clustering-methods-overview-and-quick-start-r-code/
# https://datascienceplus.com/finding-optimal-number-of-clusters/
# https://en.wikipedia.org/wiki/Curse_of_dimensionality
# density clustering and dbscan -> http://www.sthda.com/english/wiki/wiki.php?id_contents=7940
library(factoextra)
library(NbClust)

set.seed(123)
km.res <- kmeans(text$c, 4, nstart = 25)

# f <- function(x) { m * x + c}
#
# image_draw(img_preprocessed,res = 200)
# points(x = a[1], y = a[2],col = 'red')
# points(x = b[1], y = b[2],col = 'red')
# points(1:600,f(1:600),type = 'l')
#
# library(tidyverse)
# text %>% filter(word == 'Champagne')


describe_line_through_bbox <- function(bbox) {
  # Returns m and c to describe line
  # bbox = 1 left, 2 bottom, 3 right, 4 top

  # bbox = c(47,364,156,391)

  # left_side_middle
  a <- c(bbox$bbox_left,(bbox$bbox_top + bbox$bbox_bottom)/2)

  # right_side_middle
  b <- c(bbox$bbox_right,(bbox$bbox_top + bbox$bbox_bottom)/2)

  # m is gradient = change in y / change in x
  m = (b[2] - a[2]) / (b[1] - a[1])
  c = a[2] - m * a[1]

  return(c(m, c))
}

desribe_all_lines_through_box <- function(ocr_data_df) {
  num_rows <- nrow(ocr_data_df)

  # print(clean_bbox_list[[1]])
  ocr_data_df$m <- 0
  ocr_data_df$c <- 0

  for ( i in 1:num_rows) {
    # print(colors[[i]])
    # print(clean_bbox_list[[i]])
    desc <- describe_line_through_bbox(ocr_data_df[i,])
    # print(desc)
    ocr_data_df$m[i] <- desc[1]
    ocr_data_df$c[i] <- desc[2]

  }

  return(ocr_data_df)

}


apply_cluster_analysis <- function(ocr_data_df_w_description) {
  # clusters based on m and c for bounding boxes

  db <- dbscan(ocr_data_df_w_description[,"c"],eps = 0.01, MinPts = 1, scale = T)


  ocr_data_df_w_description$cluster <- db$cluster

  return(ocr_data_df_w_description)
}



# text <- desribe_all_lines_through_box(text)
#
# dev.off()
# plot(text$m,text$c)

# library(knn)
# install.packages('knn')

### Elbow method (look at the knee)
# Elbow method for kmeans
# fviz_nbclust(text[,"c"],kmeans, method = c("wss"))
#
# km.res <- kmeans(text$c, 9, nstart = 25)
#
#
# install.packages("NbClust")
#
# text %>%
#   select(c) %>%
#   scale() %>%
#   NbClust(distance = "euclidean",
#           min.nc = 2, max.nc = 10,
#           method = "complete", index ="all")
#
#
#
# nb <- NbClust(scaled_data, diss=NULL, distance = "euclidean",
#               min.nc=2, max.nc=5, method = "kmeans",
#               index = "all", alphaBeale = 0.1)
#
#
# install.packages("sparcl")
# library(sparcl)
#
#
# # generate data
# set.seed(11)
# x <- matrix(rnorm(50*70),ncol=70)
# x[1:25,1:20] <- x[1:25,1:20]+1
# x <- scale(x, TRUE, TRUE)
# # choose tuning parameter
# km.perm <- KMeansSparseCluster.permute(x,K=2,wbounds=seq(3,7,len=15),nperms=5)
# print(km.perm)
# plot(km.perm)
# # run sparse k-means
# km.out <- KMeansSparseCluster(x,K=2,wbounds=km.perm$bestw)
# print(km.out)
# plot(km.out)
# # run sparse k-means for a range of tuning parameter values
# km.out <- KMeansSparseCluster(x,K=2,wbounds=seq(1.3,4,len=8))
# print(km.out)
# plot(km.out)
# # Run sparse k-means starting from a particular set of cluster centers
# #in the k-means algorithm.
#
#
# install.packages("DBSCAN")
# install.packages("fpc")
# library("fpc")
#
# set.seed(123)
# db <- dbscan(text[,"c"],eps = 0.01, MinPts = 1, scale = T)
#
#
# # plot(db, text[,"c"], main = "DBSCAN", frame = FALSE)
#
#
#
#
# text$cluster <- db$cluster
#
#
# ggplot(text) +
#   geom_point(aes(x = m , y = c, color = as.character(cluster)))
