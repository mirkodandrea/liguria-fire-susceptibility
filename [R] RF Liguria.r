
# install.packages(c('ModelMetrics', 'Metrics', 'pROC', 'caret', 
#                   'randomForest', 'raster', 'dplyr', 'foreign', 
#                  'maptools', 'randomForest', 'rgdal', 'readxl', 
#                  'glue', 'caret', 'ModelMetrics', 'Metrics', 
#                  'pROC', 'raster', 'plyr', 'ggplot2', 'sp', 'plyr', 'raster','e1071'))


# ModelMetrics, Metrics, pROC, caret, randomForest, raster, dplyr, foreign, maptools, randomForest, rgdal, readxl, glue, e1071, caret, ModelMetrics, Metrics, pROC, raster, plyr, ggplot2, sp, plyr, raster

library(plyr)
library(dplyr)

library(sp)
library(glue)

library(foreign)
library(maptools)
library(randomForest)
library(rgdal)
library(readxl)
library(caret)
library(ModelMetrics)
library(Metrics)
library(pROC)
library(raster)

library(lubridate)

library(magrittr)
library(gsubfn)

library(ggplot2)

library(e1071)

g <- glue



build_dataset <- function(points_df, fires_df, season, year_from, year_to) {
  # selects fires by year and season, 
  # creates a dataset from points adding the column fire accordingly
  
  sel_fires_df <- fires_df
  season_fires_df <- sel_fires_df %>% subset(.$stagione==season)

  sel_fires_df <- season_fires_df %>% subset(.$anno>=year_from) %>% subset(.$anno<year_to)
  
  fire_points <- sel_fires_df$point_index    
  presences_df <- points_df[fire_points, ]
  
  no_absence_points <- season_fires_df$point_index
  absences_df <- points_df[-no_absence_points, ]
  
  presences_df$fire = 1
  absences_df$fire = 0
  
  dataset <- rbind(presences_df, absences_df)
  dataset$fire <- dataset$fire %>% as.factor
  
  return(dataset)
}



create_spatial_grid <- function(points_df, box_dimension){
  ### define SpatialGrid object
  bb <- bbox(points_df)
  
  cs <- c(box_dimension, box_dimension) # cell size 
  cc <- bb[, 1] + (cs/2)  # cell offset
  
  cd <- ceiling(diff(t(bb))/cs)  # number of cells per direction
  grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
  
  sp_grd <- SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd)))
  # print(points_df)
  
  over_boxes <- over(points_df, sp_grd)
  
  return(over_boxes)
}

select_pseudo_absences <- function(dataset, presence_index){
  pres <- dataset %>% 
    subset(presence_index)
  
  print("Selecting {nrow(pres)} elements as pseudo-absences"%>%g)
  
  abs <- dataset %>%  
    subset(!presence_index) %>% 
    sample_n(nrow(pres))
  
  pseudo_absences_dataset <- rbind(pres, abs)
  return(pseudo_absences_dataset)
}

create_folds <- function (dataset){
  unique_boxes <- dataset$box %>% unique
  folds <- replicate(nfolds, 
                     unique_boxes %>% sample(length(.)/nfolds, replace = TRUE)
  )
  return(folds)
}

split_test_train <- function(dataset, columns, fold){
  TS_dataset <- dataset %>% subset(.$box %in% fold) 
  TS_dataset <- TS_dataset %>% as.data.frame
  TS_dataset <- TS_dataset[columns]
  
  TR_dataset <- dataset %>% subset(!(.$box %in% fold))
  TR_dataset <- TR_dataset %>% as.data.frame
  TR_dataset <- TR_dataset[columns]
  
  TR <- select_pseudo_absences(TR_dataset, TR_dataset$fire==1)
  TS <- select_pseudo_absences(TS_dataset, TS_dataset$fire==1)    
  
  TR$fire <- TR$fire %>% as.factor
  TS$fire <- TS$fire %>% as.factor
  
  return(list(TR, TS))
}

# ------------------------------------------------------------------------ #
resolution = 100
data_dir = 'data_{resolution}m'%>%g

points_df <- read.csv('{data_dir}/points.csv'%>%g, row.names="point_index")
points_df <- SpatialPointsDataFrame(points_df[c("x", "y")], points_df)
# points_df$veg_agg <- points_df$veg_agg %>%  as.factor
# points_df$veg <- points_df$veg %>% as.factor

fires_df <- read.csv('{data_dir}/fires.csv'%>%g)
fires_df <- SpatialPointsDataFrame(fires_df[c("x", "y")], fires_df)
fires_df$month = month(fires_df$data)

#----------------------------------------------------------------------------------------------------------------------

season <- 1
year_from <- 2007
year_test <- 2015
box_dimension <- 15000
mtry <- 8
ntree <- 50
nfolds <- 5


dataset <- build_dataset(points_df, fires_df, season, year_from, year_test)
dataset$box <- create_spatial_grid(dataset, box_dimension)$id
folds <- create_folds(dataset)

columns = names(dataset) %>% 
                  subset(!. %in% c("row", "col", "x", "y", "box", "veg_agg", "veg", "veg_freq"))

list_of_RF = c()
list_of_auc = c()

plot.new()
# Loop over folds
for(col in 1:ncol(folds)) {
    print("fold: {col}"%>%g)
    
    fold <- folds[, col]
    
    list[TR, TS] <- split_test_train(dataset, columns, fold)

    RF <- randomForest(fire~., TR, importance=TRUE, type="prob", mtry=mtry, ntree=ntree, do.trace=TRUE)

    pred_TS <- predict(object=RF,
                        newdata=TS[, names(TS) != "fire"],
                        type="prob")
    
    TS_roc <- roc(TS$fire, pred_TS[, 2])
    TS_auc <- auc(TS_roc)
    print("AUC: {TS_auc}"%>%g)
    plot(TS_roc, col="blue", lty=2, pty="s", add=T)

    list_of_RF[[col]] <- RF 
    list_of_auc[[col]] <- TS_auc
}

sorted_indexes = sort.list(list_of_auc)
best_models = sorted_indexes[0:nfolds]

factors <- columns[columns != "fire"]
list_of_pred = c()
for(col in best_models){
  print(col)
  RF <- list_of_RF[[col]]
  pred <- predict(object=RF, newdata=points_df@data[factors], type="prob")
  list_of_pred <- cbind(list_of_pred, pred[,2])
}

pred_mean <- rowMeans(list_of_pred)
pred_mean_xy <- points_df[c('x', 'y')]
pred_mean_xy['pred'] <- pred_mean
pred_mean_xy <- as.data.frame(cbind(points_df@data$x,points_df@data$y, pred_mean))


colnames(pred_mean_xy)[1] <-"x"
colnames(pred_mean_xy)[2] <-"y"
colnames(pred_mean_xy)[3] <-"z"
write.dbf(pred_mean_xy, "rf_pred_inverno.dbf")


e <- extent(pred_mean_xy[,1:2])
r <- raster(e, resolution=resolution)
rf_pred_mean_raster <- rasterize(pred_mean_xy[, 1:2], r, pred_mean_xy[,3])

writeRaster(rf_pred_mean_raster,"rf_pred_inverno", overwrite=TRUE)

plot(rf_pred_mean_raster)


#----------------------------------------------------------------------------------------------------------------------

season <- 2
year_from <- 2007
year_test <- 2015
box_dimension <- 15000
nfolds <- 5


dataset <- build_dataset(points_df, fires_df, season, year_from, year_test)
data_xy <- dataset[c("x", "y")]

# define a spatial grid over the dataset
over_boxes <- create_spatial_grid(dataset, box_dimension)

dataset$box <- over_boxes$id
# plot the dataset with boxes colors
plot(dataset@data$x, dataset@data$y, col=dataset@data$box)

# create the folds
unique_boxes <- dataset$box %>% unique
folds <- replicate(nfolds, 
  unique_boxes %>% sample(length(.)/nfolds, replace = TRUE)
)


# columns = c("fire", "slope","dem", "north", "east", "veg_type", "urban_d", "roads_d", "crops_d")
all_columns = names(dataset)
columns = all_columns[! all_columns %in% c("row", "col", "x", "y", "box", "veg_agg", "veg", "veg_freq")]

list_of_RF = c()
list_of_auc = c()


# Loop over folds
for(col in 1:nfolds) {
  print("fold: {col}"%>%g)
  
  fold <- folds[, col]
  
  TS_dataset <- dataset %>% subset(.$box %in% fold) 
  TS_dataset <- TS_dataset %>% as.data.frame
  TS_dataset <- TS_dataset[columns]
  
  TR_dataset <- dataset %>% subset(!(.$box %in% fold))
  TR_dataset <- TR_dataset %>% as.data.frame
  TR_dataset <- TR_dataset[columns]
  
  TR <- select_pseudo_absences(TR_dataset, TR_dataset$fire==1)
  TS <- select_pseudo_absences(TS_dataset, TS_dataset$fire==1)    
  
  TR$fire <- TR$fire %>% as.factor
  TS$fire <- TS$fire %>% as.factor
  
  #? ture random forest
  RF <- randomForest(fire~., TR, importance=TRUE, type="prob", mtry=8, ntree=1000) #do.trace=TRUE,
  #RF <- tuneRF(y=TR$fire, x=TR[-1], doBest=TRUE, ntreeTry=100, stepFactor=2, importance=TRUE, type="prob", do.trace=TRUE, ntree=150)
  
  pred_TS <- predict(object=RF,
                     newdata=TS[, names(TS) != "fire"],
                     type="prob")
  
  TS_roc <- roc(TS$fire, pred_TS[, 2])
  TS_auc <- auc(TS_roc)
  print("AUC: {TS_auc}"%>%g)
  plot(TS_roc, col="blue", lty=2, pty="s")
  
  
  
  list_of_RF[[col]] <- RF 
  list_of_auc[[col]] <- TS_auc
}

sorted_indexes = sort.list(list_of_auc)
best_models = sorted_indexes[0:(ncol(folds))]

factors <- columns[columns != "fire"]
list_of_pred = c()
for(col in best_models){
  print(col)
  RF <- list_of_RF[[col]]
  pred <- predict(object=RF, newdata=points_df@data[factors], type="prob")
  list_of_pred <- cbind(list_of_pred, pred[,2])
}

pred_mean <- rowMeans(list_of_pred)
pred_mean_xy <- points_df[c('x', 'y')]
pred_mean_xy['pred'] <- pred_mean
pred_mean_xy <- as.data.frame(cbind(points_df@data$x,points_df@data$y, pred_mean))


colnames(pred_mean_xy)[1] <-"x"
colnames(pred_mean_xy)[2] <-"y"
colnames(pred_mean_xy)[3] <-"z"
write.dbf(pred_mean_xy, "rf_pred_estate.dbf")


e <- extent(pred_mean_xy[,1:2])
r <- raster(e, resolution=resolution)
rf_pred_mean_raster <- rasterize(pred_mean_xy[, 1:2], r, pred_mean_xy[,3])

writeRaster(rf_pred_mean_raster,"rf_pred_estate.tif", overwrite=TRUE)

plot(rf_pred_mean_raster)
