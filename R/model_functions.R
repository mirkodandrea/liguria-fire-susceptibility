# install.packages(c('ModelMetrics', 'Metrics', 'pROC', 'caret', 
#                   'randomForest', 'raster', 'dplyr', 'foreign', 
#                  'maptools', 'randomForest', 'rgdal', 'readxl', 
#                  'glue', 'caret', 'ModelMetrics', 'Metrics', 
#                  'pROC', 'raster', 'plyr', 'ggplot2', 'sp', 'plyr', 'raster','e1071'))


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
library(abind)

library(e1071)

g <- glue


#create a class to wrap results of experiments 
experiment <- setClass("experiment", 
slots=list(
 models="list",
 auc="numeric",
 tr="list",
 ts="list",
 raster="RasterLayer",
 season="numeric",
 nfolds="numeric",
 columns="character",
 name="character"
))




build_dataset <- function(points_df, fires_df, season, year_from, year_to) {
  # selects fires by year and season, 
  # extract all the potential absences from the dataset exluding all the fires
  # creates a dataset from points adding the column fire accordingly
  
  sel_fires_df <- fires_df
  season_fires_df <- sel_fires_df %>% 
                     subset(.$stagione == season)
  
  sel_fires_df <- season_fires_df %>% 
                  subset(.$anno >= year_from) %>% 
                  subset(.$anno < year_to)
  
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

create_folds <- function(dataset, nfolds){
  unique_boxes <- dataset$box %>% unique
  n_boxes <- length(unique_boxes)
  shuffle_unique_boxes <- sample(unique_boxes, n_boxes, replace = F)
  folds <- c()
  for (f in seq(1, n_boxes, nfolds)) {
    new_fold <- shuffle_unique_boxes[f:(f + nfolds-1)]
    folds <- rbind(folds, new_fold)
  }
  return(folds)
}

split_test_train <- function(dataset, columns, fold){
  TS_dataset <- dataset %>% subset((.$box %in% fold)) 
  TS_dataset <- TS_dataset %>% as.data.frame
  TS_dataset <- TS_dataset[columns]
  
  TR_dataset <- dataset %>% subset(!(.$box %in% fold))
  TR_dataset <- TR_dataset %>% as.data.frame
  TR_dataset <- TR_dataset[columns]
  
  TR <- select_pseudo_absences(TR_dataset, TR_dataset$fire == 1)
  TS <- select_pseudo_absences(TS_dataset, TS_dataset$fire == 1)    
  
  TR$fire <- TR$fire %>% as.factor
  TS$fire <- TS$fire %>% as.factor
  
  return(list(TR, TS))
}

train_single_fold <- function(dataset, test_ratio=0.2, columns, model, ...) {
  print("single fold")
  pres_and_abs <- select_pseudo_absences(dataset %>% as.data.frame, dataset$fire == 1)
  sample <- sample.int(
    n = nrow(pres_and_abs), 
    size = floor(test_ratio*nrow(pres_and_abs)), 
    replace = F
  )
  TR <- pres_and_abs[sample, ]
  TS  <- pres_and_abs[-sample, ]

  TR$fire <- TR$fire %>% as.factor
  TS$fire <- TS$fire %>% as.factor

  TR = TR[, columns]
  TS = TS[, columns]
  
  RF <- model(fire~., TR, ...)

  pred_TS <- predict(object = RF,
                     newdata = TS[, names(TS) != "fire"],
                     type = "prob")

  TS_roc <- roc(TS$fire, pred_TS[, 2])
  TS_auc <- auc(TS_roc)
  print("AUC: {TS_auc}"%>%g)
  RFS <- c(); RFS[[1]] <- RF
  TRS <- c(); TRS[[1]] <- TR
  TSS <- c(); TSS[[1]] <- TS
  AUCS <- c(); AUCS[[1]] <- TS_auc
  
  
  return_value <- list(RFS=RFS, AUCS=AUCS, TRS=TRS, TSS=TSS)
  return(return_value)
}


train_on_folds <- function(dataset, folds, columns, model, ...) {
  list_of_RF = c()
  list_of_AUC = c()
  list_of_TR = c()
  list_of_TS = c()

  # Loop over folds
  for (col in 1:ncol(folds) ) {
    print("fold: {col}/{ncol(folds)}" %>% g)
    
    fold <- folds[, col]
    TR = 0; TS = 0;
    list[ TR, TS ] <- split_test_train(dataset, columns, fold)
    
    RF <- model(fire~., TR, ...)
    

    pred_TS <- predict(object = RF,
                       newdata = TS[, names(TS) != "fire"],
                       type = "prob")
      
    tryCatch({
      TS_roc <- roc(TS$fire, pred_TS[, 2])
      TS_auc <- auc(TS_roc)
      
      print("AUC: {TS_auc}" %>% g)
      list_of_AUC[[col]] <- TS_auc
      
    }, error=function(e) {
      print("Errore calcolando la AUC" %>% g)
      list_of_AUC[[col]] <- 0
    })
    
    
    list_of_RF[[col]]  <- RF
    list_of_TR[[col]]  <- TR
    list_of_TS[[col]]  <- TS

  }
  return_value <- list(list_of_RF, list_of_AUC, list_of_TR, list_of_TS)
  return(return_value)
}


fit_points <- function(points_df, excluded_cols, list_of_RF) {
  # sorted_indexes = sort.list(list_of_auc)
  # best_models = sorted_indexes[0:nfolds]
  factors = names(points_df) %>% subset(!. %in% excluded_cols)

  list_of_pred = c()
  for (col in 1:length(list_of_RF)) {
    print("fit: {col}/{length(list_of_RF)}" %>% g)
    
    RF <- list_of_RF[[col]]
    pred <- predict(
      object = RF, 
      newdata = points_df@data[factors], 
      type = "prob"
    )
    list_of_pred <- cbind(list_of_pred, pred[,2])
  }
  
  pred_mean <- rowMeans(list_of_pred)
  pred_mean_xy <- as.data.frame(cbind(points_df@data$x, points_df@data$y, pred_mean))
  colnames(pred_mean_xy)[1] <- "x"
  colnames(pred_mean_xy)[2] <- "y"
  colnames(pred_mean_xy)[3] <- "z"
  
  return(pred_mean_xy)
  
}

to_raster <- function(pred_mean_xy, resolution, crs) {
  e <- extent(pred_mean_xy[,1:2])
  r <- raster(e, resolution = resolution, crs = crs)
  raster <- rasterize(pred_mean_xy[, 1:2], r, pred_mean_xy[,3])
  return(raster)
}


build_and_train <- function(points_df, fires_df, excluded_cols, season, 
                            year_from, year_test, box_dimension, nfolds, model, ...){
  
  dataset <- build_dataset(points_df, fires_df, season, year_from, year_test)
  columns = names(dataset) %>% subset(!. %in% excluded_cols)
  if (nfolds > 1) {
    dataset$box <- create_spatial_grid(dataset, box_dimension)$id
    folds <- create_folds(dataset, nfolds)
    # factors <- columns[columns != "fire"]
    
    return_value <- train_on_folds(dataset, folds, columns, model, ...)
  } else {
    return_value <- train_single_fold(dataset, 0.2, columns, model, ...)
  }
  return(return_value)
}


#-------------------------------------------------------------------------------

do_experiment <- function(
  points_df, fires_df, excluded_cols, season, 
  year_from, year_test, box_dimension, nfolds,
  mtry, ntree, nodesize, name, resolution
){
  all_cols <- names(points_df)
  used_cols <- subset(all_cols, !all_cols %in% excluded_cols)
  mtry <- ceiling(sqrt(length(used_cols)))
  
  list[RFS, AUC, TR, TS] <- build_and_train(
    points_df, fires_df, excluded_cols, season, 
    year_from, year_test, box_dimension, nfolds,
    model = randomForest, 
    mtry = mtry, ntree = ntree, nodesize = nodesize,
    do.trace = T, importance = TRUE, type = "prob")
  
  f <- fit_points(points_df, excluded_cols, RFS)
  
  proj <- CRS("+proj=tmerc +lat_0=0 +lon_0=9 +k=0.9996 +x_0=1500000 +y_0=0 +ellps=intl +towgs84=-104.1,-49.1,-9.9,0.971,-2.917,0.714,-11.68 +units=m +no_defs ")
  r <- to_raster(f, resolution, crs=proj) 
  
  writeRaster(r, "output/{name}.tiff" %>% g, overwrite = TRUE)
  plot(r, main=name)
  ret <- new('experiment',
             models = RFS, auc = AUC, tr = TR, ts = TS,
             raster = r, season = season, nfolds = nfolds,
             columns = used_cols, name = name
  )
  
  return(ret)
}

