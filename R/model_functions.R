#install.packages(c('ModelMetrics', 'Metrics', 'pROC', 'caret', 
#                   'randomForest', 'raster', 'dplyr', 'foreign', 
#                 'maptools', 'randomForest', 'rgdal', 'readxl', 
#                'glue', 'caret', 'ModelMetrics', 'Metrics', 
#                 'pROC', 'raster', 'plyr', 'ggplot2', 'sp', 'plyr', 'raster','e1071'))

#install.packages("RSNNS",  "randomUniformForest", "NeuralNetTools")

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

library(rminer)
library(e1071)
#library(RSNNS)
# Aggiunte  AT per  multi-algorithm
# 
library("randomUniformForest")#fatto apposta per avere alberi più decorrelati possibile, posso fare stime di come variabili una impatta l'altra
library("itertools")

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
 name="character",
 algo="character"
))

clusterDataset<- function(dataset){
  #faccio one hot encoding per veg_type----->library(caret) da istallare
  datasetTmp <- dataset#ho tolto la colonna fire per fare i cluster
  datasetTmp <- subset(datasetTmp,select=-fire)
 #datasetTmp@data[["fire"]] <- strtoi(datasetTmp@data[["fire"]])#gli dico di considerare '1','0'come numero e non fattore
  dummy <- dummyVars("~ .",data=datasetTmp)
  datasetTmp <- data.frame(predict(dummy,newdata=datasetTmp))
  #normalizzo dati
  standarDev <- apply(datasetTmp, 2, sd)
  means <- colMeans(datasetTmp)
  for(col in 1:ncol(datasetTmp) ){
    datasetTmp[,col]=(datasetTmp[,col]-means[col])/standarDev[col]
  }
  #creo i cluster
  datasetCluster <- kmeans(datasetTmp, 2, iter.max = 100000, nstart = 5,
                                   algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), 
                                   trace=FALSE)
  dataset@data$cluster <- datasetCluster$cluster
  dataset <- dataset %>% 
    subset(.@data$cluster ==1)#seleziono un solo tipo di cluster per analisi e costruzione del TR e TS set
  dataset <-subset(dataset,select=-cluster)
  return(dataset)
}
build_dataset <- function(points_df, fires_df, season, year_from, year_to) {
  # selects fires by year and season, 
  # extract all the potential absences from the dataset exluding all the fires
  # creates a dataset from points adding the column fire accordingly
  
  sel_fires_df <- fires_df
  season_fires_df <- sel_fires_df %>% 
                     subset(.$stagione == season)
  if(length(year_to)==1){
  sel_fires_df <- season_fires_df %>% 
                  subset(.$anno >= year_from) %>% 
                  subset(.$anno < year_to)
  }else{ # this is the case when the user specifies a specific vector of years  for test.
    sel_fires_df <- season_fires_df %>% 
                    subset(.$anno >= year_from) %>%
                    subset(!(.$anno %in% year_to))
  }
  
  fire_points <- sel_fires_df$point_index    
  presences_df <- points_df[fire_points, ]
  
  no_absence_points <- season_fires_df$point_index
  absences_df <- points_df[-no_absence_points, ]
  
  presences_df$fire = 1
  absences_df$fire = 0
  
  dataset <- rbind(presences_df, absences_df)
  dataset$fire <- dataset$fire %>% as.factor
  #dataset <- clusterDataset(dataset)
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
    sample_n(nrow(pres))#put 0.0001* for singleFold
  
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
  #Reduce rows
  set.seed(10)
  sample <- sample.int(
    n = nrow(dataset), 
    size = floor(0.3333*nrow(dataset)), #0.3333     0.4  Sardegna:0.2
    replace = F
  )
  dataset <- dataset[sample, ]#sample
  print('Dim dataset')
  print(dim(dataset))
  
  #
  TS_dataset <- dataset %>% subset((.$box %in% fold)) 
  TS_dataset <- TS_dataset %>% as.data.frame
  #TotClusterS <- sum(TS_dataset$cluster==3)+sum(TS_dataset$cluster==2)+sum(TS_dataset$cluster==1)
  #TS_dataset <- TS_dataset %>% subset((sum(.$cluster==3)>TotClusterS/4 && sum(.$cluster==3)<TotClusterS/3)&&
  #                                    (sum(.$cluster==1)>TotClusterS/4 && sum(.$cluster==1)<TotClusterS/3) &&
  #                                    (sum(.$cluster==2)>TotClusterS/4 && sum(.$cluster==2)<TotClusterS/3)) 
  TS_dataset <- TS_dataset[columns]
  
  TR_dataset <- dataset %>% subset(!(.$box %in% fold))
  TR_dataset <- TR_dataset %>% as.data.frame
  #TotClusterR <- sum(TR_dataset$cluster==3)+sum(TR_dataset$cluster==2)+sum(TR_dataset$cluster==1)
  #TR_dataset <- TR_dataset %>% subset((sum(.$cluster==3)>TotClusterR/4 && sum(.$cluster==3)<TotClusterR/3)&&
  #                                      (sum(.$cluster==1)>TotClusterR/4 && sum(.$cluster==1)<TotClusterR/3) &&
  #                                      (sum(.$cluster==2)>TotClusterR/4 && sum(.$cluster==2)<TotClusterR/3)) 
  TR_dataset <- TR_dataset[columns]
  
  
  TR <- select_pseudo_absences(TR_dataset, TR_dataset$fire == 1)
  TS <- select_pseudo_absences(TS_dataset, TS_dataset$fire == 1)    
  
  TR$fire <- TR$fire %>% as.factor
  TS$fire <- TS$fire %>% as.factor
  
  return(list(TR, TS))
}

train_single_fold <- function(dataset, test_ratio=0.2, columns, model,algo, ...) {
  print("single fold")
  pres_and_abs <- select_pseudo_absences(dataset %>% as.data.frame, dataset$fire == 1)
  sample <- sample.int(
    n = nrow(pres_and_abs), 
    size = floor(0.3333*nrow(pres_and_abs)), #test_ratio 0.4
    replace = F
  )
  TR <- pres_and_abs[sample, ]
  TS  <- pres_and_abs[-sample, ]

  TR$fire <- TR$fire %>% as.factor
  TS$fire <- TS$fire %>% as.factor

  TR = TR[, columns]
  TS = TS[, columns]
  
  my_target = "fire"
  my_vars   = setdiff(columns, "fire")
  if(algo == "mlp"){ 
    #Std_Backpropagation,BackpropBatch, e.g., have two parameters, the learning rate 
    #and the max-imum output difference.
    #The learning rate is usually a value between 0.1 and 1.  
    #It specifies thegradient descent step width.  The maximum difference defines, 
    #how much difference between out-put and target value is treated as zero error,  and not
    # backpropagated.  This parameter is used toprevent overtraining.
    #  For a complete list of the parameters of all the learning functions, see theSNNS User Manual, pp. 67.
    twofres = decodeClassLabels(TR[,"fire"]) #two-fold  results for mlp "Std_Backpropagation" 
    RF <- model(TR[,my_vars],twofres,    learnFunc = "BackpropBatch", size = 35, learnFuncParams=c(0.1, 0.01), maxit = 5000)
    pred_TS <- predict(object = RF,
                       newdata = TS[,my_vars], 
                       type = "prob")
  }else if(algo ==  "ruf"){ #Random Uniform Forest...
    RF <- model(TR[,my_vars],TR[,my_target], categorical = "veg_type", threads = "auto",  ...)
    #RF <- model(fire~., TR, ...)
    pred_TS <- predict(object = RF,
                       X = TS[, names(TS) != "fire"],
                       type = "prob") 
  }else if(algo ==  "svm"){ #Support vector Machine... model = rminer::fit
    #RF <- model(TR[,my_vars],TR[,my_target], categorical = "veg_type", threads = 1,  ...)
    #RF <- model(fire~., TR, ...)
    print("inside svm model generation and pred_TS computing") #ksvm
    RF <- model(fire~., TR, model="svm",task="prob")  #1.3.1 si chiama ksvm, altrimenti svm
    pred_TS <- predict(object = RF,
                       newdata = TS[, names(TS) != "fire"]) 
                       #,type = "prob") rminer  1.3.1
  }else if(algo ==  "mlp_rminer"){ 
    print("inside rminer::fit with mlp option  model generation and pred_TS computing") 
    RF <- model(fire~., TR, model="mlp",task="prob")
    pred_TS <- predict(object = RF,
                       newdata = TS[, names(TS) != "fire"])
                       #,type = "prob") 
  }else{ #rf
    print("inside model generation and pred TS construction for rf, last choice. algo is ... ")
    print(algo)
    RF <- model(TR[,my_vars],TR[,my_target], ...)
    pred_TS <- predict(object = RF,
                       newdata = TS[, names(TS) != "fire"],
                       type = "prob") 
    
  }
  TS_roc <- roc(TS$fire, pred_TS[, 2])
  TS_auc <- auc(TS_roc)
  print("AUC: {TS_auc}"%>%g)
  if (algo == "svm" |  algo == "mlp_rminer" ){
    RFS <- list(); RFS[[1]] <- RF 
  }else{
    RFS <- c(); RFS[[1]] <- RF    
  }

  TRS <- c(); TRS[[1]] <- TR
  TSS <- c(); TSS[[1]] <- TS
  AUCS <- c(); AUCS[[1]] <- TS_auc
  
  
  return_value <- list(RFS=RFS, AUCS=AUCS, TRS=TRS, TSS=TSS)
  return(return_value)
}


train_on_folds <- function(dataset, folds, columns, model, algo, ...) {
  start_time_bench <- Sys.time()
  list_of_RF = c()
  list_of_AUC = c()
  list_of_TR = c()
  list_of_TS = c()
  
  # Loop over folds

  # CORREZIONE 3 NOVEMBRE 
  if (algo == "svm" | algo == "mlp_rminer"){
    list_of_RF<- list() #RFS <- list() 
  }else{
    list_of_RF<- c()#RFS <- c()
  }
  
  
  for (col in 1:ncol(folds) ) {
  
  
  print("fold: {col}/{ncol(folds)}" %>% g)
  fold <- folds[, col]
  TR = 0; TS = 0;
  list[ TR, TS ] <- split_test_train(dataset, columns, fold)
  
  TR$fire <- TR$fire %>% as.factor
  TS$fire <- TS$fire %>% as.factor
  
  TR = TR[,columns]#[, columns] sample(1:length(TR), length(TR)-2)
  TS = TS[, columns]
  
  my_target = "fire"
  my_vars   = setdiff(columns, "fire")
  
  if(algo == "mlp"){ 
    twofres = decodeClassLabels(TR[,"fire"]) #two-fold  results for mlp "Std_Backpropagation" 
    RF <- model(TR[,my_vars],twofres,learnFunc = "BackpropBatch", size = 35, learnFuncParams=c(0.1, 0.01), maxit = 5000)
    pred_TS <- predict(object = RF,
                       newdata = TS[,my_vars],
                       type = "prob")
  }else if(algo ==  "ruf"){ #Random Uniform Forest...
    RF <- model(TR[,my_vars],TR[,my_target], categorical = "veg_type", threads = "auto",  ...)
    pred_TS <- predict(object = RF,
                       X = TS[, names(TS) != "fire"],
                       type = "prob") 
  }else if(algo ==  "svm"){ 
    print("inside svm model generation and pred_TS computing")#ksvm
    RF <- model(fire~., TR, model="svm",task="prob")# rminer 1.3.1 lo chiama solo svm 
    pred_TS <- predict(object = RF,
                       newdata = TS[, names(TS) != "fire"])
                       #,type = "prob")  RMINER 1.3.1 
  }else if(algo ==  "mlp_rminer"){ 
    print("inside mlp_rminer model generation and pred_TS computing")#ksvm
    RF <- model(fire~., TR, model="mlp",task="prob") 
    pred_TS <- predict(object = RF,
                       newdata = TS[, names(TS) != "fire"])
                       #,type = "prob")  #rminer 1.3.1
  }else{ #rf
    print("inside model generation and pred TS construction for rf, last choice. algo is ... ")
    RF <- model(TR[,my_vars],TR[,my_target], ...)
    pred_TS <- predict(object = RF,
                       newdata = TS[, names(TS) != "fire"],
                       type = "prob") 
    
  }
  
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

  
  end_time_bench <- Sys.time()
  training_time_s = as.numeric(end_time_bench - start_time_bench)
  return_value <- list(list_of_RF, list_of_AUC, list_of_TR, list_of_TS, training_time_s)
  return(return_value)

}




fit_points <- function(points_df, excluded_cols, list_of_RF,algo){
  # sorted_indexes = sort.list(list_of_auc)
  # best_models = sorted_indexes[0:nfolds]
  factors = names(points_df) %>% subset(!. %in% excluded_cols)
  
  list_of_pred = c()
  
  for (col in 1:length(list_of_RF)) {
    print("fit: {col}/{length(list_of_RF)}" %>% g)
    
    RF <- list_of_RF[[col]]
    if(algo == "ruf"){
    pred <- predict(
      object = RF, 
      X = points_df@data[factors], 
      type = "prob"
    )
    }else if(algo == "svm" | algo == "mlp_rminer"){
      pred <- rminer::predict(
        object = RF, 
        newdata = points_df@data[factors] 
        #type = "prob"
      )
    }else {
      pred <- predict(
        object = RF, 
        newdata = points_df@data[factors], 
        type = "prob"
      )
    }
    
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
                            year_from, year_test, box_dimension, nfolds, model, algo, name, ...){
  
  if (missing(name)){
    print("Name is missing in model_functions build_and_train")
    name <- algo
  } 
  dataset <- build_dataset(points_df, fires_df, season, year_from, year_test)
  
  columns = names(dataset) %>% subset(!. %in% excluded_cols)
  
  start_time <- Sys.time()
  
  
  if (nfolds > 1) {
    dataset$box <- create_spatial_grid(dataset, box_dimension)$id
    folds <- create_folds(dataset, nfolds)
    # factors <- columns[columns != "fire"]
    
    return_value <- train_on_folds(dataset, folds, columns, model,algo, ...)
  } else {
    
    return_value <- train_single_fold(dataset, 0.2, columns, model,algo, ...)
  }
  
  end_time <-  Sys.time()
  
  df_time <- data.frame(elapsed_time_s = difftime(end_time, start_time, units = "secs")%>%as.numeric() )
  colnames(df_time) <- c('elapsed_time_s')
  
  write.csv(df_time,  '{output_dir}/elapsed_time_{name}.csv' %>% g)# I print output time csv 
  
  
  return(return_value)
}
#VERSIONE SARA-CANCELLARE??
#build_and_train <- function(points_df, fires_df, excluded_cols, season, 
#                            year_from, year_test, box_dimension, nfolds, model,algo, ...){
#  #fare folds su anni: introduco ciclo per calcolare il risultati sui diversi anni
#  diff <- year_test-year_from;
#  c <- 2;
#  #oppure se voglio fare una scelta a caso degli anni sample(2000:2016,1)
#  year_from_func <- year_from;
#  year_test_func <- year_from_func+c;
#  #year_from_func <- sample(year_from:year_test,1);--altra opzione, ma così non li faccio tutti
#  #year_test_func <- year_from_func+c;
#  while(year_test_func <=year_test){
#    dataset <- build_dataset(points_df, fires_df, season, year_from_func, year_test_func)
#    columns = names(dataset) %>% subset(!. %in% excluded_cols)
#    if (nfolds > 1) {#mai fatto folds =5 con altri algoritmi
#      dataset$box <- create_spatial_grid(dataset, box_dimension)$id
#      folds <- create_folds(dataset, nfolds)
#      # factors <- columns[columns != "fire"]
#    
#      return_value <- train_on_folds(dataset, folds, columns, model,algo, ...) 
#    } else {
#      return_value <- train_single_fold(dataset, 0.2, columns, model, algo, ...)
#    }
#    if(year_from_func != year_from){
#      #sum of all values of return_value_mean calculated before
#      return_value_mean <- Map("+", return_value_mean,return_value);
#    }else{ 
#      return_value_mean <- return_value;
#    }
#    year_from_func <- year_from_func+c;
#    year_test_func <- year_test_func+c;
#  }
#  #mean of all values of return_value_mean calculated before
#  return_value_mean <- mapply('/',return_value_mean,diff/2);
#  return(return_value_mean)
#}


#-------------------------------------------------------------------------------

do_experiment <- function(
  points_df, fires_df, excluded_cols, season, 
  year_from, year_test, box_dimension, nfolds,
  mtry, 
  ntree, nodesize, name, resolution, algo, myname
){
  name = myname
  all_cols <- names(points_df)
  used_cols <- subset(all_cols, !all_cols %in% excluded_cols)
  #print(used_cols)
  mtry <- ceiling(sqrt(length(used_cols)))
  print("algo is")
  print(algo)
  
  if (algo == "randomForest") {
    
    list[RFS, AUC, TR, TS] <- build_and_train(
      points_df, fires_df, excluded_cols, season, 
      year_from, year_test, box_dimension, nfolds,
      model = randomForest, algo = algo,
      mtry = mtry, ntree = ntree, nodesize = nodesize,
      do.trace = T, importance = TRUE, type = "prob", name)  
    
    
  } else if (algo == "ruf") {
    
    list[RFS, AUC, TR, TS] <- build_and_train(
      points_df, fires_df, excluded_cols, season, 
      year_from, year_test, box_dimension, nfolds,
      model = randomUniformForest, algo = algo,
      mtry = mtry, ntree = ntree, nodesize = nodesize,
      do.trace = T, importance = TRUE, type = "prob",name) 
  } else if (algo == "svm" |algo == "mlp_rminer" ) {
    
    list[RFS, AUC, TR, TS] <- build_and_train(
      points_df, fires_df, excluded_cols, season, 
      year_from, year_test, box_dimension, nfolds,
      model = rminer::fit, algo = algo,#modello rminer fit  e1071::svm
      mtry = mtry, ntree = ntree, nodesize = nodesize,
      do.trace = T, importance = TRUE, type = "prob", name) 
    
  } else if (algo == "mlp") {

    list[RFS, AUC, TR, TS] <- build_and_train(
      points_df, fires_df, excluded_cols, season, 
      year_from, year_test, box_dimension, nfolds,
      model = mlp, algo = algo,
      mtry = mtry, ntree = ntree, nodesize = nodesize,
      do.trace = T, importance = TRUE, type = "prob", name) 
    
  } else{
    print("Algorithm label not read properly. Using randomForest instead.")
    print(algo)

    list[RFS, AUC, TR, TS] <- build_and_train(
      points_df, fires_df, excluded_cols, season, 
      year_from, year_test, box_dimension, nfolds,
      model = randomForest, algo = algo,
      mtry = mtry, ntree = ntree, nodesize = nodesize,
      do.trace = T, importance = TRUE, type = "prob", name) 
    
    
    }
  

  
  f <- fit_points(points_df, excluded_cols, RFS,algo)
 
  if(is_Sardegna)
	proj <- CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")#in caso questo per puglia/sicilia
  else if(is_Liguria)
	proj <-CRS("+proj=tmerc +lat_0=0 +lon_0=9 +k=0.9996 +x_0=1500000 +y_0=0 +ellps=intl +towgs84=-104.1,-49.1,-9.9,0.971,-2.917,0.714,-11.68 +units=m +no_defs ")
  else if(is_Puglia)#proj <- CRS("+proj=tmerc +lat_0=0 +lon_0=9 +k=0.9996 +x_0=1500000 +y_0=0 +ellps=intl +towgs84=-168.6,-34.0,38.6,-0.374,-0.679,-1.379,-9.48 +units=m +no_defs ")
    proj <- CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
  else if(is_Sicilia)
	proj <- CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
	
	
  r <- to_raster(f, resolution, crs=proj) 

  
  ret <- new('experiment',
             models = RFS, auc = AUC, tr = TR, ts = TS,
             raster = r, season = season, nfolds = nfolds,
             columns = used_cols, name = name, algo = algo
  )
  
  return(ret)
}


