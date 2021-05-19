# create output folder
dir.create(output_dir, showWarnings = F)

# parameter for loading R data (.RData format)
load_data <- F

# parameter for evaluating the variables' importance 
do_class_imp <- T

# Specific years of training and testing
if (is_Bulgaria){
  year_1 =  2006 
  year_2 = 2014
  shapes_dir = '{region_name}_shapefiles' %>% g
  vegetation_string = "veg"
}


# if batch_test = False please select only the experiments that have been defined in model.R
# If batch test = True,it will be re-used the list of models trained so far

if(batch_test == TRUE){
 experiments = results_list  
}else{
experiments <- c(
  rf_fivefolds_std_s
)
}

if ( load_data ){
  data_file <- 'output/RF_{year_from}_{year_test - 1}.RData' %>% g
  load(data_file)
}


# extract test fires from shapefile
if(is_Bulgaria){
    # reading the shapefile of fires for future analysis (it must include a 'year' column)
    BA <- readOGR("/home/gruppo4/Bulgaria/bg_dati_per_cluster/merge_fires_diss_ry_wgs84_35N.shp"  %>% g)
    BA$stagione = 2                 # the season is just one for all months 
    BA$year = strtoi(BA$random_yea)
    BA$anno = strtoi(BA$year)
}

# option for considering custom test years. 
if(length(year_test)==1){
  # create test set without seasonal division for Bulgaria case
  if(is_Bulgaria){
    BA_test_s <- BA[((BA$stagione==2) & (BA$anno >= year_test)), ]
  }else{
    BA_test_w <- BA[((BA$stagione==1) & (BA$anno >= year_test)), ]
    BA_test_s <- BA[((BA$stagione==2) & (BA$anno >= year_test)), ]
  }
  
}else{
  BA_test_w <- BA[((BA$stagione==1) & (BA$anno %in% year_test)), ]
  BA_test_s <- BA[((BA$stagione==2) & (BA$anno %in% year_test)), ]
  
}


for (exp in experiments) {
  writeRaster(exp@raster, "{output_dir}/{exp@name}.tiff" %>% g, overwrite = TRUE)
}

for (exp in experiments) {
  # define 2 datasets for summer and winter in case seasonal division is taken into account
  if ( exp@season == 1 ){
    BA_test <- BA_test_w
  } else {
    BA_test <- BA_test_s
  }
}
  
  # Do some analysis 
  print('{exp@name}' %>% g)
  out_dir <- "{output_dir}/{exp@name}" %>% g
  dir.create(out_dir, showWarnings = F)
  df_quantiles <- extract_on_quantiles(exp@raster, BA_test)
  print(df_quantiles)
  write.csv(df_quantiles, file = "{out_dir}/quantiles.csv" %>% g)
  
  df_thresholds <- extract_on_thresholds(exp@raster, BA_test)
  print(df_thresholds)
  write.csv(df_thresholds, file = "{out_dir}/thresholds.csv" %>% g)

  df_binary <- extract_on_thresholds(exp@raster, BA_test, 
                                         thresholds=c(0, 0.5, 1),
                                         thresholds_name=c('N', 'Y'))
  print(df_binary)
  write.csv(df_binary, file = "{out_dir}/binary.csv" %>% g)
  #svg_filename <- "{out_dir}/var_imp.svg" %>% g
  print(plot_var_importance(exp)) # it visualizes variables importance
  
  
  if (do_class_imp){
    # svg(filename = "{out_dir}/class_imp_type.svg" %>% g)
    if(exp@algo=='randomForest'){
      print(plot_class_importance(exp, vegetation_string))
      #dev.off()
      if ( 'veg' %in% exp@columns ){
        #svg(filename = "{out_dir}/class_imp_freq.svg" %>% g)
        print(plot_class_importance(exp, 'veg'))
      }
	    
    }else if(exp@algo=='ruf'){
      plot_class_importance(exp,vegetation_string)
    }else{
      print("I am {exp@algo}, hence I don't have yet  a partialplot equivalent, sorry!" %>% g)
    }
  }



df_performances <- data.frame()
df_area_Y <- data.frame()


if (user_clustering && exp@season == 1 ){
  years = yearVectorTest_w
}else if(user_clustering && exp@season == 2){
  years = yearVectorTest_s
}else{
  years = seq(year_1,  year_2)
}
for(year in years){

  if(is_Bulgaria){
    BA_test_s <- BA[((BA$stagione==2) & (BA$anno >= year_test)), ]
  }else{
    BA_test_w <- BA[((BA$stagione==1) & (BA$anno >= year_test)), ]
    BA_test_s <- BA[((BA$stagione==2) & (BA$anno >= year_test)), ]
  }
    for (exp in experiments) {
    if ( exp@season == 1 ){
      BA_test <- BA_test_w
    } else {
      BA_test <- BA_test_s
    }
    
    print('{year} - {exp@name}' %>% g)
    df_binary <- extract_on_thresholds(exp@raster, BA_test, 
                                       thresholds=c(0, 0.5, 1), 
                                       thresholds_name=c('N', 'Y'))
    area_Y <- df_binary['Y',2]
    perfomance <- df_binary['Y',2]/df_binary['Y',1]
    df_performances[year, exp@name] <- perfomance
    df_area_Y[year, exp@name] <- area_Y
  }
}


# save the model performances
if (!user_clustering){
  write.csv(df_performances[year_1:year_2, ], file = '{output_dir}/performances_{exp@name}.csv' %>% g)
  write.csv(df_area_Y[year_1:year_2, ], file = '{output_dir}/area_Y_{exp@name}.csv' %>% g)
}else{
  write.csv(c(df_performances[years[1],],df_performances[years[2],],df_performances[years[3],]), file = '{output_dir}/performances_{exp@name}.csv' %>% g)
  write.csv(c(df_area_Y[years[1],],df_area_Y[years[2],],df_area_Y[years[3],]), file = '{output_dir}/area_Y_{exp@name}.csv' %>% g)
}
auc_results=c();
for (exp in experiments) {
  raster_vals <- exp@raster@data@values
  count = sum((raster_vals>=0.5), na.rm=T)
  value = count/sum(!is.na(raster_vals)) *100
  auc_results<- c(auc_results, '{exp@name} - auc: {mean(exp@auc)} - area: {value}' %>% g)
  print("{exp@name} - auc: {mean(exp@auc)} - area: {value}" %>% g)
}
write.csv(auc_results, file = '{output_dir}/{exp@name}_AUC.csv' %>% g)


#------------------------------- Build test dataset to check RMSE ---------------------------------#
if(is_Bulgaria){
  dataset_s <- build_dataset(points_df, fires_df, 2, year_test, 9999)
  test_dataset_s <- select_pseudo_absences(dataset_s@data, dataset_s@data$fire == 1)
}else{
	dataset_w <- build_dataset(points_df, fires_df, 1, year_test, 9999)
	test_dataset_w <- select_pseudo_absences(dataset_w@data, dataset_w@data$fire == 1)
	dataset_s <- build_dataset(points_df, fires_df, 2, year_test, 9999)
	test_dataset_s <- select_pseudo_absences(dataset_s@data, dataset_s@data$fire == 1)
}

rmse_results=c();
for (exp in experiments) {
  if ( exp@season == 1 ){
    rmse_value <- test_experiment(exp, test_dataset_w)
  } else {
    rmse_value <- test_experiment(exp, test_dataset_s)
  }
  rmse_results<- c(rmse_results, 'The rmse of {exp@name} is: {rmse_value}' %>% g)
  print('{exp@name} : {rmse_value}' %>% g)
}
write.csv(rmse_results, file = '{output_dir}/{exp@name}_RMSE.csv' %>% g)

