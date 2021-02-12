dir.create(output_dir, showWarnings = F)

load_data <- F
do_class_imp <- T

# Specific years of testing... In this part, if no user define vector of testing years
# is specified, the test years will be 
#
#             years = seq(year_1,  year_2)

if(is_Liguria){
  shapes_dir = 'shapefiles'
  vegetation_string = "veg_type" 
  # years of analysis 
	year_1 = 2000#2012#1997
	year_2 = 2015  
}else if (is_Sardegna) {
	year_1 =  2007 
	year_2 = 2016
	shapes_dir = '{region_name}_shapefiles' %>% g
  vegetation_string = "veg"
}else if (is_Puglia) {
	year_1 =  2007 
	year_2 = 2015
	shapes_dir = '{region_name}_shapefiles' %>% g
  vegetation_string = "veg"
}else if (is_Sicilia) {
	year_1 =  2007 
	year_2 = 2015
	shapes_dir = '{region_name}_shapefiles' %>% g
  vegetation_string = "veg"
}


### PLEASE SELECT ONLY THE EXPERIMENTS THAT HAVE BEEN DEFINED  IN  model.R
### If I want to do a batch test, I simply re-use the list of models I have
### trained so far...
if(batch_test == TRUE){
 experiments = results_list  
}else{
experiments <- c(
  #onefold_ksvm_s,
  #onefold_std_s
  #onefold_std_w,
  #onefold_perc_w
  # onefold_freq_w,
  #fivefolds_std_w
  #fivefolds_perc_w,
  #fivefolds_freq_w,
  #ninefolds_perc_w,
  #onefold_perc_s
  # onefold_freq_s,
  #mlpr_fivefolds_std_s,
  #svm_fivefolds_std_s,
  #ruf_nop_fivefolds_std_s
  #ruf_fivefolds_std_s,
  rf_fivefolds_std_s
  #rf_nop_fivefolds_std_s
  #mlp_fivefolds_std_s
  #fivefolds_perc_s,
  #fivefolds_freq_s,
  #ninefolds_perc_s
  #ruf_onefold_std_s,
  #ruf_nop_onefold_std_s,
  #svm_onefold_std_s,
  #mlpr_onefold_std_s,
  #rf_onefold_std_s
  #rf_nop_onefold_std_s
  #mlp_onefold_std_s
)
}

if ( load_data ){
  data_file <- 'output/RF_{year_from}_{year_test - 1}.RData' %>% g
  load(data_file)
}

# extract test fires from shapefile

if(is_Sardegna){
	BA <- readOGR("{shapes_dir}/incendi_sar_wgs84_32N.shp"  %>% g)
	BA$stagione = 2 
} else {
	BA <- readOGR("{shapes_dir}/perimetrazioni_1997_2017.shp"  %>% g)
	BA$anno = as.numeric(BA$anno)
	BA$stagione = as.numeric(BA$stagione)
}
if(length(year_test)==1){
  BA_test_w <- BA[((BA$stagione==1) & (BA$anno >= year_test)), ]
  BA_test_s <- BA[((BA$stagione==2) & (BA$anno >= year_test)), ]
}else{
  BA_test_w <- BA[((BA$stagione==1) & (BA$anno %in% year_test)), ]
  BA_test_s <- BA[((BA$stagione==2) & (BA$anno %in% year_test)), ]
  
}

for (exp in experiments) {
  writeRaster(exp@raster, "{output_dir}/{exp@name}.tiff" %>% g, overwrite = TRUE)
}

for (exp in experiments) {
  if ( exp@season == 1 ){
    BA_test <- BA_test_w
  } else {
    BA_test <- BA_test_s
  }
  
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
  #print("I am not doing here any Writing {svg_filename}" %>% g)
  #print(plot_var_importance(exp, 20)) just 20 variables
  print(plot_var_importance(exp)) #now it visualizes everything
  
  if(is_Sardegna){
	#svg(
		#filename = svg_filename,
		#width=12, 
		#height=9, 
		#pointsize=12
	#)
	print(plot_var_importance(exp, 20))
	#dev.off()
  }
  
  
  if (do_class_imp){
    #svg(filename = "{out_dir}/class_imp_type.svg" %>% g)
    if(exp@algo=='randomForest'){
      print(plot_class_importance(exp, vegetation_string))
      #dev.off()
	    if(is_Liguria){
		    if ( 'veg_freq' %in% exp@columns ){
			  #svg(filename = "{out_dir}/class_imp_freq.svg" %>% g)
			  print(plot_class_importance(exp, 'veg_freq'))
		  }
	  }
	    else{
        if ( 'veg' %in% exp@columns ){
          #svg(filename = "{out_dir}/class_imp_freq.svg" %>% g)
          print(plot_class_importance(exp, 'veg'))
        }
	    }
    }else if(exp@algo=='ruf'){
      plot_class_importance(exp,vegetation_string)
    }else{
      print("I am {exp@algo}, hence I don't have yet  a partialplot equivalent, sorry!" %>% g)
    }
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
  BA_test_w <- BA[((BA$stagione==1) & (BA$anno == year)), ]
  BA_test_s <- BA[((BA$stagione==2) & (BA$anno == year)), ]
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

if (!user_clustering){
  write.csv(df_performances[year_1:year_2, ], file = '{output_dir}/performances_{exp@algo}.csv' %>% g)
  write.csv(df_area_Y[year_1:year_2, ], file = '{output_dir}/area_Y_{exp@algo}.csv' %>% g)
}else{
  write.csv(c(df_performances[years[1],],df_performances[years[2],],df_performances[years[3],]), file = '{output_dir}/performances_{exp@algo}.csv' %>% g)
  write.csv(c(df_area_Y[years[1],],df_area_Y[years[2],],df_area_Y[years[3],]), file = '{output_dir}/area_Y_{exp@algo}.csv' %>% g)
}
auc_results=c();
for (exp in experiments) {
  raster_vals <- exp@raster@data@values
  count = sum((raster_vals>=0.5), na.rm=T)
  value = count/sum(!is.na(raster_vals)) *100
  auc_results<- c(auc_results, '{exp@name} - auc: {mean(exp@auc)} - area: {value}' %>% g)
  print("{exp@name} - auc: {mean(exp@auc)} - area: {value}" %>% g)
}
write.csv(auc_results, file = '{output_dir}/{exp@algo}_AUC.csv' %>% g)

#------------------------------- Build test dataset to check RMSE ---------------------------------#
if(is_Sardegna){
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
write.csv(rmse_results, file = '{output_dir}/{exp@algo}_RMSE.csv' %>% g)

