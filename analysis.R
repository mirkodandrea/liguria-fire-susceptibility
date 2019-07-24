
output_dir <- 'output/{year_from}_{year_test - 1}' %>% g
dir.create(output_dir, showWarnings = F)

load_data = F
do_class_imp = F

if ( load_data ){
  data_file <- 'output/RF_{year_from}_{year_test - 1}.RData' %>% g
  load(data_file)
}

# Zonal statistics
BA <- readOGR("shapefiles/perimetrazioni_1997_2017.shp")
BA_test_w <- BA[((BA$stagione==1) & (BA$anno >= year_test)), ]
BA_test_s <- BA[((BA$stagione==2) & (BA$anno >= year_test)), ]

experiments <- c(
  onefold_std_w,
  onefold_perc_w,
  # onefold_freq_w,
  fivefolds_std_w,
  fivefolds_perc_w,
  # fivefolds_freq_w,
  ninefolds_perc_w,
  
  onefold_std_s,
  onefold_perc_s,
  # onefold_freq_s,
  fivefolds_std_s,
  fivefolds_perc_s,
  # fivefolds_freq_s,
  ninefolds_perc_s
)

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
  
  svg_filename <- "{out_dir}/var_imp.svg" %>% g
  print("Writing {svg_filename}" %>% g)
  svg(
    filename = svg_filename,
    width=12, 
    height=9, 
    pointsize=12
  )
  print(plot_var_importance(exp, 15))
  dev.off()
  
  
  if (do_class_imp){
    svg(filename = "{out_dir}/class_imp_type.svg" %>% g)
    print(plot_class_importance(exp, 'veg_type'))
    dev.off()
  
    if ( 'veg_freq' %in% exp@columns ){
      svg(filename = "{out_dir}/class_imp_freq.svg" %>% g)
      print(plot_class_importance(exp, 'veg_freq'))
      dev.off()
    }
  }
}


df_performances <- data.frame()
df_area_Y <- data.frame()
for(year in seq(1997, 2017)){
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

write.csv(df_performances[1997:2017, ], file = '{output_dir}/performances.csv' %>% g)
write.csv(df_area_Y[1997:2017, ], file = '{output_dir}/area_Y.csv' %>% g)

for (exp in experiments) {
  raster_vals <- exp@raster@data@values
  count = sum((raster_vals>=0.5), na.rm=T)
  value = count/sum(!is.na(raster_vals)) *100
  print("{exp@name} - auc: {mean(exp@auc)} - area: {value}" %>% g)
}


dataset_w <- build_dataset(points_df, fires_df, 1, year_test, 9999)
test_dataset_w <- select_pseudo_absences(dataset_w@data, dataset_w@data$fire == 1)


dataset_s <- build_dataset(points_df, fires_df, 2, year_test, 9999)
test_dataset_s <- select_pseudo_absences(dataset_s@data, dataset_s@data$fire == 1)

for (exp in experiments) {
  if ( exp@season == 1 ){
    rmse_value <- test_experiment(exp, test_dataset_w)
  } else {
    rmse_value <- test_experiment(exp, test_dataset_s)
  }
  print('{exp@name} : {rmse_value}' %>% g)
}


