source('R/analysis_functions.R')

# load(file='RF_1996_2015.RData')

# Zonal statistics
BA <- readOGR("shapefiles/perimetrazioni_1997_2017.shp")
BA_test_w <- BA[((BA$stagione==1) & (BA$anno >= year_test)), ]
BA_test_s <- BA[((BA$stagione==2) & (BA$anno >= year_test)), ]

experiments <- c(
  onefold_std_w,
  onefold_perc_w,
  onefold_freq_w,
  fivefolds_std_w,
  fivefolds_perc_w,
  fivefolds_freq_w,

  onefold_std_s,
  onefold_perc_s,
  onefold_freq_s,
  fivefolds_std_s,
  fivefolds_perc_s,
  fivefolds_freq_s
)


for (exp in experiments) {
  if ( exp@season == 1 ){
    BA_test <- BA_test_w
  } else {
    BA_test <- BA_test_s
  }
  
  print(exp@name)
  out_dir <- "output/{exp@name}" %>% g
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
  
  svg(filename = "{out_dir}/var_imp.svg" %>% g)
  plot_var_importance(exp)
  dev.off()
  
  # png(filename = "{out_dir}/class_imp_type.png" %>% g, width = 1920, height = 1080)
  # plot_class_importance(exp, 'veg_type')
  # dev.off()

  # if ( 'veg_freq' %in% exp@columns ){
    # png(filename = "{out_dir}/class_imp_freq.png" %>% g, width = 1920, height = 1080)
    # plot_class_importance(exp, 'veg_freq')
    # dev.off()
  # }

}




ZS_w <- extract_stats(std_w@raster, BA_test_w)
write.csv(ZS_w, file = "ZS_winter.csv")

ZS_s <- extract_stats(std_s@raster, BA_test_s)
write.csv(ZS_s, file = "ZS_summer.csv")

#------------------------------------------------------------------------------------------------------
#no perc, 
ZS_w_perc <- extract_stats(perc_w@raster, BA_test_w)
write.csv(ZS_w_perc, file = "ZS_winter_perc.csv")

ZS_s_perc <- extract_stats(perc_s@raster, BA_test_s)
write.csv(ZS_s_perc, file = "ZS_summer_perc.csv")

#------------------------------------------------------------------------------------------------------
#no perc, 
ZS_w_freq <- extract_stats(freq_w@raster, BA_test_w)
write.csv(ZS_w_freq, file = "ZS_winter_freq.csv")

ZS_s_freq <- extract_stats(freq_s@raster, BA_test_s)
write.csv(ZS_s_freq, file = "ZS_summer_freq.csv")

#------------------------------------------------------------------------------------------------------
#no perc, 
ZS_w_1fold <- extract_stats(onefold_w@raster, BA_test_w)
write.csv(ZS_w_1fold, file = "ZS_winter_1fold.csv")

ZS_s_1fold <- extract_stats(onefold_s@raster, BA_test_s)
write.csv(ZS_s_1fold, file = "ZS_summer_1fold.csv")

#---------------------------------------------------------------------------------------------
# add statics to polygons (BA)
# pc -> veg percentage model
# std -> only veg type
# fq -> most frequent neighbour
# of -> as std but with only one fold
BA_test_w@data$pc_m <- ZS_w_perc$`Mean`
BA_test_w@data$pc_s <- ZS_w_perc$`Standard deviation`
BA_test_w@data$std_m <- ZS_w$`Mean`
BA_test_w@data$std_s <- ZS_w$`Standard deviation`
BA_test_w@data$fq_m <- ZS_w_freq$`Mean`
BA_test_w@data$fq_s <- ZS_w_freq$`Standard deviation`
BA_test_w@data$of_m <- ZS_w_1fold$`Mean`
BA_test_w@data$of_s <- ZS_w_1fold$`Standard deviation`

writeOGR(BA_test_w, file.path('./'), "BA_test_w_ZS", driver="ESRI Shapefile", overwrite_layer=T)

BA_test_s@data$pc_m<-ZS_s_perc$`Mean`
BA_test_s@data$pc_s<-ZS_s_perc$`Standard deviation`
BA_test_s@data$std_m<-ZS_s$`Mean`
BA_test_s@data$std_s<-ZS_s$`Standard deviation`
BA_test_s@data$fq_m<-ZS_s_freq$`Mean`
BA_test_s@data$fq_s<-ZS_s_freq$`Standard deviation`
BA_test_s@data$of_m<-ZS_s_1fold$`Mean`
BA_test_s@data$of_s<-ZS_s_1fold$`Standard deviation`

writeOGR(BA_test_s, file.path('./'), "BA_test_s_ZS", driver="ESRI Shapefile", overwrite_layer=T)


#-------------------------------------------------------------------------------

plot_var_importance(std_w)
plot_var_importance(std_w)
plot_var_importance(perc_w)
plot_var_importance(perc_s)
plot_var_importance(freq_w)
plot_var_importance(freq_s)
plot_var_importance(onefold_w)
plot_var_importance(onefold_s)

plot_class_importance(std_w, 'veg_type')
plot_class_importance(std_s, 'veg_type')
plot_class_importance(perc_w, 'veg_type')
plot_class_importance(perc_s, 'veg_type')
plot_class_importance(freq_w, 'veg_type')
plot_class_importance(freq_s, 'veg_type')
plot_class_importance(onefold_w, 'veg_type')
plot_class_importance(onefold_s, 'veg_type')

plot_class_importance(freq_w, 'veg_freq')
plot_class_importance(freq_s, 'veg_freq')


print(extract_on_quantiles(onefold_w@raster, BA_test_w))
print(extract_on_quantiles(onefold_perc_w@raster, BA_test_w))
print(extract_on_quantiles(perc_w@raster, BA_test_w))
print(extract_on_quantiles(std_w@raster, BA_test_w))
print(extract_on_quantiles(freq_w@raster, BA_test_w))


print(extract_on_quantiles(onefold_s@raster, BA_test_s))
print(extract_on_quantiles(onefold_perc_s@raster, BA_test_s))
print(extract_on_quantiles(perc_s@raster, BA_test_s))
print(extract_on_quantiles(std_s@raster, BA_test_s))
print(extract_on_quantiles(freq_s@raster, BA_test_s))
