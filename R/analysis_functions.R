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

extract_stats <- function(raster, BA){
  ZS_mean <- extract(raster, BA, fun=mean, na.rm=T, df=TRUE)
  names(ZS_mean)[2]<-"Mean"
  ZS_sd <- extract(raster, BA, fun=sd, na.rm=T, df=TRUE)
  names(ZS_sd)[2]<-"Standard deviation"
  ZS <- ZS_mean %>% right_join(ZS_sd, by="ID")
  return(ZS)
}



plot_var_importance <- function(exp, n=NULL){
  imp <- c()
  
  for (c in 1:length(exp@models)){
    model <- exp@models[[c]]
    var_imp <- varImp(model, sort=TRUE, n.var=min(30, nrow(model$importance)),
                      type=NULL, class=NULL, scale=TRUE,
                      main=deparse(substitute(model)))
    imp_df <- data.frame(var_imp$`0`)
    rownames(imp_df) <- rownames(var_imp)
    imp[[c]] <- imp_df
  }
  imp <- abind(imp, along=3)
  imp_mean <- apply(imp, c(1,2), mean)
  imp_sd <- apply(imp, c(1,2), sd)
  
  df <- data.frame( mean=imp_mean, sd=imp_sd)
  colnames(df) <- c('mean', 'sd')
  df$class <- rownames(df)
  df <- df[order(-df$mean),]
  if( !is.null(n) ) {
    df <- df %>% head(n)
  }
  
  
  var_plot <- ggplot(df, aes(x=reorder(class, -mean), y=mean)) + 
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2) +
    geom_line() +
    geom_point() +
    ggtitle(exp@name) +
    ylab('Importance') +
    xlab('Variable') +
    theme(
      axis.text=element_text(size=14),
      axis.text.x = element_text(angle = 30, hjust = 1),
      
    )
  return( var_plot )
}

plot_class_importance <- function(exp, variable, n=NULL){
  pp_veg <- c()
  for(c in 1:length(exp@models)){
    model <- exp@models[[c]]
    tr = exp@tr[[c]]
    
    pP_params <- list(x=model,  pred.data=tr, 
                      x.var=variable, which.class="1", plot=F) 
    pp_veg[[c]] <- do.call("partialPlot", pP_params) %>%
      as.data.frame(., row.names='x') 
  }
  pp_veg <- abind(pp_veg, along=3)
  pp_veg_mean <- apply(pp_veg, c(1,2), mean)
  pp_veg_sd <- apply(pp_veg, c(1,2), sd)
  
  df <- data.frame( mean=pp_veg_mean, sd=pp_veg_sd)
  colnames(df) <- c('mean', 'sd')
  df$type <- rownames(df)
  df <- df[order(-df$mean),]
  if( !is.null(n) ) {
    df <- df %>% head(n)
  }
  
  
  var_plot <- ggplot(df, aes(x=reorder(type, -mean), y=mean)) + 
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2) +
    geom_line() +
    geom_point() +
    ggtitle(exp@name) +
    ylab('Importance') +
    xlab('Class') +
    theme(
      axis.text=element_text(size=14),
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
  return(var_plot)
}

plot_ecdf <- function(raster, BA, add=F, col='black'){
  masked <- mask(x = raster, mask = BA)
  
  raster_vals <- raster@data@values
  masked_vals <- masked@data@values
  plot(ecdf(raster_vals), col = col, add = add)
  plot(ecdf(masked_vals), col = col, add = T)
}


summary_masked <- function(raster, BA){
  masked <- mask(x=raster, mask=BA)
  
  raster_vals <- raster@data@values
  masked_vals <- masked@data@values
  df <- as.data.frame( cbind( summary(raster_vals),  summary(masked_vals)))
  colnames(df) <- c('raster', 'fires')
  return(df)
}

extract_on_quantiles <- function(raster, BA, quantiles = c(.25, .50, .75, .90, .95)){
  raster_vals <- raster@data@values
  q <- c(0, quantiles, 1)
  thresholds <- quantile(raster_vals, q, na.rm = T) 
  th_names <- names(thresholds)
  th_names <- th_names[2: length(th_names)]
  df <- extract_on_thresholds(raster, BA, thresholds, th_names)
  df['p_value'] = thresholds[2:length(thresholds)]
  return(df)
}

extract_on_thresholds <- function(raster, BA, 
    thresholds = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
    thresholds_names = c('VERY-LOW', 'LOW', 'MEDIUM', 'HIGH', 'VERY-HIGH')
    ){
  masked <- mask(x=raster, mask=BA)
  raster_vals <- raster@data@values
  masked_vals <- masked@data@values

  df <- data.frame(
    matrix(ncol = 2, nrow = length(thresholds_names)), 
    row.names = thresholds_names
  )
  colnames(df) <- c('TotArea', 'BurnArea')
  
  for (i in 1:(length(thresholds)-1)) {
    t1 = thresholds[[i]]
    t2 = thresholds[[(i+1)]]
    
    
    count = sum((raster_vals>=t1) & (raster_vals <t2), na.rm=T)
    value = count/sum(!is.na(raster_vals))
    df[i, 1] = value * 100
    
    count = sum((masked_vals>=t1) & (masked_vals <t2), na.rm=T)
    value = count/sum(!is.na(masked_vals))
    df[i, 2] = value * 100
  }
  return(df)
}


test_experiment <- function(exp, test_dataset){
  obs_value <- test_dataset$fire == '1'
  
  list_of_pred <- c()
  col <- 1
  for( m in exp@models ){
    print("fit: {col}/{length(exp@models)}" %>% g)
    pred <- predict(
      object = m, 
      newdata = test_dataset[, exp@columns], 
      type = "prob"
    )
    
    # print(rmse(obs_value, pred[,2]))
    
    list_of_pred <- cbind(list_of_pred, pred[,2])
    col = col + 1
  }
  
  pred_mean <- rowMeans(list_of_pred)
  
  
  rmse(obs_value, pred_mean)
}

