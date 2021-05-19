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

g <- glue

extract_stats <- function(raster, BA){
  ZS_mean <- extract(raster, BA, fun=mean, na.rm=T, df=TRUE)
  names(ZS_mean)[2]<-"Mean"
  ZS_sd <- extract(raster, BA, fun=sd, na.rm=T, df=TRUE)
  names(ZS_sd)[2]<-"Standard deviation"
  ZS <- ZS_mean %>% right_join(ZS_sd, by="ID")
  return(ZS)
}


# fucntion for calculating the variable importance
plot_var_importance <- function(exp, n=NULL){
  imp <- c()
  
  
  for (c in 1:length(exp@models)){
    model <- exp@models[[c]]
    #RandomForest::Importance()
    #NeuralNetTools::olden(onefold_mlp_s@models[[1]], y_names = "Y1" )
       
    if(exp@algo=="svm" | exp@algo == "mlp_rminer"){
        var_imp <- rminer::Importance(model,exp@tr[[c]], method = "sensv")
        vett_imp <- c()
        vett_names <- c()
        for (e in 1:length(var_imp$imp)){
          if(!is.null(var_imp$sresponses[[e]]$n))
            vett_imp[e] <- var_imp$imp[e]    
        }
        for (e in 1:length(var_imp$sresponses)){
          #save variables' name
          if(!is.null(var_imp$sresponses[[e]]$n))
            vett_names[e]<- var_imp$sresponses[[e]]$n
        }
        
        imp_df <- data.frame(vett_imp)
        rownames(imp_df) <- vett_names
        imp[[c]] <- imp_df
      
    }else if(exp@algo=="mlp"){
      print("I am doing variable importance for mlp")
      var_imp <- NeuralNetTools::olden(exp@models[[c]])#mlp_onefold_std_s@models[[1]])#rminer::Importance(model,exp@tr[[c]],method = "vsa")
      vett_imp <- c()
      vett_names <- c()
      for (e in 1:var_imp$data$importance){
        #if(!is.null(var_imp$sresponses[[e]]$n))
          vett_imp[e] <- var_imp$data$importance[e]
      }
      for (e in 1:length(var_imp$data$x_names)){
        #if(!is.null(var_imp$sresponses[[e]]$n))
          vett_names[e]<- var_imp$data$x_names[e]
      }
      imp_df <- data.frame(vett_imp)
      rownames(imp_df) <- vett_names
      imp[[c]] <- imp_df
    }
    #------------------------------------------------------------------    
    else if(exp@algo=="ruf"){
      imp_df <- data.frame( model$forest$variableImportance$score)
      rownames(imp_df) =  model$forest$variableImportance$variables
      #rownames(imp_df) <- rownames(imp_df)
      imp[[c]] <- imp_df
      }
    else{
      print("I suppose I am randomForest")
      var_imp = randomForest::importance(model)  
      imp_df <- data.frame(var_imp[,4])
      rownames(imp_df) <- rownames(var_imp)
      imp[[c]] <- imp_df
      
      }
    
  }#end vectors
  if(exp@algo=="svm"| exp@algo == "mlp_rminer"){
    print("I suppose I am a SVM or MLP_RMINER...")
    imp <- abind(imp, along=3)#mischio
    imp_mean <- apply(imp, c(1,2), mean)
    imp_sd <- apply(imp, c(1,2), sd)
    df <- data.frame(mean=imp_mean[,], sd=imp_sd[,])# mean and std df---data.frame
    colnames(df) <- c('mean', 'sd')
    df$class <- rownames(df)
    df <- df[order(-df$mean),]
    if( !is.null(n) ) {
      df <- df %>% head(n)
    }
    write.csv(df,  '{output_dir}/variable_importance_{exp@name}.csv' %>% g) # saving in csv format
    
  }
  else if(exp@algo=="ruf"){

    print("I suppose I am a UniformRandomForest...")
    imp <- abind(imp, along=3)
    imp_mean <- apply(imp, c(1,2), mean)
    imp_sd <- apply(imp, c(1,2), sd)
    df <- data.frame(mean=imp_mean[,], sd=imp_sd[,])
    colnames(df) <- c('mean', 'sd')
    df$class <- rownames(df)
    df <- df[order(-df$mean),]
    if( !is.null(n) ) {
      df <- df %>% head(n)
    }
    write.csv(df,  '{output_dir}/variable_importance_{exp@name}.csv' %>% g) # saving in csv format
    return( df )
    
  }#end ruf
  else{
    print("I suppose I am a randomForest...")
    imp <- abind(imp, along=3)
    imp_mean <- apply(imp, c(1,2), mean)
    imp_sd <- apply(imp, c(1,2), sd)
    df <- data.frame(mean=imp_mean[,], sd=imp_sd[,])# mean and std df---data.frame
    colnames(df) <- c('mean', 'sd')
    df$class <- rownames(df)
    df <- df[order(-df$mean),]
    if( !is.null(n) ) {
      df <- df %>% head(n)
    }
    
    write.csv(df,  '{output_dir}/variable_importance_{exp@name}.csv' %>% g) # saving in csv format  

  
  } #end randomforest
  
  
  return( df )
} #end function


partialPlotRuf <-  function (x, pred.data, x.var, which.class, w, plot=TRUE, add=FALSE,
            n.pt = min(length(unique(pred.data[, xname])), 51), rug = TRUE,
            xlab=deparse(substitute(x.var)), ylab="",
            main=paste("Partial Dependence on", deparse(substitute(x.var))),
            ...)
  {
    classRF <- !x$forest$regression 
    if (is.null(x$forest))
      stop("The randomForest object must contain the forest.\n")
    x.var <- substitute(x.var)
    xname <- if (is.character(x.var)) x.var else {
      if (is.name(x.var)) deparse(x.var) else {
        eval(x.var)
      }
    }
    xv <- pred.data[, xname]
    n <- nrow(pred.data)
    if (missing(w)) w <- rep(1, n)
    if (classRF) {
      if (missing(which.class)) {
        focus <- 1
      }
      else {
        focus <- charmatch(which.class, x$classes)
        if (is.na(focus))
          stop(which.class, "is not one of the class labels.")
      }
    }
    if (is.factor(xv) && !is.ordered(xv)) {
      x.pt <- levels(xv)
      y.pt <- numeric(length(x.pt))
      for (i in seq(along = x.pt)) {
        x.data <- pred.data
        x.data[, xname] <- factor(rep(x.pt[i], n), levels = x.pt)
        if (classRF) {
          pr <- predict(x, x.data, type = "prob")
          y.pt[i] <- weighted.mean(log(ifelse(pr[, focus] > 0,
                                              pr[, focus], .Machine$double.eps)) -
                                     rowMeans(log(ifelse(pr > 0, pr, .Machine$double.eps))),
                                   w, na.rm=TRUE)
        } else y.pt[i] <- weighted.mean(predict(x, x.data), w, na.rm=TRUE)
        
      }
      if (add) {
        points(1:length(x.pt), y.pt, type="h", lwd=2, ...)
      } else {
        if (plot) barplot(y.pt, width=rep(1, length(y.pt)), col="blue",
                          xlab = xlab, ylab = ylab, main=main,
                          names.arg=x.pt, ...)
      }
    } else {
      if (is.ordered(xv)) xv <- as.numeric(xv)
      x.pt <- seq(min(xv), max(xv), length = n.pt)
      y.pt <- numeric(length(x.pt))
      for (i in seq(along = x.pt)) {
        x.data <- pred.data
        x.data[, xname] <- rep(x.pt[i], n)
        if (classRF) {
          pr <- predict(x, x.data, type = "prob")
          y.pt[i] <- weighted.mean(log(ifelse(pr[, focus] == 0,
                                              .Machine$double.eps, pr[, focus]))
                                   - rowMeans(log(ifelse(pr == 0, .Machine$double.eps, pr))),
                                   w, na.rm=TRUE)
        } else {
          y.pt[i] <- weighted.mean(predict(x, x.data), w, na.rm=TRUE)
        }
      }
      if (add) {
        lines(x.pt, y.pt, ...)
      } else {
        if (plot) plot(x.pt, y.pt, type = "l", xlab=xlab, ylab=ylab,
                       main = main, ...)
      }
      if (rug && plot) {
        if (n.pt > 10) {
          rug(quantile(xv, seq(0.1, 0.9, by = 0.1)), side = 1)
        } else {
          rug(unique(xv, side = 1))
        }
      }
    }
    invisible(list(x = x.pt, y = y.pt))
  }


# importance of classes inside a certain variable (like the vegetation one)
plot_class_importance <- function(exp, variable_name, n=NULL){
  pp_veg = c()
  if(exp@algo == "ruf"){
    for(c in 1:length(exp@models)){
      model <- exp@models[[c]]
      tr = exp@tr[[c]]#[1:6000,] # reduce trials 
      pP_params <- list(x=model,  pred.data= tr, 
                        x.var=variable_name, which.class="1", plot=F) 
      pp_veg[[c]] <- do.call("partialPlotRuf", pP_params) %>%
        as.data.frame(., row.names='x') # NO PLOT IN RSTUDIO
      
    }
    
  }else{  
    for(c in 1:length(exp@models)){
      model <- exp@models[[c]]
      tr = exp@tr[[c]]#[1:6000,] # reduce trialse
      pP_params <- list(x=model,  pred.data=tr, 
                        x.var=variable_name, which.class="1", plot=F) 
      pp_veg[[c]] <- do.call("partialPlot", pP_params) %>%
        as.data.frame(., row.names='x') 
	}
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
  
  write.csv(df,  '{output_dir}/vclass_importance_{exp@name}.csv' %>% g)   

  return(df)
}

plot_ecdf <- function(raster, BA, add=F, col='black'){
  masked <- mask(x = raster, mask = BA)
  
  raster_vals <- raster@data@values
  masked_vals <- masked@data@values
  plot(ecdf(raster_vals), col = col, add = add)
  plot(ecdf(masked_vals), col = col, add = T)
}

# masking
summary_masked <- function(raster, BA){
  masked <- mask(x=raster, mask=BA)
  
  raster_vals <- raster@data@values
  masked_vals <- masked@data@values
  df <- as.data.frame( cbind( summary(raster_vals),  summary(masked_vals)))
  colnames(df) <- c('raster', 'fires')
  return(df)
}

# evaluate quantiles 
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

# function for evaluating what is the percentage of fires that falls inside a quantile range of values 
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

# calculate the rmse on the test dataset
test_experiment <- function(exp, test_dataset){
  obs_value <- test_dataset$fire == '1'
  list_of_pred <- c()
  col <- 1
  for( m in exp@models ){
    print("fit: {col}/{length(exp@models)}" %>% g)
  if(exp@algo == "ruf"){
    pred_TS <- predict(object = m,
                       X = test_dataset[,  exp@columns != "fire"],
                       type = "prob")  
  }else{
      pred <- predict(
      object = m, 
      newdata = test_dataset[, exp@columns],type = "prob")
      
    
  }
    if(exp@algo == "ruf"){
      list_of_pred <- cbind(list_of_pred, pred_TS[,2])
    }else{
      list_of_pred <- cbind(list_of_pred, pred[,2])
    }
    col = col + 1
  }
  
  pred_mean <- rowMeans(list_of_pred)
  
  
  rmse(obs_value, pred_mean)
}