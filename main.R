# load model functions
source('R/model_functions.R')

# load analysis functions
source('R/analysis_functions.R')

resolution = 100
box_dimension <- 15000
ntree <- 750
nodesize <- 50
# load data
data_dir = 'data_{resolution}m' %>% g
points_df <- read.csv('{data_dir}/points.csv' %>% g, row.names="point_index")
points_df <- SpatialPointsDataFrame(points_df[c("x", "y")], points_df)
fires_df <- read.csv('{data_dir}/fires.csv' %>% g)
fires_df <- SpatialPointsDataFrame(fires_df[c("x", "y")], fires_df)
fires_df$month = month(fires_df$data)



year_from <- 1997
year_test <- 2012

source('model.R')
source('analysis.R')

year_from <- 1997
year_test <- 2016

source('model.R')
source('analysis.R')

year_from <- 2010
year_test <- 2016

source('model.R')
source('analysis.R')
