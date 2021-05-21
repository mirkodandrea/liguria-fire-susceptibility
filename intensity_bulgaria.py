#!/usr/bin/env python
# coding: utf-8



import pandas as pd
import  geopandas as  gpd
import numpy as np
import matplotlib.pyplot as plt
import rasterio as rio
from utils import *
from progressbar import ProgressBar

from pylab import rcParams
rcParams['figure.figsize'] = 5, 10

# files for the intensity map production

# paths of  rasters  
dem_file = "dem_500_wgs84_35N.tiff"
slope_file = "slope_500_int.tif"
risico_file  = "risico_raster.tif"
susc_file = "clim_smo_xy_randomForest_5_fold_std_sea_2.tif"

# load the corine land cover 
clc18 = gpd.read_file("CLC2018/CLC2018_diss.shp")

# risico aggregated vegetaion
p_veg = pd.read_csv("p_vegetazione.csv")
p_veg = p_veg.set_index("#ID")
p_veg["ID"] = p_veg.index
p_dict =  p_veg.to_dict(orient = "index")


get_ipython().system('ls')


# load the DEM and slope

with rio.open(dem_file) as src:
    print(f'Reading dem file {dem_file}')
    dem = src.read(1, masked = True)
    for i, dtype, nodataval in zip(src.indexes, src.dtypes, src.nodatavals):
        print(i, dtype, nodataval)
    print(dem.__class__)
    print(dem.shape)
_, dx, _, _, _, dy = src.transform.to_gdal()    
bbox = src.bounds


with rio.open(slope_file) as src1:
    print(f'Reading slope file')
    slp = src1.read(1, masked = True)
    for i, dtype, nodataval in zip(src1.indexes, src1.dtypes, src1.nodatavals):
        print(i, dtype, nodataval)
    print(slp.__class__)
    print(slp.shape)
_, dx, _, _, _, dy = src.transform.to_gdal()    
bbox = src.bounds


# rasterize the vegetation 
print(f'Rasterizing vegetation')
risico_raster = rasterize_numerical_feature(clc18, dem_file, 'id_aggr')    
save_raster_as(risico_raster, risico_file, dem_file)

# open the file already saved as src2
with rio.open(risico_file) as src2:
    risico = src2.read(1, masked = True)
    for i, dtype, nodataval in zip(src2.indexes, src2.dtypes, src2.nodatavals):
        print(i, dtype, nodataval)
    print(risico.__class__)
    print(risico.shape)
_, dx, _, _, _, dy = src2.transform.to_gdal()    
bbox = src.bounds


# initialize the rate of spread and intensity matrices
mask = ( ~dem.mask) & (~slp.mask) & (~risico.mask) 
indeces = np.argwhere(mask)
ros = np.zeros(dem.data.shape)
intens = np.zeros(dem.data.shape)

intens = intens.astype('float32')

# select model parameters' values 
wind_effect = 2.0
ffmc = 0.05 #dffm

# model equations for RoS and Intensity evalautions
bar = ProgressBar()
a = 0
for idx in bar(indeces):
    i, j = idx[0],idx[1]

    ros_t =  p_dict[int(risico[i,j])]['v0'] 
    ros_t *= np.exp(-1 * (ffmc / 20.0)**2.0)
    ros_t *= wind_effect
    ros_t *= 1.0 + LAMBDA * ( (slp[i,j]*np.pi/180.0) / (np.pi / 2.0))
    ros[i,j] = ros_t
    lhv_canopy = p_dict[int(risico[i,j])]['hhv'] * (1.0 - (p_dict[int(risico[i,j])]['umid'] / 100.0)) - Q * (p_dict[int(risico[i,j])]['umid'] / 100.0)
    intens_t  = ros[i,j] 
    intens_t *= (p_dict[int(risico[i,j])]['hhv'] *(1.0 - (ffmc / 100.0)) - Q * (ffmc / 100.0))* p_dict[int(risico[i,j])]['d0'] + lhv_canopy * p_dict[int(risico[i,j])]['d1'] 
    intens_t /=  3600.0
    intens[i,j] = intens_t



# save the latter intensity matrix as tif file
intensity_dataset = rio.open(
     './intensity_w2_m0p05.tif',
     'w',
     driver=src.driver,
     height=src.shape[0],
     width=src.shape[1],
     count=1,
     dtype=intens.dtype,
     crs=src.crs,
     transform=src.transform)

intensity_dataset.write(intens, 1)
intensity_dataset.close()

# load the susceptibility map
susc = rio.open(susc_file, masked = True)

rf = susc.read(1, masked = True)

rf2 = rf.copy()

# plt.matshow(rf)
# plt.colorbar()

# reproject the intensity raster as the susceptibility's projection
intens_reproject = intens.copy()

a,b =  rio.warp.reproject(intens,destination = rf2, src_transform=src.transform,
     src_crs=src.crs,
     dst_transform=susc.transform,
     dst_crs=susc.crs)





