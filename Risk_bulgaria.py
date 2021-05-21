#%%

#import libraries 

import pandas as pd
import  geopandas as  gpd
import numpy as np
import matplotlib.pyplot as plt
import rasterio as rio
from utils import *
from progressbar import ProgressBar
from pylab import rcParams

#%%

# dem and slope file  path
dem_file = "dem_500_wgs84_35N.tiff"
slope_file = "slope_500_int.tif"

# susceptibility and intensity maps
susc_file = "clim_smo_xy_randomForest_5_fold_std_sea_2.tif"
int_file = "intensity_w2_m0p05.tif"

# susceptibility raster reprojected as the intensity raster 
susc_repr_file = 'susc_repr_as_intensity.tif'               

# the risk raster that will be created 
risk_file = 'Risk_final.tiff'

impact_folder = "./impacts"

# define an object exposure 
# each exposed element will be defined through this object
class Exposure:

    def __init__(self, name, shapename, tiffname, coeff= 1, buffer_m = 500, do_carve = False, do_dissolve = False, intensity_th = 0.0):
        '''
        

        Parameters
        ----------
        name : string
            a custon name of the exposed element.
        shapename : string
            name of the exposed element's shape file .
        tiffname : string
            name of the tiff that will be created (i.e 'esempio.tiff').
        coeff : int or float, optional
            coefficinet for weighting the exposed element . The default is 1.
        buffer_m : int, optional
            a buffer in meter around the exposed element. The default is 500.
        do_carve : boolean, optional
            if True the area of element is carved out and only the bufferd area will be conisdered. The default is False.
        do_dissolve : boolean, optional
            if True the shapefile polygons will be dissolved. The default is False.
        intensity_th : int or float, optional
            intensity threashold over which the element is considered vulnerable. The default is 0.0.

        Returns
        -------
        None.

        '''
        self.name = name
        self.shapename = shapename
        self.tiffname = tiffname
        self.coeff = coeff    
        self.buffer_m = buffer_m
        self.do_carve = do_carve
        self.do_dissolve = do_dissolve
        self.intensity_th = intensity_th
 


# %%
# initialize a list of exposed elements
list_of_exp = []

# create every exposed element considered for the final risk 

settlements = Exposure("settlements", "A_BgSettle_Poly.shp", "settlements.tiff", buffer_m = 750, do_carve = True)

culture_nat_imp_reg = Exposure("culture_national", "FF_CH_01_01_01_CultureNationalImportance_Register.shp",
'culture_national.tiff', do_carve = False) 

culture_UNESCO = Exposure("culture_UNESCO",   "FF_CH_01_01_02_CultureUNESCO_Register.shp", "culture_UNESCO.tiff", do_carve = False)


livestock_breeding = Exposure("livestock_breeding", 
"FF_EA_04_02_LivestockBreeding_Facilities_Cadaster.shp", "livestock_breeding.tiff", do_carve = False)

mining_facilities = Exposure('mining_facilities', 'FF_EA_05_01_Mining_Industry_Facilities_Cadaster.shp',
                             'mining_facilities.tiff', do_carve = True, do_dissolve = True )


hotel = Exposure('hotel', 'FF_EA_06_03_Hotel_Facilities_Cadaster.shp', 'hotel.tiff', do_carve = False, do_dissolve = True )


manufactory = Exposure('manufactory', 'FF_EA_06_07_Manufacturing_Industry_Facilities_Cadaster.shp', 'manufactory.tiff', do_carve = False, do_dissolve = True )


sport = Exposure('sport', 'FF_EA_06_09_CultureSport_Facilities_Cadaster.shp', 'sport.tiff', do_carve = False  )


protected_area = Exposure('protected_area',  'FF_ENV_01_01_ProtectedAreasZZT_Register.shp',  'protected_area.tiff', coeff = 0.1, do_carve = False  )


seveso_facilities = Exposure('seveso_facilities', 'FF_ENV_02_01_01_SevesoFacilities_Register.shp', 'seveso_facilities.tiff', do_carve = False  )


landfill_and_plants = Exposure('landfill_and_plants', 'FF_ENV_02_01_02_ComplexPermits_Register.shp', 'landfill_and_plants.tiff', do_carve = False  )


health = Exposure('health', 'FF_HH_02_01_01_01_Health_Services_Cadaster.shp', 'health.tiff', do_carve = False  )


hospital = Exposure('hospital', 'FF_HH_02_01_01_02_Health_Services_Hospitals_Register.shp', 'hospital.tiff', do_carve = False  )


social_home = Exposure('social_home', 'FF_HH_02_01_01_03_Health_Services_SocialHomes_Register.shp', 'social_home.tiff', do_carve = False  )


social_agency = Exposure('social_agency', 'FF_HH_02_01_01_04_Health_Services_SocialAgency_Register.shp', 'social_agency.tiff', do_carve = False  )

# the most important and complete one (poly) related to education
education = Exposure('education', 'FF_HH_02_01_02_01_Education_Services_Cadaster.shp', 'education.tiff', do_carve = False  )


kinder_gartens = Exposure('school', 'FF_HH_02_01_02_02_Education_Services_SchoolsKinderGartens_Register.shp', 'school.tiff', do_carve = False  )
 

# high scool and colleges --> inside education
# private colleges and high school register they are a bit redundant /not relevant

enery_gas = Exposure('enery_gas', 'FF_HH_02_01_03_EnergyGas_Services_Cadaster.shp', 'enery_gas.tiff', do_carve = False, do_dissolve = True  )

#roads filtered 
roads_filtered = Exposure('roads_filtered', 'roads_filtered_wgs84_35N.shp', 'roads_filtered.tiff', do_carve = False  )

# intake
water_supply = Exposure('water_supply', 'FF_HH_02_01_06_02_WaterSuply_Services_Intake_Register.shp', 'water_supply.tiff', do_carve = False  )


dumping_lpis = Exposure('dumping_lpis', 'FF_HH_02_01_08_02_Dumping_Services_LPIS.shp', 'dumping_lpis.tiff', do_carve = False  )


old_forests = Exposure('old_forests', 'GFS_MZHG_2016.shp', 'old_forests.tiff', do_carve = False, do_dissolve = False, coeff = 0.3  )


old_forest_Rila = Exposure('old_forest_Rila', 'GFS_Rila__bufer_MZHG_2020.shp', 'old_forest_Rila.tiff', do_carve = False, do_dissolve = False, coeff = 0.3 )



list_of_exp.append(old_forests)
list_of_exp.append(old_forest_Rila)
list_of_exp.append(settlements)
list_of_exp.append(culture_nat_imp_reg)
list_of_exp.append(culture_UNESCO)
list_of_exp.append(livestock_breeding)
list_of_exp.append(mining_facilities)
list_of_exp.append(hotel)
list_of_exp.append(manufactory)
list_of_exp.append(sport)
list_of_exp.append(protected_area)
list_of_exp.append(landfill_and_plants)
list_of_exp.append(health)
list_of_exp.append(hospital)
list_of_exp.append(social_home)
list_of_exp.append(social_agency)
list_of_exp.append(education)
list_of_exp.append(kinder_gartens)
list_of_exp.append(enery_gas)
list_of_exp.append(roads_filtered)
list_of_exp.append(dumping_lpis)


# define a function that evaluate the final risk 
# takes into account the susceptibility map, the intensity map and the list of exposed element 
def risk(dem_file, slope_file, susc_file, 
         int_file, list_of_exposures, risk_file ):
    '''
    

    Parameters
    ----------
    dem_file : string
        dem file path.
    slope_file : string
        slope file path.
    susc_file : string
        susceptibility file path.
    int_file : string
        intensity file path.
    list_of_exposures : list
        list of exposed elements.
    risk_file : string
        the path of the output risk map.

    Returns
    -------
    risk map in tiff format.

    '''
    
    rcParams['figure.figsize'] = 5, 10
    
    # src to get susceptibility metadata
    with rio.open(susc_file) as src:
        print(f'Reading susceptibility file {susc_file}')
        susc = src.read(1, masked = True)
        for i, dtype, nodataval in zip(src.indexes, src.dtypes, src.nodatavals):
            print(i, dtype, nodataval)
        print(susc.__class__)
        print(susc.shape)
        #dem = float(dem)
        #dem[dem == 65535] = np.NaN
    _, dx, _, _, _, dy = src.transform.to_gdal()    
    bbox = src.bounds
 
    
    # src2 to get fireline potential intensity metadata
    with rio.open(int_file) as src2:
        print(f'Reading intensity file {int_file}')
        intens = src2.read(1, masked = True)
        for i, dtype, nodataval in zip(src2.indexes, src2.dtypes, src2.nodatavals):
            print(i, dtype, nodataval)
        print(intens.__class__)
        print(intens.shape)
        #dem = float(dem)
        #dem[dem == 65535] = np.NaN
    _, dx2, _, _, _, dy2 = src2.transform.to_gdal()    
    bbox2 = src2.bounds
    
    
    # final product of any exposure    
    total_risk = np.zeros(intens.shape)

    # read the shapefiles
    for exp in list_of_exposures:
        print("hallo, I am doing the following exposure:")
        print(exp.name)
        exp_shp = gpd.read_file(impact_folder + "/"+ exp.shapename)
        exp_shp = exp_shp.to_crs("EPSG:32635")
        # if it is needed, a dissolve is performed
        if exp.do_dissolve:
            exp_shp['dissolvefield'] = 1
            exp_shp = exp_shp.dissolve(by='dissolvefield')
        # create buffer; since it is a geoseries
        # and not a geodataframe; got to convert
        exp_shape_b = exp_shp.buffer(exp.buffer_m)
        exp_shape_b = gpd.GeoDataFrame(exp_shape_b)
        # geometry is missing, it is indexed as "0". Need to add it.
        exp_shape_b["geometry"] = exp_shape_b.iloc[:,0]        
        # create the carved layer
        if exp.do_carve:            
            exp_shape_b = gpd.overlay(exp_shape_b, exp_shp, how='difference')

        # the produced raster needs to be saved in geotiff format with the same metadata as the fire intensity layer
        # w2 m0p05 stands for: scenario with wind factor = 2 and fine fuel moisture factor = 0.05 
        raster = rasterize_numerical_feature(exp_shape_b, int_file)
        save_raster_as_noforce(raster, exp.tiffname, int_file )
      
            
        # the saved raster is opened as src3
        with rio.open(exp.tiffname) as src3:
            print(f'Reading indicator file {exp.tiffname}')
            raster = src3.read(1, masked = True)
            for i, dtype, nodataval in zip(src3.indexes, src3.dtypes, src3.nodatavals):
                print(i, dtype, nodataval)
            print(raster.__class__)
            print(raster.shape)
            #dem = float(dem)
            #dem[dem == 65535] = np.NaN
        _, dx3, _, _, _, dy3 = src3.transform.to_gdal()    
        bbox3 = src3.bounds
    
    
    
        # identify the pixels in the rasterized wildland exposed interfaces
        # and  with non zero fireline intensity.
        mask = (intens > exp.intensity_th) & (raster == 1.0)
        
        indeces = np.argwhere(mask)

        # create the risk matrix-raster 
        # associated to the referred exposed element
        risk = np.zeros(intens.shape)

        # every pixel P can receive "risk" from itself and the 8 neighbours "X"
       
       	# [X][X][X]
       	# [X][P][X]
       	# [X][X][X]
   
   
        neighbours = [(0,0), (-1, -1 ), (-1  ,0 ) ,( -1, 1 ) , ( 0, -1) , (0, 1), (1,-1 ) , (1, 0) , (1, 1)]


        bar = ProgressBar()
        for idx in bar(indeces):
            i, j = idx[0],idx[1]
            risk_t = 0.0
            # every pixel calls for risk from neighbours (and itself)
            for nn in neighbours:
                risk_t += intens[i + nn[0], j+nn[1]]

            risk[i,j] = risk_t 



        # rescaling the risk by the element coeffient 
        risk = risk*exp.coeff 
        
        # save the risk 
        save_raster_as_noforce(risk, str('risk_' + exp.tiffname) , int_file)
        
        # update the total risk by summing the risk assocaited to each exposed layer
        total_risk += risk
    
    # end of exposure loop 
    # save the total risk using the metadata of the intensity file   
    # save_raster_as(risk,risk_file ,int_file)
        
        
    # normalizing the risk
    max_risk = total_risk.max()
    print('max value of risk before normalization = ', max_risk)    
    
    total_risk = total_risk / max_risk
    print('max value of risk after normalization = ', total_risk.max())
      
    save_raster_as_noforce(total_risk, 'risk_no_susc.tiff' , int_file)
       
    # suscettibilty reprojected as intensity  
    susc_reproject = np.zeros(intens.shape)
    
    
    a,b =  rio.warp.reproject(susc,destination = susc_reproject, src_transform=src.transform,
           src_crs=src.crs,
           dst_transform=src2.transform,
           dst_crs=src2.crs)

    
    # fill with zeros the nodatas of reprojected susceptibility   
    susc_reproject_zerofill = np.where(susc_reproject<0.001, 0, susc_reproject) 
    
    # pixel by pixel multiplication --> suspectibility times total risk
    final_risk = total_risk * susc_reproject_zerofill
    
    print('Just plotting the final risk')
    plt.matshow(final_risk)
    plt.colorbar()
    
    # saving the risk raster    
    save_raster_as_noforce(final_risk, risk_file, int_file)
    
    print('The risk is saved as tiff file')
    
    
# %%


risk(dem_file, slope_file, susc_file, 
     int_file, list_of_exp, risk_file)




