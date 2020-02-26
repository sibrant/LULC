#install packages if necessary
#install.packages("rgdal")
#install.packages("lulcc")
#install.packages("sp")
#install.packages("maptools")
#install.packages("rgeos")

#open packages
library(rgdal)
library(lulcc)
library(sp)
library(maptools)
library(rgeos)
library(raster)

setwd("f:/Agrimetrics/Unilever_PalmOil/DataAnalysis/CRISP_LC_IND_2000-2015/BOR")

## LC classes ##

#make table of MoF land cover classes and their grid values
MoFcat = c(2014, 2010, 20091, 20092, 20093, 20095, 20122, 2001,2002, 2004, 
           2005, 20041, 20051, 2007, 3000, 2006, 2500, 5001, 20121, 20141,
           20142, 20094, 5003, 2012, 20071, 50011)
MoFlab =  c('Bare Land','Plantation','Dry Rice Land','Dry Rice Land Mixed w/Scrub', 
            'Rice Land', 'Transmigration', 'Transmigration2', 'Primary Dry Land Forest',
            'Secondary Dry Land Forest', 'Primary Mangrove Forest','Primary Swamp Forest',
            'Secondary Mangrove Forest', 'Secondary Swamp Forest', 'Scrubland', 
            'Savannah', 'HTI', 'Cloud', 'Bodies of Water', 'Airport', 'Mining', 'Snow', 
            'Fish Pond', 'Ocean / River', 'Housing', 'Swamp Scrubland', 'Swamp')

MoFcatLab = rbind(MoFcat,MoFlab)

#Labels CCI classes
CCIcat = c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,
           122,130,140,150,152,153,160,170,180,190,200,201,202,210,220)
CCIlab =  c('No data','Cropland, rainfed','Cropland, rainfed herbaceous cover','Cropland, rainfed tree or shrub cover',
            'Cropland, irrigated or post-flooding','Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)',
            'Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) / cropland (<50%) ','Tree cover, broadleaved, evergreen, closed to open (>15%)',
            'Tree cover, broadleaved, deciduous, closed to open (>15%)','Tree cover, broadleaved, deciduous, closed (>40%)',
            'Tree cover, broadleaved, deciduous, open (15-40%)','Tree cover, needleleaved, evergreen, closed to open (>15%)',
            'Tree cover, needleleaved, evergreen, closed (>40%)','Tree cover, needleleaved, evergreen, open (15-40%)',
            'Tree cover, needleleaved, deciduous, closed to open (>15%)','Tree cover, needleleaved, deciduous, closed (>40%)',
            'Tree cover, needleleaved, deciduous, open (15-40%)','Tree cover, mixed leaf type (broadleaved and needleleaved)',
            'Mosaic tree and shrub (>50%) / herbaceous cover (<50%)','Mosaic herbaceous cover (>50%) / tree and shrub (<50%)',
            'Shrubland','Shrubland evergreen','Shrubland deciduous','Grassland','Lichens and mosses','Sparse vegetation (tree, shrub, herbaceous cover) (<15%)',
            'Sparse shrub (<15%)','Sparse herbaceous cover (<15%)','Tree cover, flooded, fresh or brakish water','Tree cover, flooded, saline water',
            'Shrub or herbaceous cover, flooded, fresh/saline/brakish water','Urban areas','Bare areas','Consolidated bare areas',
            'Unconsolidated bare areas','Water bodies','Permanent snow and ice')

CCIcatLab = rbind(CCIcat,CCIlab)

#Labels CRISP classes
CRISPcat = c(1,2,3,4,5,6,7,8,9,10,11,12,13)
CRISPlab = c('Water','Mangrove','Peatswamp forest','Lowland evergreen forest','Lower montane evergreen forest','Upper montane evergreen forest',
             'Plantation/regrowth','Lowland mosaic','Montane mosaic','Lowland open','Montane open','Urban',
             'Large scale palm plantations')

CRISPcatLab = rbind(CRISPcat,CRISPlab)

## Sei Mangkei

#Make spatial point for Sei Mangkei area
x= 99.350029
y= 3.136465
seimangkei=cbind(x,y)
seimangkei = SpatialPoints(seimangkei)

#give it a projection
latlong = "+init=epsg:4326"
seimangkeiWGS = SpatialPoints(seimangkei, proj4string = CRS(latlong))

#function to obtain UTM zone
long2UTM <- function(long) {
  (floor((long + 180)/6) %% 60) + 1
}

seimangkeiUTMzone = long2UTM(x)
projUTM = paste0("+proj=utm +zone=",seimangkeiUTMzone," +datum=WGS84")

#change projection to UTM to create 50km buffer
seimangkeiUTM <- spTransform(seimangkeiWGS, CRS(projUTM) )
seimangkeiBuffer = gBuffer(seimangkeiUTM, width =50000, quadsegs = 200, byid=TRUE)

#convert back to WGS to have consistent projection with LC maps
seimangkeiBufferWGS = spTransform(seimangkeiBuffer, CRS(latlong) )

#write into shapefile
newdata = data.frame('seimangkei_50km')
seimangkeiBufferWGS = SpatialPolygonsDataFrame(seimangkeiBufferWGS, data=newdata)
writeOGR(seimangkeiBufferWGS, '.', "SeiMangkei_50km", driver="ESRI Shapefile", overwrite_layer = T) 

#define poly as seimangkei
poly = seimangkeiBufferWGS
polyName = 'Sei Mangkei_50km'

# Alternatively, import
poly = readOGR('.','SeiMangkei_50km')
polyName = 'Sei Mangkei_50km'


## Beluran ##

#Make spatial point for Beluran area
x=117.386808
y=5.733953
beluran=cbind(x,y)
beluran = SpatialPoints(beluran)

#give it a projection
latlong = "+init=epsg:4326"
beluranWGS = SpatialPoints(beluran, proj4string = CRS(latlong))

#function to obtain UTM zone
long2UTM <- function(long) {
  (floor((long + 180)/6) %% 60) + 1
}

beluranUTMzone = long2UTM(x)
projUTM = paste0("+proj=utm +zone=",beluranUTMzone," +datum=WGS84")

#change projection to UTM to create 50km buffer
beluranUTM <- spTransform(beluranWGS, CRS(projUTM) )
beluranBuffer = gBuffer(beluranUTM, width =50000, quadsegs = 200, byid=TRUE)

#convert back to WGS to have consistent projection with LC maps
beluranBufferWGS = spTransform(beluranBuffer, CRS(latlong) )

#write into shapefile
newdata = data.frame('Beluran_50km')
beluranBufferWGS = SpatialPolygonsDataFrame(beluranBufferWGS, data=newdata)
writeOGR(beluranBufferWGS, '.', "Beluran_50km", driver="ESRI Shapefile", overwrite_layer = T) 

#define poly as beluran
poly = beluranBufferWGS
polyName = 'Beluran_50km'

# Alternatively, import
poly = readOGR('.','Beluran_50km')
polyName = 'Beluran_50km'

##Beluran Peat SOils ## 
poly = readOGR("g:/Agrimetrics/Unilever_PalmOil/DataAnalysis/CCI_LC_2000-2010/BOR/Beluran_50km.shp")
polyName = 'Beluran_50km_PeatSoils'


##Beluran Mineral Soils
poly = gBuffer(poly, byid=TRUE, width=0)
poly2 = readOGR("g:/Agrimetrics/Unilever_PalmOil/DataAnalysis/_GIS/PeatSoils_Beluran50km.shp")
poly2 = gBuffer(poly2, byid=TRUE, width=0)
poly3 = gDifference(poly, poly2, byid=T)
newdata = data.frame('Beluran_50km_MineralSoils')
poly = SpatialPolygonsDataFrame(poly3, data=newdata, match.ID = F)
writeOGR(poly, '.', "Beluran_50km_MineralSoils", driver="ESRI Shapefile", overwrite_layer = T) 

poly = readOGR('.','Beluran_50km_MineralSoils.shp')
polyName = 'Beluran_50km_MineralSoils'


## Kotawaringin ##
poly = readOGR("F:/Agrimetrics/Unilever_PalmOil/DataAnalysis/IND_KotingawaringinBarat_WGS84.shp")
polyName = poly$NAME_2

## Kotawarining peat soils ##
poly = readOGR("f:/Agrimetrics/Unilever_PalmOil/DataAnalysis/PeatSoils_KotingawaringinBarat.shp")
polyName = 'Kotawaringin Barat_PeatSoils'

## Central Kalimantan ##
poly = readOGR("f:/Agrimetrics/Unilever_PalmOil/DataAnalysis/IND_CenKal_WGS84.shp")
polyName = poly$NAME_1

## Central Kalimantan Peat Soils##
poly = readOGR("f:/Agrimetrics/Unilever_PalmOil/DataAnalysis//PeatSoils_Kalimantan Tengah.shp")
polyName = 'Kalimantan Tengah_PeatSoils'

## Central Kalimantan Mineral Soils##
poly3 = gDifference(poly, poly2)
poly = readOGR("./Kalimantan Tengah_MineralSoils.shp")
polyName = 'Kalimantan Tengah_MineralSoils'

## Clip rasters ##

#Make output dir for the clipped rasters
outputDir = dir.create(toString(polyName))

#make list of geotiff files in current working directory
rlist=list.files(getwd(), pattern="tif$", full.names=FALSE)

#load geotiff files as rasters in R and clip to polygon
for(i in rlist) {
  #load rasters into R environment and create unique name
  raster = assign(unlist(strsplit(i, "[.]"))[1], raster(i))
  rname = raster@data@names
  rclipname = paste0('./',polyName,'/',rname,'_',polyName,'.tif')
  
  # check if extent of raster and polygons overlap completely
  rasterextent = as(extent(raster), 'SpatialPolygons')
  if (gContainsProperly(poly, rasterextent)==F) {
    if (!file.exists(rclipname)) {
      clip1 = crop(raster, poly)
      clip2 = mask(clip1, poly, filename=rclipname)
      
      rasterclip = assign(paste0(rname,'_',polyName),clip2)
  } else {
    next
  }
  } else {
    next
  }
}



##LULC analysis ##

#make list of clipped files
cliplist=list.files(paste0('./',polyName,'/'), pattern="tif$", full.names=FALSE)

#select two images to compare
for (i in 1:length(cliplist)) {
  #crosstabulate t0 and tlast
  if (i == length(cliplist)) {
    t0 = assign(unlist(strsplit(cliplist[1], "[.]"))[1], raster(paste0(getwd(),'/',polyName,'/',cliplist[1])))
    lab0 = t0@data@names
    t1 = assign(unlist(strsplit(cliplist[length(cliplist)], "[.]"))[1], raster(paste0(getwd(),'/',polyName,'/',
                                                                                      cliplist[length(cliplist)])))
    lab1 = t1@data@names
    
  }
    
    #select two time steps
    else {
      t0 = assign(unlist(strsplit(cliplist[i], "[.]"))[1], raster(paste0(getwd(),'/',polyName,'/',cliplist[i])))
      lab0 = t0@data@names
      t1 = assign(unlist(strsplit(cliplist[i+1], "[.]"))[1], raster(paste0(getwd(),'/',polyName,'/',cliplist[i+1])))
      lab1 = t1@data@names
    }
    
   
    
    #check whether they are same extent, otherwise resample extent t1 to extent t0
    if (isTRUE(t0@ncols != t1@ncols | t0@nrows != t1@nrows)) {
      t1resample = raster(nrows = t0@nrows, ncols = t0@ncols, xmn = t0@extent@xmin, xmx = t0@extent@xmax, 
                           ymn = t0@extent@ymin, ymx = t0@extent@ymax , crs = t0@crs)
      t1 = projectRaster(from = t1, to = t1resample, method = 'ngb')
    }
    
    #Calculate pixel/Ha via UTM (optional, takes computing time)
    lon_av = mean(c(t0@extent@xmax,t0@extent@xmin))
    t0UTMzone = long2UTM(lon_av)
    t0proj = projectRaster(t0, crs = (paste0("+proj=utm +zone=",t0UTMzone," +datum=WGS84")))
    xsize = (abs(t0proj@extent@xmax-t0proj@extent@xmin)/t0proj@ncols)
    ysize = (abs(t0proj@extent@ymax-t0proj@extent@ymin)/t0proj@nrows)
    hapixel = ((xsize * ysize)/1e4)
    
    # extract class information from each time step
    f0 = freq(t0)
    l0 = c(f0[,1])
    l0 = l0[!is.na(l0)]
    f1 = freq(t1)
    l1 = c(f1[,1])
    l1 = l1[!is.na(l1)]
    
    #find overlapping and unique land cover classes
    catUnion = union(l0,l1)
    
    #subset the complete land cover class list with the classes present in the images
    if (grepl('MoF', lab0)) {
      labSubset = subset(MoFcatLab[2,], MoFcatLab[1,] %in% catUnion)
      catSubset = subset(MoFcatLab[1,], MoFcatLab[1,] %in% catUnion)
    } else if (grepl('CCI', lab0)) {
      labSubset = subset(CCIcatLab[2,], CCIcatLab[1,] %in% catUnion)
      catSubset = subset(CCIcatLab[1,], CCIcatLab[1,] %in% catUnion)
    } else {
      labSubset = subset(CRISPcatLab[2,], CRISPcatLab[1,] %in% catUnion)
      catSubset = subset(CRISPcatLab[1,], CRISPcatLab[1,] %in% catUnion)
    }
    
    #make new data frame of land cover classes only present in the images
    catLabSubset = rbind(catSubset,labSubset)
    
    #cross-tabulate land use change between two time steps, with crossTabulate (LULCC)
    table = crossTabulate2(t0, t1, categories = catLabSubset[1,], labels = catLabSubset[2,])
    tableHa = round(hapixel*table[,],digits=0)
    write.csv(table, paste0(getwd(),'/',polyName,'/LULCC_',lab0,'_',lab1,'.csv'), row.names=T)
    write.csv(tableHa, paste0(getwd(),'/',polyName,'/LULCC_',lab0,'_',lab1,'_Ha.csv'), row.names=T)
    
}


## Map plotting ##

#plot maps

gridValues = c(2002,2006,2007,2010,2012,2014,5001,20041,20071,20091,20093,20094,50011)
colors = c('#00ff00','#ebd5a0','#966400','#ffea80','#ff0000','#fff5d7','#0000ff','#009678','#00dc82','#ffff64','#aaf0f0','#00fbff')
plot(t0,breaks=gridValues,col=colors)
plot(t1,breaks=gridValues,col=colors)