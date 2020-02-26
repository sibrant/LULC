library(sf)
library(fasterize)
library(qdapTools)


#open files
mof = st_read('G:/Agrimetrics/Unilever_PalmOil/DataAnalysis/MoF_LC_IND_1990-2013/_RawData/MoF_LC_1990_to_2015.shp')
moftable = read.csv('g:/Agrimetrics/Unilever_PalmOil/DataAnalysis/EmissionConversionTable_MoF.csv')


idn = st_read('g:/Agrimetrics/Unilever_PalmOil/DataAnalysis/_GIS/IDN_adm1_edit3.shp')
sumUta = idn[which(idn$NAME_1 == 'Sumatera Utara'),]

#intersect mof with SumUta, takes LONG!
mofSumUta = st_intersection(mof, sumUta)

#convert to Polygon type again
mofSumUta = st_cast(mofSumUta, 'POLYGON')

#calculate UTM zone and reproject
b = as.vector(st_bbox(mofSumUta))
lon_av = mean(c(b[1],b[3]))
UTMzone = (floor((lon_av + 180)/6) %% 60) + 1
crsLine = paste0("+proj=utm +zone=",UTMzone," +datum=WGS84")
utmMofSumUta =  st_transform(mofSumUta, crsLine)

#make
r30 = raster(utmMofSumUta, res = 30)
r100 = raster(utmMofSumUta, res = 100)

r30MofSumUta = fasterize(utmMofSumUta, r30, 'LC2015')
r100MofSumUta = fasterize(utmMofSumUta, r100, 'LC2015')

#calculate pixel size to convert to ha
xsize = (abs(r30MofSumUta@extent@xmax-r30MofSumUta@extent@xmin)/r30MofSumUta@ncols)
ysize = (abs(r30MofSumUta@extent@ymax-r30MofSumUta@extent@ymin)/r30MofSumUta@nrows)
r30hapixel = ((xsize * ysize)/1e4)

xsize = (abs(r100MofSumUta@extent@xmax-r100MofSumUta@extent@xmin)/r100MofSumUta@ncols)
ysize = (abs(r100MofSumUta@extent@ymax-r100MofSumUta@extent@ymin)/r100MofSumUta@nrows)
r100hapixel = ((xsize * ysize)/1e4)

# calculate presence of classes for each resolution
r30freq = freq(r30MofSumUta)
r30freq = cbind(r30freq,(r30freq[,2]*r30hapixel))
r30class = as.vector(lookup(r30freq[,1], moftable[, 1:2]))
r30freq = cbind(r30freq,r30class)

colnames(r30freq) = c('Grid Value', 'Pixels', 'Area (ha)', 'LULC class')

csvname  = 'MoFSumUta2015_30mFrequency.csv'
write.csv(r30freq, csvname)

r100freq = freq(r100MofSumUta)
r100freq = cbind(r100freq,(r100freq[,2]*r100hapixel))
r100class = as.vector(lookup(r100freq[,1], moftable[, 1:2]))
r100freq = cbind(r100freq,r100class)

colnames(r100freq) = c('Grid Value', 'Pixels', 'Area (ha)', 'LULC class')

csvname  = 'MoFSumUta2015_100mFrequency.csv'
write.csv(r100freq, csvname)

#write grids and shapefiles away for reference and quicker re-run
st_write(mofSumUta, 'MoF_SumUta.shp')
writeRaster(r30MofSumUta, 'Mof2015_SumUta_30m.tif')
