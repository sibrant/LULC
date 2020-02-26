library(rgdal)
library(rgeos)
library(sp)
library(gdalUtils)
library(raster)
library(maptools)
library(ggplot2)
library(qdapTools)
library(sf)

#load reference data for aggregation
cciTable = read.csv('g:/Agrimetrics/Unilever_PalmOil/DataAnalysis/EmissionConversionTable_CCI_Aggregate2.csv')
crispTable = read.csv('g:/Agrimetrics/Unilever_PalmOil/DataAnalysis/EmissionConversionTable_CRISP_Aggregate2.csv')
mofTable = read.csv('g:/Agrimetrics/Unilever_PalmOil/DataAnalysis/EmissionConversionTable_MoF_Aggregate2.csv')

#Load mapcurves function from separate script

#setwd to R script folders
setwd("G:/Agrimetrics/Unilever_PalmOil/DataAnalysis/R/")

if(!exists('mapcurves', mode='function')) source('mapcurves.R')

setwd('G:/Agrimetrics/Unilever_PalmOil/DataAnalysis/_ComparisonLULCData/MapCurves/Aggregation_171103')

#load the files

#2015 data
year='2015'
#KalTen
assign('mof' , raster("G:/Agrimetrics/Unilever_PalmOil/DataAnalysis/MoF_LC_IND_1990-2013/IDN/Kalimantan Tengah/MoF_IDN_LC2015_Kalimantan Tengah.tif"))
assign('cci', raster("G:/Agrimetrics/Unilever_PalmOil/DataAnalysis/CCI_LC_1992-2015/IDN/Kalimantan Tengah/CCI_LC_v2_IDN_2015_Kalimantan Tengah.tif"))
assign('crisp', raster("G:/Agrimetrics/Unilever_PalmOil/DataAnalysis/CRISP_LC_IND_2000-2015/IDN/Kalimantan Tengah/CRISP_LC_IDN_2015_Kalimantan Tengah.tif"))
area = 'KalTen'

#SumUta
assign('mof' , raster("G:/Agrimetrics/Unilever_PalmOil/DataAnalysis/MoF_LC_IND_1990-2013/IDN/Sumatera Utara/MoF_IDN_LC2015_Sumatera Utara.tif"))
assign('cci', raster("G:/Agrimetrics/Unilever_PalmOil/DataAnalysis/CCI_LC_1992-2015/IDN/Sumatera Utara/CCI_LC_v2_IDN_2015_Sumatera Utara.tif"))
assign('crisp', raster("G:/Agrimetrics/Unilever_PalmOil/DataAnalysis/CRISP_LC_IND_2000-2015/IDN/Sumatera Utara/CRISP_LC_IDN_2015_Sumatera Utara.tif"))
area = 'SumUta'

#Riau
assign('mof' , raster("G:/Agrimetrics/Unilever_PalmOil/DataAnalysis/MoF_LC_IND_1990-2013/IDN/Riau/MoF_IDN_LC2015_Riau.tif"))
assign('cci', raster("G:/Agrimetrics/Unilever_PalmOil/DataAnalysis/CCI_LC_1992-2015/IDN/Riau/CCI_LC_v2_IDN_2015_Riau.tif"))
assign('crisp', raster("G:/Agrimetrics/Unilever_PalmOil/DataAnalysis/CRISP_LC_IND_2000-2015/IDN/Riau/CRISP_LC_IDN_2015_Riau.tif"))
area = 'Riau'

#2000 data
year='2000'
#KalTen
assign('mof' , raster("G:/Agrimetrics/Unilever_PalmOil/DataAnalysis/MoF_LC_IND_1990-2013/IDN/Kalimantan Tengah/MoF_IDN_LC2000_Kalimantan Tengah.tif"))
assign('cci', raster("G:/Agrimetrics/Unilever_PalmOil/DataAnalysis/CCI_LC_1992-2015/IDN/Kalimantan Tengah/CCI_LC_v2_IDN_2000_Kalimantan Tengah.tif"))
assign('crisp', raster("G:/Agrimetrics/Unilever_PalmOil/DataAnalysis/CRISP_LC_IND_2000-2015/IDN/Kalimantan Tengah/CRISP_LC_IDN_2000_Kalimantan Tengah.tif"))
area = 'KalTen'

#SumUta
assign('mof' , raster("G:/Agrimetrics/Unilever_PalmOil/DataAnalysis/MoF_LC_IND_1990-2013/IDN/Sumatera Utara/MoF_IDN_LC2000_Sumatera Utara.tif"))
assign('cci', raster("G:/Agrimetrics/Unilever_PalmOil/DataAnalysis/CCI_LC_1992-2015/IDN/Sumatera Utara/CCI_LC_v2_IDN_2000_Sumatera Utara.tif"))
assign('crisp', raster("G:/Agrimetrics/Unilever_PalmOil/DataAnalysis/CRISP_LC_IND_2000-2015/IDN/Sumatera Utara/CRISP_LC_IDN_2000_Sumatera Utara.tif"))
area = 'SumUta'

#Riau, with 2000 data
assign('mof' , raster("G:/Agrimetrics/Unilever_PalmOil/DataAnalysis/MoF_LC_IND_1990-2013/IDN/Riau/MoF_IDN_LC2000_Riau.tif"))
assign('cci', raster("G:/Agrimetrics/Unilever_PalmOil/DataAnalysis/CCI_LC_1992-2015/IDN/Riau/CCI_LC_v2_IDN_2000_Riau.tif"))
assign('crisp', raster("G:/Agrimetrics/Unilever_PalmOil/DataAnalysis/CRISP_LC_IND_2000-2015/IDN/Riau/CRISP_LC_IDN_2000_Riau.tif"))
area = 'Riau'

#get frequency tables for aggregation
freqMof = freq(mof)
freqMof = as.vector(na.omit(as.vector(freqMof[,1])))

for (i in freqMof) {
  mof[mof == i] <- lookup(i, mofTable[, c(1,6)])
}
writeRaster(mof, paste0(area, '_', year, '_MoF_agg.tif'), overwrite=T)

cci= cci*10
freqCci = freq(cci)
freqCci = as.vector(na.omit(as.vector(freqCci[,1])))

for (i in freqCci) {
  cci[cci == i] <- lookup(i/10, cciTable[, c(1,6)])
}
writeRaster(cci, paste0(area, '_', year, '_CCI_agg.tif'), overwrite=T)

crisp = crisp * 10
freqCrisp = freq(crisp)
freqCrisp = as.vector(na.omit(as.vector(freqCrisp[,1])))

for (i in freqCrisp) {
  crisp[crisp == i] <- lookup(i/10, crispTable[, c(1,6)])
}
writeRaster(crisp, paste0(area, '_', year, '_CRISP_agg.tif'), overwrite=T)

#resample both cci and crisp to mof
ccitemp = raster(nrows = mof@nrows, ncols = mof@ncols, xmn = mof@extent@xmin, xmx = mof@extent@xmax, 
                    ymn = mof@extent@ymin, ymx = mof@extent@ymax , crs = mof@crs)
cciresample = projectRaster(from = cci, to = ccitemp, method = 'ngb')
rm(ccitemp)

crisptemp = raster(nrows = mof@nrows, ncols = mof@ncols, xmn = mof@extent@xmin, xmx = mof@extent@xmax, 
                 ymn = mof@extent@ymin, ymx = mof@extent@ymax , crs = mof@crs)
crispresample = projectRaster(from = crisp, to = crisptemp, method = 'ngb')
rm(crisptemp)

#resample cci to crisp
ccitemp2 = raster(nrows = crisp@nrows, ncols = crisp@ncols, xmn = crisp@extent@xmin, xmx = crisp@extent@xmax, 
                 ymn = crisp@extent@ymin, ymx = crisp@extent@ymax , crs = crisp@crs)
cciresample2 = projectRaster(from = cci, to = ccitemp2, method = 'ngb')
rm(ccitemp2)

# #apply mapcurves
# rescrisp2cci = mapcurves(crisp,cciresample2, 'CRISP', 'CCI')
# lapply(rescrisp2cci[2:8], function(x) write.table( data.frame(x), paste0(area, '_', year,'_CRISP2CCI_agg.csv'), append=TRUE, sep=','))
# ggsave(paste0(area,'_', year,'_CRISP2CCI_agg.png'), plot = rescrisp2cci[[1]], width = 250, height = 250, units = 'mm', dpi = 300)
# 
# resmof2cci = mapcurves(mof, cciresample, 'MoF', 'CCI')
# lapply(resmof2cci[2:8], function(x) write.table( data.frame(x), paste0(area, '_', year,'_MoF2CCI_agg.csv'), append=TRUE, sep=','))
# ggsave(paste0(area,'_', year,'_MoF2CCI_agg.png'), plot = resmof2cci[[1]], width = 250, height = 250, units = 'mm', dpi = 300)
# 
# 
# resmof2crisp = mapcurves(mof,crispresample, 'MoF', 'CRISP')
# lapply(resmof2crisp[2:8], function(x) write.table( data.frame(x), paste0(area, '_', year,'_MoF2CRISP_agg.csv'), append=TRUE, sep=','))
# ggsave(paste0(area, '_', year,'_MoF2CRISP_agg.png'), plot = resmof2crisp[[1]], width = 250, height = 250, units = 'mm', dpi = 300)

###############################################################
## calculate area consistency
csvname = paste0(area, '_', year, '_AreaOverlap.csv')
header = 'LULC compared, % Consistency'
write(header, csvname)

#crisp2cci
difcrisp2cci = crisp-cciresample2
fdifcrisp2cci = na.omit(freq(difcrisp2cci))
nulldifcrisp2cci = as.numeric(fdifcrisp2cci[which(fdifcrisp2cci[,1] == 0),2])
sumdifcrisp2cci = sum(fdifcrisp2cci[,2])
percdifcrisp2cci = (nulldifcrisp2cci/sumdifcrisp2cci)*100
line = paste0('CRISP2CCI,',percdifcrisp2cci)
write(line, csvname, append = T)

difcrisp2cci[difcrisp2cci != 0] <- 100
difcrisp2cci[difcrisp2cci == 0] <- 1
difcrisp2cci[difcrisp2cci == 1-0] <- 0
writeRaster(difcrisp2cci, paste0(area, '_', year, '_CRISP2CCI_agg_overlap.tif'), overwrite=T)

#mof2cci
difmof2cci = mof-cciresample
fdifmof2cci = na.omit(freq(difmof2cci))
nulldifmof2cci = as.numeric(fdifmof2cci[which(fdifmof2cci[,1] == 0),2])
sumdifmof2cci = sum(fdifmof2cci[,2])
percdifmof2cci = (nulldifmof2cci/sumdifmof2cci)*100
line = paste0('MoF2CCI,',percdifmof2cci)
write(line, csvname, append = T)

difmof2cci[difmof2cci != 0] <- 100
difmof2cci[difmof2cci == 0] <- 1
difmof2cci[difmof2cci == 100] <- 0
writeRaster(difmof2cci, paste0(area, '_', year, '_MoF2CCI_agg_overlap.tif'), overwrite=T)

#mof2crisp
difmof2crisp = mof - crispresample
fdifmof2crisp = na.omit(freq(difmof2crisp))
nulldifmof2crisp = as.numeric(fdifmof2crisp[which(fdifmof2crisp[,1] == 0),2])
sumdifmof2crisp = sum(fdifmof2crisp[,2])
percdifmof2crisp = (nulldifmof2crisp/sumdifmof2crisp)*100
line = paste0('MoF2CRISP,',percdifmof2crisp)
write(line, csvname, append = T)

difmof2crisp[difmof2crisp != 0] <- 100
difmof2crisp[difmof2crisp == 0] <- 1
difmof2crisp[difmof2crisp == 100] <- 0
writeRaster(difmof2crisp, paste0(area, '_', year, '_MoF2CRISP_agg_overlap.tif'), overwrite=T)


