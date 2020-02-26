library(rgdal)
library(rgeos)
library(sp)
library(gdalUtils)
library(raster)
library(maptools)
library(qdapTools)


## SET UTM ZONE OBTAIN FUNCTION ##
long2UTM <- function(long) {
  (floor((long + 180)/6) %% 60) + 1
}

#Import LC label and GHG emission value tables

cci = read.csv('g:/Agrimetrics/Unilever_PalmOil/DataAnalysis/EmissionConversionTable_CCI.csv')
crisp = read.csv('g:/Agrimetrics/Unilever_PalmOil/DataAnalysis/EmissionConversionTable_CRISP.csv')
mof = read.csv('g:/Agrimetrics/Unilever_PalmOil/DataAnalysis/EmissionConversionTable_MoF.csv')

#Read grid files, based on peat soil mapping.
#Could also be more general
setwd("g:/Agrimetrics/Unilever_PalmOil/DataAnalysis/MoF_LC_IND_1990-2013/IDN")


#make list of dirs
dirlist = list.dirs(recursive=F)

#make subset for Kalten, Riau, Sumuta for CCI & CRISP
dirlist = dirlist[c(15,25,33)]

for (k in dirlist) {
  
  #dir.create(paste0(k,'/GHG_Map'))
  #dir.create(paste0(k,'/LULCC_MapTable'))
  
    #make list of clipped grids
    cliplist=list.files(paste0(k,'/'), pattern="tif$", full.names=FALSE)
    
    #subset cliplist for MoF 
    #cliplist = cliplist[c(3,5,7,10)]
    
    
    #make subset for CCI v2 data
    #cliplist = cliplist[4:length(cliplist)]
    
    #subset for 2000,2005,2010,2015
    #cliplist = cliplist[c(9, 14,19,24)]
    #subset for 2000,2003,2006,2009,2011,2012,2013,2015
    #cliplist = cliplist[c(9,12,15,18,20,21,22,24)]
    
    shplist=list.files(paste0(k,'/'), pattern="shp$", full.names=FALSE, recursive= T)
    if (length(shplist) > 0) {
      peat = readOGR(paste0(k,'/',shplist[grep('Peat',shplist)]))
      min = readOGR(paste0(k,'/',shplist[grep('Min',shplist)]))
    }
    
    
    for (i in 1:length(cliplist)) {
    #assign first and last time step as t0 and t1
        if (i == length(cliplist)) {
          t0 = assign(unlist(strsplit(cliplist[1], "[.]"))[1], raster(paste0(k,'/',cliplist[1])))
          lab0 = t0@data@names
          lab0 = gsub('_v2','',lab0)
          if (grepl('CCI', lab0)) {
          lab0 = gsub('_IDN','',lab0)
          }
          t0[t0 < 1 ] = NA
          
          t1 = assign(unlist(strsplit(cliplist[length(cliplist)], "[.]"))[1], raster(paste0(k,'/',cliplist[length(cliplist)])))
          lab1 = t1@data@names
          lab1 = gsub('_v2','',lab1)
          if (grepl('CCI', lab1)) {
          lab1 = gsub('_IDN','',lab1)
          }
          t1[t1 < 1 ] = NA
          }
        
        #assign t0 and subsequent time step t1
        else {
          t0 = assign(unlist(strsplit(cliplist[i], "[.]"))[1], raster(paste0(k,'/',cliplist[i])))
          lab0 = t0@data@names
          lab0 = gsub('_v2','',lab0)
          if (grepl('CCI', lab0)) {
          lab0 = gsub('_IDN','',lab0)
          }
          t0[t0 < 1 ] = NA
          
          t1 = assign(unlist(strsplit(cliplist[i+1], "[.]"))[1], raster(paste0(k,'/',cliplist[i+1])))
          lab1 = t1@data@names
          lab1 = gsub('_v2','',lab1)
          if (grepl('CCI', lab1)) {
          lab1 = gsub('_IDN','',lab1)
          }
          t1[t1 < 1 ] = NA
          
        }
      
      # extract class information from each time step
      if (grepl('MoF', lab0)) {
        class = mof
      } else if (grepl('CCI', lab0)) {
        class = cci
      } else {
        class = crisp
      }
      
        #extract period of research, for peat soil emission calculation
        yr0 = as.numeric(gsub("\\D", "", lab0))
        yr1 = as.numeric(gsub("\\D", "", lab1))
        period=yr1-yr0
        
        if (period > 1) {
          per = 'years'
        }
        
        else if (period==1) {
          per = 'year'
        }
        
        #get file info for naming
        lulc = unlist(strsplit(lab0, '_'))[1]
        aoi = paste0(unlist(strsplit(lab0, '_'))[4:length(unlist(strsplit(lab0, '_')))], collapse='_')
        
        #check whether they are same extent, otherwise resample extent t1 to extent t0
        if (isTRUE(t0@ncols != t1@ncols | t0@nrows != t1@nrows | 
                   t0@extent@xmin != t1@extent@xmin | t0@extent@xmax != t1@extent@xmax)) {
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
        
        #define multiplication factor for change map
        t0fact = 10^nchar(max(class[,1]))
        
        #make mask of change pixels, all values of 0 indicate pixels without LULC change
        t01mask = abs(t0-t1)
        t01mask[t01mask > 0] = 1
        
        #multiply t0 with factor to create unique values for LULC changes
        t0mult = t0 * t0fact
        
        #add t0mult to t1 and multiply with mask to calculate change grid
        t01change = (t0mult + t1) * t01mask
        
        #erase pixels with value 0
        t01change[t01change == 0]  <- NA
        
        #calculate frequency distributions for legend 
        freqt01change = freq(t01change)
        freqt01change = na.omit(freqt01change)
        
        #calculate Ha change
        hachange = freqt01change[,2]*hapixel
        
        #calculate frequency distributions for peat and mineral areas
        if (length(shplist > 0)) {
          #extract peat soil areas
          t01clippeat = rasterize(peat, t01change, mask=TRUE)
          
          freqt01peat = freq(t01clippeat)
          freqt01peat = na.omit(freqt01peat)
          
          #calculate Ha change
          hachangepeat = freqt01peat[,2]*hapixel
          
          #extract mineral soil areas
          t01clipmin = rasterize(min, t01change, mask=TRUE)
          
          freqt01min = freq(t01clipmin)
          freqt01min = na.omit(freqt01min)
          
          #calculate Ha change
          hachangemin = freqt01min[,2]*hapixel
          
          }
        
        if (length(freqt01change > 0)) {
        
          #Make table for peat and mineral soil areas
          if (length(shplist > 0)) {
            
            #create empty dataframes
            lulctable = data.frame()
            lulctablemin = data.frame()
            lulctablepeat =  data.frame()
            
            #make table for mineral soils
            lulctablemin = cbind(freqt01min[,1],(hachangemin),(freqt01min[,1]%/%t0fact),(freqt01min[,1]%%t0fact))
            changelabmin = paste0(lookup(lulctablemin[,3], class[, 1:2])," --> ",lookup(lulctablemin[,4], class[, 1:2]))
            
            changeagbmin = (lookup(lulctablemin[,3], class[, c(1,3)]) - lookup(lulctablemin[,4], class[, c(1,3)]))
            
            ghgagbmin = changeagbmin * hachangemin
            
            soillabmin = 'Mineral'
            
            empty=''
            
            lulctablemin = cbind(lulctablemin, changelabmin, soillabmin, changeagbmin, ghgagbmin, empty, empty, ghgagbmin)
            
            #make overview GHG change
            ghgsummin = sum(as.numeric(lulctablemin[,11]))
            ghgsumminyr = ghgsummin / period
            
            #make overview Ha change 
            hachangemin = sum(as.numeric(lulctablemin[,2]))
            hachangeminyr = hachangemin/period
            
            #make table for peat soils
            if (length(freqt01peat > 0)) {
                lulctablepeat = cbind(freqt01peat[,1],(hachangepeat),(freqt01peat[,1]%/%t0fact),(freqt01peat[,1]%%t0fact))
                changelabpeat = paste0(lookup(lulctablepeat[,3], class[, 1:2])," --> ",lookup(lulctablepeat[,4], class[, 1:2]))
            
                changeagbpeat = (lookup(lulctablepeat[,3], class[, c(1,3)]) - lookup(lulctablepeat[,4], class[, c(1,3)]))
            
                ghgagbpeat = changeagbpeat * hachangepeat
            
                soillabpeat = 'Peat'
            
                changesoil = lookup(lulctablepeat[,4], class[, c(1,4)])
            
                ghgsoil = changesoil * hachangepeat * period
                
                ghgtotalpeat = ghgagbpeat + ghgsoil
                
                lulctablepeat = cbind(lulctablepeat, changelabpeat, soillabpeat, changeagbpeat, ghgagbpeat, changesoil, ghgsoil, ghgtotalpeat)
            
                #make overview GHG change
                ghgsumpeat = sum(as.numeric(lulctablepeat[,11]))
                ghgsumpeatyr = ghgsumpeat / period
                
                #make overview Ha change 
                hachangepeat = sum(as.numeric(lulctablepeat[,2]))
                hachangepeatyr = hachangepeat/period
                
                
                #Combine the two tables
            
                lulctable = rbind(lulctablemin, lulctablepeat)
                
            }
            
            else {
              lulctable = lulctablemin
            }
            
            colnames(lulctable) = c('Grid Value', 'Area (ha)', 'Class t0', 'Class t1', 'From .. to ..', 'Soil Type', 'AGB Emission Factor',
                                    'AGB GHG Emissions', 'Soil Emissions/yr Factor', 'Soil GHG Emissions', 'Total GHG Emissions')
            
            #write to CSV
            csvname  = paste0(k, '/LULCC_MapTable/LULCC_Table_',lulc,'_',aoi,'_',yr0,'_',yr1,'.csv')
            write.csv(lulctable, csvname)
            
            #add lines for GHG overview
            ghgsum = sum(as.numeric(lulctable[,11]))
            
            ghgyr = ghgsum/period
            
            ghgminperc = format(round(((ghgsummin/ghgsum)*100),2), nsmall=2)
            ghgpeatperc = 100-as.numeric(ghgminperc)
            
            headline = paste0(',Total area,Mineral Soils,Peat Soils')
            sumline = paste0('Mg C emissions in ', period, ' ', per, ':,',ghgsum,',',ghgsummin,',',ghgsumpeat)
            yrline = paste0('Mg C emissions per year:, ',ghgyr,',',ghgsumminyr,',',ghgsumpeatyr)
            percline = paste0('Percentage of total:,,',ghgminperc,',',ghgpeatperc)
            
            write('\n', csvname, append=T)
            write(headline, csvname, append=T)
            write(sumline, csvname, append=T)
            write(yrline, csvname, append=T)
            write(percline, csvname, append=T)
            
            
            #add lines for ha change overview
            hasum = sum(as.numeric(lulctable[,2]))
            hayr = hasum/period
            
            haminperc = format(round(((hachangemin/hasum)*100),2), nsmall=2)
            hapeatperc = 100-as.numeric(haminperc)
            
            headhaline = paste0(',Total area,Mineral Soils,Peat Soils')
            sumhaline = paste0('Ha LULC change in ', period, ' ', per, ':,',hasum,',',hachangemin,',',hachangepeat)
            yrhaline = paste0('Ha LULC change per year:, ',hayr,',',hachangeminyr,',',hachangepeatyr)
            perchaline = paste0('Percentage of total:,,',haminperc,',',hapeatperc)
            
            write('\n', csvname, append=T)
            write(headhaline, csvname, append=T)
            write(sumhaline, csvname, append=T)
            write(yrhaline, csvname, append=T)
            write(perchaline, csvname, append=T)
            
            ## MAKE MAPS
            
            #create mask maps for mineral soils
            t01emissmin <- setValues(raster(t01clipmin), NA)
            for (i in 1:nrow(lulctablemin)) {
              t01emissmin[t01clipmin==as.numeric(lulctablemin[i,1])] = as.numeric(lulctablemin[i,7])
            }
            
            t01emissmin[is.na(t01emissmin[])] <- 0
            
            #create emission maps for peat soils
            if (length(freqt01peat > 0)) {
              t01emisspeat <- setValues(raster(t01clippeat), NA)
              for (i in 1:nrow(lulctablepeat)) {
                t01emisspeat[t01clippeat==as.numeric(lulctablepeat[i,1])] = as.numeric(lulctablepeat[i,7]) + (period * (as.numeric(lulctablepeat[i,9])))
              }
              
              t01emisspeat[is.na(t01emisspeat[])] <- 0
              
              #combine mineral and peat maps
              t01emiss = t01emissmin + t01emisspeat
            }
            
            else {
              #set emission map to mineral map
              t01emiss = t01emissmin
            }
            
            t01emiss[t01emiss == 0]  <- NA
            
          }
          
          else if (length(freqt01change ==  0)) {
            
            lulctable = data.frame()
          
            #add columns
            lulctable = cbind(freqt01change[,1],(hachange),(freqt01change[,1]%/%t0fact),(freqt01change[,1]%%t0fact))
            changelab = paste0(lookup(lulctable[,3], class[, 1:2])," --> ",lookup(lulctable[,4], class[, 1:2]))
            
            changeagb = (lookup(lulctable[,3], class[, c(1,3)]) - lookup(lulctable[,4], class[, c(1,3)]))
            
            ghgagb = changeagb * hachange
              
            #changecol = (as.numeric(lulctable[,1])/max(as.numeric(lulctable[,1])))
            lulctable = cbind(lulctable, changelab, changeagb, ghgagb)
            colnames(lulctable) = c('Grid Value', 'Area (ha)', 'Class t0', 'Class t1', 'From .. to ..', 'AGB Emission Factor', 'AGB GHG Emissions')
            
            #write to CSV
            write.csv(lulctable, paste0(k, '/LULCC_MapTable/LULCC_Table_',lulc,'_',aoi,'_',yr0,'_',yr1,'.csv'))
            
            ghgsum = sum(as.numeric(lulctable[,7]))
            
            ghgyr = ghgsum/period
            
            sumline = paste0('Total emissions:, ',ghgsum, ', Mg C in ', period, ' ', per, '.')
            yrline = paste0('Yearly emissions:, ',ghgyr, ', Mg C per year.')
            write('\n', paste0(k, '/LULCC_MapTable/LULCC_Table_',lulc,'_',aoi,'_',yr0,'_',yr1,'.csv'), append=T)
            write(sumline, paste0(k, '/LULCC_MapTable/LULCC_Table_',lulc,'_',aoi,'_',yr0,'_',yr1,'.csv'), append=T)
            write(yrline, paste0(k, '/LULCC_MapTable/LULCC_Table_',lulc,'_',aoi,'_',yr0,'_',yr1,'.csv'), append=T)
            
            #make maps
            #create emission maps for mineral soils
            t01emiss <- setValues(raster(t01change), NA)
            for (i in 1:nrow(lulctable)) {
              t01emiss[t01change==as.numeric(lulctable[i,1])] <- as.numeric(lulctable[i,6])
            }
            
            t01emiss[t01emiss == 0]  <- NA
          }
         
          
          #write GHG emission map to geotiff
          writeRaster(t01emiss, paste0(k,'/GHG_Map/GHG_Map_',lulc,'_',aoi,'_',yr0,'_',yr1,'.tif'), overwrite=T)
          print(paste0(Sys.time(),' GHG_Map_',lulc,'_',aoi,'_',yr0,'_',yr1))
          
          #write LULC change map to geotiff
          writeRaster(t01change, paste0(k,'/LULCC_MapTable/LULCC_Map_',lulc,'_',aoi,'_',yr0,'_',yr1,'.tif'), overwrite=T)
          
          #remove temporary files
          rm(list= ls(pattern='t0'))
          rm(list= ls(pattern='t1'))
          rm(list = ls(pattern=unlist(strsplit(k, "[/]"))[2]))
          removeTmpFiles(h=0)
        }
        
        else {
          next
        }
          
    }
}


