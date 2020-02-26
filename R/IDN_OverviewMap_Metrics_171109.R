library(sf)

setwd("G:/Agrimetrics/Unilever_PalmOil/DataAnalysis/_GIS/IDN_Overview")

filenames = list.files(recursive=TRUE,pattern="*.shp",full.names=FALSE)

varnames = vector()

for (i in filenames) {
  varname = unlist(strsplit(i, '[.]'))[1]
  assign(varname, read_sf(i))
  varnames = c(varnames, varname)
  
}

#union and convert to UTM
utmnames = vector()

for (i in varnames) {
  utmname = paste0(i,'_UTM')
  i_union = st_union(eval(as.symbol(i)))
  assign(utmname, st_transform(i_union, 32647))
  utmnames = c(utmnames, utmname)
  
}

overview = vector()

for (i in utmnames) {
  area = st_area(eval(as.symbol(i)))
  row = cbind(i,area)
  overview = rbind(overview, row)
  
}

#alternative, via geosphere
#Values are different, but probably better

library(geosphere)

overview2 = vector()

for (i in varnames) {
  latlonname = paste0(i,'_LatLon')
  i_union = st_union(eval(as.symbol(i)))
  area = st_area(i_union)
  row = cbind(latlonname ,area)
  overview2 = rbind(overview2, row)
  
}

