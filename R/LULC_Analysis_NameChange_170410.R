library(reshape2)
library(qdapTools)
library(stringr)
library(dplyr)

dirlist = list.dirs(path = ".", full.names = TRUE, recursive = TRUE)

for (k in dirlist) {
  
  csvlist = list.files(k, pattern="LULCC", full.names=F)
  
  for (j in csvlist) {
    fnsplit = strsplit(j,'_')
    fnsplit = unique(unlist(fnsplit))
    fn1 = paste0(fnsplit[2:4], collapse='_')
    fn2 = nth(fnsplit,-2)
    fn3 = paste0(fnsplit[5:(length(fnsplit)-2)], collapse='_')
    fn = paste0(fnsplit[1],'_',fn1,'_',fn2,'_',fn3,'.csv')
    
    oldname = paste0(k,'/',j)
    newname = paste0(k,'/',fn)
    file.rename(oldname, newname)
  }
}
