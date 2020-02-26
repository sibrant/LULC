mapcurves <- function(A,B,nameA,nameB){
  
  library(ggplot2)

## R implementation of the mapcurves goodness of fit measure 
## (Hargrove et al. 20006, see full reference below) for comparing 
## two categorical maps
##
## usage: 
##
##  out <- mapcurves(mapA,mapB)
##
## where 
##   mapA = a matrix or vector with integers, representing a categorical map
##   mapB = a matrix or vector with integers, representing a categorical map
##   out = a list with the (named) fields 
##       out$GOF = the mapcurves goodness of fit value (GOF)
##       out$Refmap = the map to be used as reference map ('A' or 'B')
##       out$GOFtable = the GOF for each pare of classes in maps A and B 
##       out$mGOF_A2B = maximum GOF when using A as reference map
##       out$mGOF_B2A = maximum GOF when using B as reference map
##       out$BMC_A2B = best matching class (BMC) and corresponding maximum GOF
##                       when using map A as reference (data frame with three columns)
##       out$BMC_B2A = BMC and corresponding maximum GOF when using map B as reference
##                       (data frame with three columns)
##
## additional requirements:
##    - maps A and B should be matrices or vectors of equal size 
##    - missing values should be coded with NA and are disregarded 
##       in the comparison
##
## This implementation is based on the description of the Mapcurves algorithm
## by Hargrove et al. in the following paper:
##
## William W. Hargrove, Forrest M. Hoffman and Paul F. Hessburg (2006) 
##   Mapcurves: a quantitative method for comparing categorical maps.
##   J Geograph Syst, 8, 187–208. DOI 10.1007/s10109-006-0025-x

## Emiel van Loon, May 2011
## University of Amsterdam
## http://staf.science.uva.nl/~vanloon/


A <- as.vector(A)    # required for the unique function
B <- as.vector(B)

# identify all unique values in maps A and B,
# the sort function automatically removes the NA values that 
# are still reported by unique.
a <- sort( unique(A) )  
b <- sort( unique(B) )
nra <- length(a)
nrb <- length(b)
tC <- matrix( data=NA, nrow=nra, ncol=nrb )
rC <- matrix( data=NA, nrow=nra, ncol=nrb )

for (i in 1:nra) {
    for (j in 1:nrb) {
        tC[i,j] = sum( ( A==a[i] ) & ( B==b[j] ), na.rm = TRUE )
    }
}

Sa = rowSums(tC)
Sb = colSums(tC)

for (i in 1:nra) {
    for (j in 1:nrb) {
        rC[i,j] = ( tC[i,j]^2 ) / ( Sa[i]*Sb[j] );
    }
}

rSa = rowSums(rC);
rSb = colSums(rC);

sSa = c(sort(rSa), 1);
sSb = c(sort(rSb), 1);

sSa100 = sSa*100
sSb100 = sSb*100

# percent of categories larger than this GOF value
PCLa = c( seq(1, 0, length.out=nra+1));
PCLb = c( seq(1, 0, length.out=nrb+1));

#original version, seems incorrect to me
# PCLa = c( 1, seq(1, 0, length.out=nra), 0);
# PCLb = c( 1, seq(1, 0, length.out=nrb), 0);

PCLa100 = PCLa*100
PCLb100 = PCLb*100
# 
# dev.new()
# plot(sSa, PCLa, type='b', pch=1 , col='blue',lty=1,
#       xlab='GOF score', ylab='% of map classes >= GOF score')
# lines(sSb, PCLb, type='b', pch=1 , col='red',lty=1)
# title(main='Mapcurves diagram')
# legend( list(x=0.7, y=0.98), legend=c('A as reference', 'B as reference'),
#         col=c('blue', 'red'), pch=c( 1, 1), lty=c(1,1) )
# 
# #dev.new()

p = ggplot(NULL) +
  geom_line(aes(x=PCLa100, y=sSa100, color='A'), lwd=1) + 
  geom_point(aes(x=PCLa100, y=sSa100, color='A'), size=2) +
  geom_area(aes(x=PCLa100, y=sSa100, fill='A'), alpha=0.2) +
  geom_line(aes(x=PCLb100, y=sSb100, color='B'), lwd=1) + 
  geom_point(aes(x=PCLb100, y=sSb100, color='B'), size=2) +
  geom_area(aes(x=PCLb100, y=sSb100, fill='B'), alpha=0.2) +
  scale_colour_discrete(name='Legend', breaks = c('A', 'B'), 
                        labels=c(paste0(nameB,' as Original, \n',nameA,' as Reference'), 
                                 paste0(nameA,' as Original, \n',nameB,' as Reference'))) +
  scale_fill_discrete(guide=FALSE) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.position="bottom") +
  theme(legend.title = element_blank(),
        legend.key = element_rect(size = 2),
        legend.key.size = unit(2, 'lines'),
        legend.background= element_rect(colour='black', linetype="solid")) +
  ggtitle(paste0('Mapcurves Plot ', nameA, ' vs. ', nameB)) +
  theme(plot.title = element_text(size = 25, hjust = 0.5)) + 
  xlab('Goodness of Fit (GoF)') + 
  theme(axis.title.x = element_text(size=20)) +
  theme(axis.text.x = element_text(size = 15)) +
  ylab('% Overlapping Categories >= this GoF') +
  theme(axis.title.y = element_text(size=20)) +
  theme(axis.text.y = element_text(size = 15))

# calculate surface under curve, when using
# respectively map A and map B
# calculation uses trapezium rule, which is 
# exact for piece-wise linear as in this case

#old versions, not correct
# GOFa = 0
# for ( i in 1:nra+1) {
#    GOFa = GOFa + ( sSa[i+1] - sSa[i] ) * PCLa[i+1]  + 
#               0.5 * ( sSa[i+1] - sSa[i] ) * ( PCLa[i] - PCLa[i+1] )
# }

# GOFb = 0
# for ( i in 1:nrb ) {
#   GOFb = GOFb + ( sSb[i+1] - sSb[i] ) * PCLb[i+1]  + 
#     0.5 * ( sSb[i+1] - sSb[i] ) * ( PCLb[i] - PCLb[i+1] )
# }

#updated versions
GOFa = 0
for ( i in 1:nra ) {
  GOFa = GOFa + sSa[i] * (PCLa[i] - PCLa[i+1])  + 
    0.5 * ( sSa[i+1] - sSa[i] ) * ( PCLa[i] - PCLa[i+1] )
}

GOFb = 0
for ( i in 1:nrb ) {
  GOFb = GOFb + sSb[i] * (PCLb[i] - PCLb[i+1])  + 
    0.5 * ( sSb[i+1] - sSb[i] ) * ( PCLb[i] - PCLb[i+1] )
}

# prepare data for output

if (GOFa >= GOFb){
   Refmap <- 'A'
   GOF <- GOFa
}

if (GOFa < GOFb){
   Refmap <- 'B'
   GOF <- GOFb
}

GOFtable <- round(rC,3)
rownames(GOFtable) <- a
colnames(GOFtable) <- b

vb <- vector(mode="integer",length=nra)
mg <- vector(mode="double",length=nra)
BMC_A2B <- data.frame(A=a,B=vb,mGOF=mg) 

va <- vector(mode="integer",length=nrb)
mg  <- vector(mode="double",length=nrb)
BMC_B2A <- data.frame(A=va,B=b,mGOF=mg) 

for (i in 1:nra){
    BMC_A2B$mGOF[i] = round( max( rC[i,]), 3)
    BMC_A2B$B[i] = b[ which.max( rC[i,] ) ]
}

for (j in 1:nrb){
    BMC_B2A$mGOF[j] = round( max( rC[,j]), 3)
    BMC_B2A$A[j] = a[ which.max( rC[,j] ) ]
}

return( list(p, GOF=GOF, GOFa=GOFa, GOFb=GOFb, Refmap=Refmap, GOFtable=GOFtable, 
              BMC_A2B=BMC_A2B, BMC_B2A=BMC_B2A ) )

}