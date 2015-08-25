# Create Map 1 illustrating temperate and mixed community range shift over three times periods (Climatology, 2045, 2095)

# Set workspace and library
setwd("~/Documents/GitHub/ms_thesis/article/figs")
library('raster')
library("rgdal")
library("sp")
library("colorRamps")
library("rgeos")
library("RColorBrewer")

# Load data
load("/home/steve/Documents/GitHub/STModel-Wrapper/outputs/analysis/stack_nStates_local_2045.robj")
st_45 <- st
load("/home/steve/Documents/GitHub/STModel-Wrapper/outputs/analysis/stack_nStates_local_2095.robj")
st_95 <- st
load("/home/steve/Documents/GitHub/STModel-Wrapper/outputs/analysis/stack_nStates_local_2015.robj")
st_15 <- st

#load("/home/steve/Documents/GitHub/STModel-Wrapper/outputs/analysis/stack_nStates_landCI.robj ")
#st_landCI <- st

st_95 <- projectRaster(st_95,crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
st_45 <- projectRaster(st_45,crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
st_15 <- projectRaster(st_15,crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
#st_landCI <- projectRaster(st_landCI,crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# get shapefiles
CAN_border <- getData('GADM', country='CAN', level=1)
CAN_alt <- getData('alt', country='CAN', mask=TRUE)
US_alt <-  getData('alt', country='US', mask=TRUE)
US_border <-  getData('GADM', country='US', level=1)

load("/home/steve/Documents/GitHub/STModel-Wrapper/inputs/shp/shp_stm_area.rdata")

# Crop + 1 degree
CAN_border <- crop(CAN_border,extent(st_45)+1)
CAN_alt <- crop(CAN_alt,extent(st_45)+1)
US_border <- crop(US_border,extent(st_45)+1)
US_alt <- crop(US_alt[[1]],extent(st_45)+1)

# Merging shp and raster
border <- union(CAN_border,US_border)
alt <- merge(CAN_alt,US_alt)

# Mask NA
st_45 <- mask(st_45,border)
st_95 <- mask(st_95,border)
st_15 <- mask(st_15,border)
#st_landCI <- mask(st_landCI,border)

# Reproject to conic lambert
new_proj <- "+proj=lcc +lat_1=60 +lat_2=46 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
st_45 <- projectRaster(st_45, crs=new_proj)
st_95 <- projectRaster(st_95, crs=new_proj)
st_15 <- projectRaster(st_15, crs=new_proj)
#st_landCI <- projectRaster(st_landCI,crs=new_proj)
alt <- projectRaster(alt,crs=new_proj)
border <- spTransform(border,CRS(new_proj))
great_lakes <- spTransform(great_lakes,CRS(new_proj))

# Set params for figures
pal <-colorRampPalette(rev(brewer.pal(11,"RdYlBu")))
dem <-  colorRampPalette(c("grey95", "grey50","grey10","black"))
brk <- 100

#Spectral ou RdYlBu

png(file="./future_distrib_states.png",width = 10.5, height =7.5,units="in",res=300)
layout(matrix(c(1:4,5,5), 3,2 ,byrow=TRUE),height=c(1,1,0.4),TRUE)
par(mar=c(0.5,0.5,0.5,0.5),oma=c(1, 2, 1, 1))


plot(border,border=NA,col="grey70")
#image(alt,axes=FALSE,xlab="",ylab="",asp=1,col=dem(100))
image(st_15$T,axes=FALSE,xlab="",ylab=,asp=1,zlim=c(0.01,1),breaks=round(seq(0,1,length.out=brk),2),col=pal(brk-1),add=TRUE)
plot(great_lakes,lwd=0.4,col="white",border="grey20",add=TRUE)
plot(border,add=TRUE,lwd=0.6,border="grey20",col=NA)
llgridlines(border,cex=0.8,lty=3)

plot(border,border=NA,col="grey70")
#image(alt,axes=FALSE,xlab="",ylab="",asp=1,col=dem(100))
image(st_95$T,axes=FALSE,xlab="",ylab="",asp=1,zlim=c(0.01,1),breaks=round(seq(0,1,length.out=brk),2),col=pal(brk-1),add=TRUE)
plot(great_lakes,lwd=0.4,col="white",border="grey20",add=TRUE)
plot(border,add=TRUE,lwd=0.6,border="grey20",col=NA)
llgridlines(border,cex=0.8,lty=3)

plot(border,border=NA,col="grey70")
#image(alt,axes=FALSE,xlab="",ylab="",asp=1,col=dem(100))
image(st_15$M,axes=FALSE,xlab="",ylab="",asp=1,zlim=c(0.01,1),breaks=round(seq(0,1,length.out=brk),2),col=pal(brk-1),add=TRUE)
plot(great_lakes,lwd=0.4,col="white",border="grey20",add=TRUE)
plot(border,add=TRUE,lwd=0.6,border="grey20",col=NA)
llgridlines(border,cex=0.8,lty=3)

plot(border,border=NA,col="grey70")
#image(alt,axes=FALSE,xlab="",ylab="",asp=1,col=dem(100))
image(st_95$M,axes=FALSE,xlab="",ylab="",asp=1,zlim=c(0.01,1),breaks=round(seq(0,1,length.out=brk),2),col=pal(brk-1),add=TRUE)
plot(great_lakes,lwd=0.4,col="white",border="grey20",add=TRUE)
plot(border,add=TRUE,lwd=0.6,border="grey20",col=NA)
llgridlines(border,cex=0.8,lty=3)

# plot(border,border=NA,col="grey70")
# #image(alt,axes=FALSE,xlab="",ylab="",asp=1,col=dem(100))
# image(st_45$T,axes=FALSE,xlab="",ylab="",asp=1,zlim=c(0.01,1),breaks=round(seq(0,1,length.out=brk),2),col=pal(brk-1),add=TRUE)
# plot(great_lakes,lwd=0.4,col="white",border="grey20",add=TRUE)
# plot(border,add=TRUE,lwd=0.6,border="grey20",col=NA)
# llgridlines(border,cex=0.8,lty=3)
#
# plot(border,border=NA,col="grey70")
# #image(alt,axes=FALSE,xlab="",ylab="",asp=1,col=dem(100))
# image(st_45$M,axes=FALSE,xlab="",ylab="",asp=1,zlim=c(0.01,1),breaks=round(seq(0,1,length.out=brk),2),col=pal(brk-1),add=TRUE)
# plot(great_lakes,lwd=0.4,col="white",border="grey20",add=TRUE)
# plot(border,add=TRUE,lwd=0.6,border="grey20",col=NA)
# llgridlines(border,cex=0.8,lty=3)


mtext("2095", 3, -1,outer=TRUE,adj=0.77,font=2,cex=1.2)
mtext("2015", 3, -1,outer=TRUE,adj=0.23,font=2,cex=1.2)

mtext("Mixed", 2, 0,outer=TRUE,adj=0.37,font=2,cex=1.2)
#mtext("2045", 2, 0,outer=TRUE,adj=0.57,font=2,cex=1.2)
mtext("Temperate", 2, 0,outer=TRUE,adj=0.84,font=2,cex=1.2)

par(mar=c(4,18,4,5))
image(matrix(1:brk-1),col=pal(brk-1),axes=FALSE,ann=FALSE)
axis(1,at=seq(0,1,length.out=5),lab=as.character(round(seq(0,1,length.out=5),2)))
box(lwd=1.2)

mtext("Predicted state probability \n over all simulations",1,line=-0.65,at=-0.17,font=2)

dev.off()

# Create Map 2 illustrating te time lag (Climatology, 2045, 2095)
