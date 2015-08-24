
# Prepare raster for visualisation of the initial landscape.
setwd("/home/steve/Documents/GitHub/STModel-Wrapper/")

# Get list of files
files <- list.files("./outputs/15062015_CI_lands/",full.names = TRUE)

# load function
source("./fcts.r")

rs[] <- 0
st <- stack(rs,rs,rs,rs)
names(st) <- c("B","T","M","R")

# Loop over list of paths
for (i in 1:length(files)){
  # Get version and path
  cat("Processing...",files[i],".......\n")
  

  path <- files[i]
  
  # Load rs ref and create stack for output
  rs <- stmToRaster(path)
  maskNA <- reclassify(rs, c(1,4,1, 0,0,0))
  maskNA[maskNA==0] <- NA
  
  st$B <- mask(sum(st$B,reclassify(rs, c(1,1,1, 0,0,0, 2,4,0),include.lowest=TRUE,right=NA)),maskNA)
  st$T <- mask(sum(st$T,reclassify(rs, c(2,2,1, 0,1,0, 3,4,0),include.lowest=TRUE,right=NA)),maskNA)
  st$M <- mask(sum(st$M,reclassify(rs, c(3,3,1, 0,2,0, 4,4,0),include.lowest=TRUE,right=NA)),maskNA)
  st$R <- mask(sum(st$R,reclassify(rs, c(4,4,1, 0,3,0),include.lowest=TRUE,right=NA)),maskNA)
  
}

st <- st/length(files)
save(st,file="./outputs/analysis/stack_nStates_landCI.robj")
