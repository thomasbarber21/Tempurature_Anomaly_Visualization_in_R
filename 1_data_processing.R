# ---------------------------------------------------------------
# Description: go from raster climate to population-weighted climate 
# at the country-level
# ---------------------------------------------------------------

# ---------------------------------------------------------------
# 1. Preliminary stuff (directories and packages)
# ---------------------------------------------------------------

# Clean up workspace and load or install necessary packages if necessary
rm(list=ls())
want <- c("sf","terra","RColorBrewer","Matrix","tmaptools","rworldmap", "av", "magick")
need <- want[!(want %in% installed.packages()[,"Package"])]
if (length(need)) install.packages(need)
lapply(want, function(i) require(i, character.only=TRUE))
rm(want, need)

# Working directories
dir <- list()
dir$root <- dirname(getwd())
dir$weather   <- paste(dir$root,"/data/weather",sep="")
dir$pop <- paste(dir$root,"/data/population",sep="")


# ---------------------------------------------------------------
# 2. Load data 
# ---------------------------------------------------------------

# Raster - elevation 
e <- rast(list.files(dir$weather, full.names=T)[1])
id <- land <- e
land[] <- ifelse(e[]>0,1,NA)
id[] <- ifelse(e[]>0,1:ncell(e),NA)
plot(e, main="Elevation")
plot(land, main="Land indicator")
plot(id, main="ID = original cell position")

# Raster - climate
s <- rast(list.files(dir$weather, full.names=T)[2])

# Raster population
fname <- list.files(dir$pop, full.names=T, recursive = T)
fname <- fname[grepl("[.]tif",fname)]
p <- rast(fname)


# Shapefiles
co <- st_as_sf(countriesLow) 

# polygons are only overlaid on the Eastern hemisphere

rland <- terra::rotate(land)
rid   <- terra::rotate(id)

plot(rland, main="land indicator in recentered raster")
plot(vect(co), add=T) 

plot(rid)
plot(vect(co), add=T) 

newid <- rid
newid[] <- ifelse(rland[]==1,1:ncell(e),NA)

# These are different
plot(newid)
plot(rid)

# Check it out
matchid <- data.frame(original.id=c(rid[]), new.id=c(newid[]))
matchid <- matchid[!is.na(matchid[,1]),] # drop rows with NAs
head(matchid)

table(apply(matchid, 1, diff)) 

# ---------------------------------------------------------------
# 3. Aggregation of raster data in time (monthly to annual)
# ---------------------------------------------------------------

# Test run the code for a smaller set of years
if (F) {
  t <- s[[1:(12*10)]]
  years <- 1901:(1901+9)#1901:2012
  index <- rep(years, each=12)
  system.time({ 
    t2 <- tapp(t, index=index, fun=mean)
  })
}

# Create year index for full data
years <- 1901:2012
index <- rep(years, each=12)

# Perform annual aggregation
system.time({
  s2 <- tapp(s, index, fun=mean)
})

# ---------------------------------------------------------------
# 4. Aggregation of raster data in space
# ---------------------------------------------------------------

# 4.1 Obtain aggregation weights in matrix form -----
co$ID <- 1:nrow(co)
co.info <- st_drop_geometry(co[,c("ID","ISO3","ADMIN")])
info <- extract(newid, vect(co), weights=TRUE, cells=TRUE, exact=TRUE)
info <- merge(info, co.info)
dens <- p/5
dens2 <- resample(dens, newid, method="bilinear")
info2 <- lapply(as.character(unique(info$ISO3)), function(coname) {
  
  print(coname) 
  
  df <- info[info$ISO3==coname,]
  
  x <- dens2[df[,"cell"]]
  
  x <- x/sum(x,na.rm=T)
  
  df$popweight <- unlist(x)
  
  df <- df[!is.na(df$popweight),, drop=F]
  
  df <- df[,c("ID","cell","popweight")]
  
})

names(info2) <- as.character(unique(info$ISO3))

info2 <- info2[-c(which(sapply(info2, nrow)==0))]

for (i in 1:length(info2)) {
  info2[[i]]$countryorder <- i
}

temp <- do.call("rbind", info2)

P <- sparseMatrix(i=temp$countryorder,
                  j=temp$cell,
                  x=temp$popweight,
                  dims = c(length(info2),ncell(s)))

rownames(P) <- names(info2)

rowSums(P)

# 4.2 Perform aggregation ----

M <- s2[]
Mco <- P %*% M
d <- data.frame(ISO3=rownames(Mco), as.matrix(Mco))

# 4.3 Combine it with a map/shapefile ----

co <- merge(co, d, by="ISO3")

# The end