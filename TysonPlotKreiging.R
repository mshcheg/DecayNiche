coordinateGrid <- read.csv("~/DecayNiche/oberleb-cwd-5a4e8ea6676d/Data/TRCP_Coordinates_Grid_20131004.csv")
CWD.data <- read.csv("~/DecayNiche/oberleb-cwd-5a4e8ea6676d/Data/2012.CWD.clean.v0.5.csv")


CWD.plot <- coordinateGrid[which(coordinateGrid$gx > 280),]
CWD.plot <- CWD.plot[which(CWD.plot$gy > 65 & CWD.plot$gy < 320 ),]

library(LatticeKrig)
#CWD.coords <- CWD.plot[,c("gx","gy")]
#elevation.kreig <- LatticeKrig(CWD.coords, CWD.plot$elev)
#surface( obj )

soil.data <- read.csv("~/DecayNiche/oberleb-cwd-5a4e8ea6676d/Data/TRCP_Env_20x20_20140217.csv")
CWD.soil <- soil.data[which(soil.data$gx25ha > 280),]
CWD.soil <- CWD.soil[which(CWD.soil$gy25ha > 65 & CWD.soil$gy25ha < 320 ),]
soil.coords <- CWD.soil[,c("gx25ha","gy25ha")]


par(mfrow=c(4,6))
soil.Al.kreig <- LatticeKrig(soil.coords, CWD.soil$Al_12)
soil.base.saturation.kreig <- LatticeKrig(soil.coords, CWD.soil$BS_12)
soil.calcium.kreig <- LatticeKrig(soil.coords, CWD.soil$Ca_12)
soil.conductivity.kreig <- LatticeKrig(soil.coords, CWD.soil$ECEC_12)
soil.iron.kreig <- LatticeKrig(soil.coords, CWD.soil$Fe_12)
soil.potassium.kreig <- LatticeKrig(soil.coords, CWD.soil$K_12)
soil.magnesium.kreig <- LatticeKrig(soil.coords, CWD.soil$Mg_12)
soil.manganese.kreig <- LatticeKrig(soil.coords, CWD.soil$Mn_12)
soil.sodium.kreig <- LatticeKrig(soil.coords, CWD.soil$Na_12)
soil.ammonium.kreig <- LatticeKrig(soil.coords, CWD.soil$NH4_12)
soil.ammonium.mineralization.kreig <- LatticeKrig(soil.coords, CWD.soil$NH4min_12)
soil.nitrogen.10.kreig <- LatticeKrig(soil.coords, CWD.soil$NminTot_12)
soil.nitrate.kreig <- LatticeKrig(soil.coords, CWD.soil$NO3_12)
soil.nitrate.mineralization.kreig <- LatticeKrig(soil.coords, CWD.soil$NO3min_12)
soil.nitrogen.kreig <- LatticeKrig(soil.coords, CWD.soil$Ntot_12)
soil.phosphorous.kreig <- LatticeKrig(soil.coords, CWD.soil$P_12)
soil.pH.CaCl2.kreig <- LatticeKrig(soil.coords, CWD.soil$pHCaCl2_12)
soil.pH.H2O.kreig <- LatticeKrig(soil.coords, CWD.soil$pHWater_12)
soil.bases.kreig <- LatticeKrig(soil.coords, CWD.soil$TEB_12)
soil.aspect.kreig <- LatticeKrig(soil.coords, CWD.soil$aspect)
soil.convexity.kreig <- LatticeKrig(soil.coords, CWD.soil$convexity)
soil.elevation.kreig <- LatticeKrig(soil.coords, CWD.soil$elev_mean)
soil.slope.kreig <- LatticeKrig(soil.coords, CWD.soil$slope)

pdf("CWDplot_soilInterpolations.pdf", width=11, height=8)
par(mfrow=c(4,6))
surface( soil.Al.kreig, main="soil aluminum")
surface( soil.base.saturation.kreig, main="soil base saturation" )
surface( soil.calcium.kreig, main="soil calcium" )
surface( soil.conductivity.kreig, main="soil ECEC" )
surface( soil.iron.kreig, main="soil iron" )
surface( soil.potassium.kreig, main="soil potassium" )
surface( soil.magnesium.kreig, main="soil magnesium" )
surface( soil.manganese.kreig, main="soil manganese" )
surface( soil.sodium.kreig, main="soil sodium" )
surface( soil.ammonium.kreig, main="soil ammonium" )
surface( soil.ammonium.mineralization.kreig, main="soil ammonium min rate" )
surface( soil.nitrogen.10.kreig, main="soil total nitrogen min rate" )
surface( soil.nitrate.kreig, main="soil nitrate" )
surface( soil.nitrate.mineralization.kreig, main="soil nitrate min rate" )
surface( soil.nitrogen.kreig, main="soil total nitrogen" )
surface( soil.phosphorous.kreig, main="soil phosphorous" )
surface( soil.pH.CaCl2.kreig, main="soil pH in CaCl2" )
surface( soil.pH.H2O.kreig, main="soil pH in water" )
surface( soil.bases.kreig, main="soil TEB" )
surface( soil.aspect.kreig, main="aspect" )
surface( soil.convexity.kreig, main="convexity" )
surface( soil.elevation.kreig, main="mean elevation" )
surface( soil.slope.kreig, main="slope" )
dev.off()

library(raster)

soil.Al.kreig.prediction <- predictSurface(soil.Al.kreig)
soil.Al.kreig.raster <- raster(soil.Al.kreig.prediction)
writeRaster(soil.Al.kreig.raster, file="Al_12.grd", overwrite=T)

soil.base.saturation.kreig.prediction <- predictSurface(soil.base.saturation.kreig)
soil.base.saturation.kreig.raster <- raster(soil.base.saturation.kreig.prediction)
writeRaster(soil.base.saturation.kreig.raster, file="BS_12.grd", overwrite=T)

soil.calcium.kreig.prediction <- predictSurface(soil.calcium.kreig)
soil.calcium.kreig.raster <- raster(soil.calcium.kreig.prediction)
writeRaster(soil.calcium.kreig.raster, file="Ca_12.grd", overwrite=T)

soil.conductivity.kreig.prediction <- predictSurface(soil.conductivity.kreig)
soil.conductivity.kreig.raster <- raster(soil.conductivity.kreig.prediction)
writeRaster(soil.conductivity.kreig.raster, file="ECEC_12.grd", overwrite=T)

soil.iron.kreig.prediction <- predictSurface(soil.iron.kreig)
soil.iron.kreig.raster <- raster(soil.iron.kreig.prediction)
writeRaster(soil.iron.kreig.raster, file="Fe_12.grd", overwrite=T)

soil.potassium.kreig.prediction <- predictSurface(soil.potassium.kreig)
soil.potassium.kreig.raster <- raster(soil.potassium.kreig.prediction)
writeRaster(soil.potassium.kreig.raster, file="K_12.grd", overwrite=T)

soil.magnesium.kreig.prediction <- predictSurface(soil.magnesium.kreig)
soil.magnesium.kreig.raster <- raster(soil.magnesium.kreig.prediction)
writeRaster(soil.magnesium.kreig.raster, file="Mg_12.grd", overwrite=T)

soil.manganese.kreig.prediction <- predictSurface(soil.manganese.kreig)
soil.manganese.kreig.raster <- raster(soil.manganese.kreig.prediction)
writeRaster(soil.manganese.kreig.raster, file="Mn_12.grd", overwrite=T)

soil.sodium.kreig.prediction <- predictSurface(soil.sodium.kreig)
soil.sodium.kreig.raster <- raster(soil.sodium.kreig.prediction)
writeRaster(soil.sodium.kreig.raster, file="Na_12.grd", overwrite=T)

soil.ammonium.kreig.prediction <- predictSurface(soil.ammonium.kreig)
soil.ammonium.kreig.raster <- raster(soil.ammonium.kreig.prediction)
writeRaster(soil.ammonium.kreig.raster, file="NH4_12.grd", overwrite=T)

soil.ammonium.mineralization.kreig.prediction <- predictSurface(soil.ammonium.mineralization.kreig)
soil.ammonium.mineralization.kreig.raster <- raster(soil.ammonium.mineralization.kreig.prediction)
writeRaster(soil.ammonium.mineralization.kreig.raster, file="NH4min_12.grd", overwrite=T)

soil.nitrogen.10.kreig.prediction <- predictSurface(soil.nitrogen.10.kreig)
soil.nitrogen.10.kreig.raster <- raster(soil.nitrogen.10.kreig.prediction)
writeRaster(soil.nitrogen.10.kreig.raster, file="NminTot_12.grd", overwrite=T)

soil.nitrate.kreig.prediction <- predictSurface(soil.nitrate.kreig)
soil.nitrate.kreig.raster <- raster(soil.nitrate.kreig.prediction)
writeRaster(soil.nitrate.kreig.raster, file="NO3_12.grd", overwrite=T)

soil.nitrate.mineralization.kreig.prediction <- predictSurface(soil.nitrate.mineralization.kreig)
soil.nitrate.mineralization.kreig.raster <- raster(soil.nitrate.mineralization.kreig.prediction)
writeRaster(soil.nitrate.mineralization.kreig.raster, file="NO3min_12.grd", overwrite=T)

soil.nitrogen.kreig.prediction <- predictSurface(soil.nitrogen.kreig)
soil.nitrogen.kreig.raster <- raster(soil.nitrogen.kreig.prediction)
writeRaster(soil.nitrogen.kreig.raster, file="Ntot_12.grd", overwrite=T)

soil.phosphorous.kreig.prediction <- predictSurface(soil.phosphorous.kreig)
soil.phosphorous.kreig.raster <- raster(soil.phosphorous.kreig.prediction)
writeRaster(soil.phosphorous.kreig.raster, file="P_12.grd", overwrite=T)

soil.pH.CaCl2.kreig.prediction <- predictSurface(soil.pH.CaCl2.kreig)
soil.pH.CaCl2.kreig.raster <- raster(soil.pH.CaCl2.kreig.prediction)
writeRaster(soil.pH.CaCl2.kreig.raster, file="pHCaCl2_12.grd", overwrite=T)

soil.pH.H2O.kreig.prediction <- predictSurface(soil.pH.H2O.kreig)
soil.pH.H2O.kreig.raster <- raster(soil.pH.H2O.kreig.prediction)
writeRaster(soil.pH.H2O.kreig.raster, file="pHWater_12.grd", overwrite=T)

soil.bases.kreig.prediction <- predictSurface(soil.bases.kreig)
soil.bases.kreig.raster <- raster(soil.bases.kreig.prediction)
writeRaster(soil.bases.kreig.raster, file="TEB_12.grd", overwrite=T)

soil.aspect.kreig.prediction <- predictSurface(soil.aspect.kreig)
soil.aspect.kreig.raster <- raster(soil.aspect.kreig.prediction)
writeRaster(soil.aspect.kreig.raster, file="aspect.grd", overwrite=T)

soil.convexity.kreig.prediction <- predictSurface(soil.convexity.kreig)
soil.convexity.kreig.raster <- raster(soil.convexity.kreig.prediction)
writeRaster(soil.convexity.kreig.raster, file="convexity.grd", overwrite=T)

soil.elevation.kreig.prediction <- predictSurface(soil.elevation.kreig)
soil.elevation.kreig.raster <- raster(soil.elevation.kreig.prediction)
writeRaster(soil.elevation.kreig.raster, file="elev_mean.grd", overwrite=T)

soil.slope.kreig.prediction <- predictSurface(soil.slope.kreig)
soil.slope.kreig.raster <- raster(soil.slope.kreig.prediction)
writeRaster(soil.slope.kreig.raster, file="slope.grd", overwrite=T)

(RasterList <- (dir(path="SoilKreigRasters/", pattern="*.grd")))
RasterPath <- lapply(RasterList, function(x) paste("SoilKreigRasters/", x, sep =""))

CWD.EV <- stack(RasterPath)
names <- strsplit(RasterList, ".grd")
names(CWD.EV) <- names

CWD.EV.PCA <- princomp(na.omit(CWD.EV[]), cor=T)
res <- predict(CWD.EV.PCA, na.omit(CWD.EV[]))

