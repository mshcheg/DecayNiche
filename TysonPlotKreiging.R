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
SurfaceList<- list( x= seq(290,490),y=seq(70,310) )
Surface<- make.surface.grid(SurfaceList)

soil.Al.kreig.prediction <- predict(soil.Al.kreig, Surface)
soil.Al.kreig.raster <- raster(soil.Al.kreig.prediction)
writeRaster(soil.Al.kreig.raster, file="Al_12.grd")

soil.base.saturation.kreig.prediction <- predict(soil.base.saturation.kreig, Surface)
soil.base.saturation.kreig.raster <- raster(soil.base.saturation.kreig.prediction)
writeRaster(soil.base.saturation.kreig.raster, file="BS_12.grd")

soil.calcium.kreig.prediction <- predict(soil.calcium.kreig, Surface)
soil.calcium.kreig.raster <- raster(soil.calcium.kreig.prediction)
writeRaster(soil.calcium.kreig.raster, file="Ca_12.grd")

soil.conductivity.kreig.prediction <- predict(soil.conductivity.kreig, Surface)
soil.conductivity.kreig.raster <- raster(soil.conductivity.kreig.prediction)
writeRaster(soil.conductivity.kreig.raster, file="ECEC_12.grd")

soil.iron.kreig.prediction <- predict(soil.iron.kreig, Surface)
soil.iron.kreig.raster <- raster(soil.iron.kreig.prediction)
writeRaster(soil.iron.kreig.raster, file="Fe_12.grd")

soil.Al.kreig.prediction <- predict(soil.Al.kreig, Surface)
soil.potassium.kreig.raster <- raster(soil.potassium.kreig.prediction)
writeRaster(soil.potassium.kreig.raster, file="K_12.grd")

soil.magnesium.kreig.prediction <- predict(soil.magnesium.kreig, Surface)
soil.magnesium.kreig.raster <- raster(soil.magnesium.kreig.prediction)
writeRaster(soil.magnesium.kreig.raster, file="Mg_12.grd")

soil.manganese.kreig.prediction <- predict(soil.manganese.kreig, Surface)
soil.manganese.kreig.raster <- raster(soil.manganese.kreig.prediction)
writeRaster(soil.manganese.kreig.raster, file="Mn_12.grd")

soil.sodium.kreig.prediction <- predict(soil.sodium.kreig, Surface)
soil.sodium.kreig.raster <- raster(soil.sodium.kreig.prediction)
writeRaster(soil.sodium.kreig.raster, file="Na_12.grd")

soil.ammonium.kreig.prediction <- predict(soil.ammonium.kreig, Surface)
soil.ammonium.kreig.raster <- raster(soil.ammonium.kreig.prediction)
writeRaster(soil.ammonium.kreig.raster, file="NH4_12.grd")

soil.ammonium.mineralization.kreig.prediction <- predict(soil.ammonium.mineralization.kreig, Surface)
soil.ammonium.mineralization.kreig.raster <- raster(soil.ammonium.mineralization.kreig.prediction)
writeRaster(soil.ammonium.mineralization.kreig.raster, file="NH4min_12.grd")

soil.nitrogen.10.kreig.prediction <- predict(soil.nitrogen.10.kreig, Surface)
soil.nitrogen.10.kreig.raster <- raster(soil.nitrogen.10.kreig.prediction)
writeRaster(soil.nitrogen.10.kreig.raster, file="NminTot_12.grd")

soil.nitrate.kreig.prediction <- predict(soil.nitrate.kreig, Surface)
soil.nitrate.kreig.raster <- raster(soil.nitrate.kreig.prediction)
writeRaster(soil.nitrate.kreig.raster, file="NO3_12.grd")

soil.nitrate.mineralization.kreig.prediction <- predict(soil.nitrate.mineralization.kreig, Surface)
soil.nitrate.mineralization.kreig.raster <- raster(soil.nitrate.mineralization.kreig.prediction)
writeRaster(soil.nitrate.mineralization.kreig.raster, file="NO3min_12.grd")

soil.nitrogen.kreig.prediction <- predict(soil.nitrogen.kreig, Surface)
soil.nitrogen.kreig.raster <- raster(soil.nitrogen.kreig.prediction)
writeRaster(soil.nitrogen.kreig.raster, file="Ntot_12.grd")

soil.phosphorous.kreig.prediction <- predict(soil.phosphorous.kreig, Surface)
soil.phosphorous.kreig.raster <- raster(soil.phosphorous.kreig.prediction)
writeRaster(soil.phosphorous.kreig.raster, file="P_12.grd")

soil.pH.CaCl2.kreig.prediction <- predict(soil.pH.CaCl2.kreig, Surface)
soil.pH.CaCl2.kreig.raster <- raster(soil.pH.CaCl2.kreig.prediction)
writeRaster(soil.pH.CaCl2.kreig.raster, file="pHCaCl2_12.grd")

soil.pH.H2O.kreig.prediction <- predict(soil.pH.H2O.kreig, Surface)
soil.pH.H2O.kreig.raster <- raster(soil.pH.H2O.kreig.prediction)
writeRaster(soil.pH.H2O.kreig.raster, file="pHWater_12.grd")

soil.bases.kreig.prediction <- predict(soil.bases.kreig, Surface)
soil.bases.kreig.raster <- raster(soil.bases.kreig.prediction)
writeRaster(soil.bases.kreig.raster, file="TEB_12.grd")

soil.aspect.kreig.prediction <- predict(soil.aspect.kreig, Surface)
soil.aspect.kreig.raster <- raster(soil.aspect.kreig.prediction)
writeRaster(soil.aspect.kreig.raster, file="aspect.grd")

soil.convexity.kreig.prediction <- predict(soil.convexity.kreig, Surface)
soil.convexity.kreig.raster <- raster(soil.convexity.kreig.prediction)
writeRaster(soil.convexity.kreig.raster, file="convexity.grd")

soil.elevation.kreig.prediction <- predict(soil.elevation.kreig, Surface)
soil.elevation.kreig.raster <- raster(soil.elevation.kreig.prediction)
writeRaster(soil.elevation.kreig.raster, file="elev_mean.grd")

soil.slope.kreig.prediction <- predict(soil.slope.kreig, Surface)
soil.slope.kreig.raster <- raster(soil.slope.kreig.prediction)
writeRaster(soil.slope.kreig.raster, file="slope.grd")





