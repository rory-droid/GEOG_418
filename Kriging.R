f.0 <- as.formula(PM25AGG ~ 1) 

var.smpl <- variogram(f.0, spSample, cloud = FALSE) 


f.1 <- as.formula(PM25AGG ~ X + Y) 
f.2 <- as.formula(PM25AGG ~ X + Y + I(X*X)+I(Y*Y) + I(X*Y))

?variogram
var.smpl <- variogram(f.2, spSample, cloud = FALSE) #, cutoff=1000000, width=89900)
dat.fit  <- fit.variogram(var.smpl, fit.ranges = F, fit.sills = F,
                          vgm(model = "Exp"))
plot(var.smpl, dat.fit)


# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
dat.krg <- krige( f.0, spSample, grd, dat.fit)

# Convert kriged surface to a raster object for clipping
r_krig <- raster(dat.krg)
r_krig_mask <-mask(r_krig, income.tracts)
# Plot the map
tm_shape(r_krig_mask) + 
  tm_raster(n=10, palette="-RdBu",  
            title="Predicted pm25 \n(in ppm)") +
  #tm_shape(spSample) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

r   <- raster(dat.krg, layer="var1.var")
r.m <- mask(r, SC.AirBasin.t)

tm_shape(r.m) + 
  tm_raster(n=7, palette ="Reds",
            title="Variance map 
Spherical krig\n(in squared ppm)") +tm_shape(ozone.mean.spdf) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

r   <- sqrt(raster(dat.krg, layer="var1.var")) * 1.96
r.m <- mask(r, SC.AirBasin.t)

tm_shape(r.m) + 
  tm_raster(n=7, palette ="Reds",
            title="95% CI map \n(in ppm)") +tm_shape(ozone.mean.spdf) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)
