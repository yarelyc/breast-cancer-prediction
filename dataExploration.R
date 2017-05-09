dat = read.csv("/Users/yarelychino/Desktop/breast-cancer-prediction/data.csv")
dat$X = NULL

#plot
plot(dat$texture_mean, dat$radius_mean, col = "blue", main = "Radius by Texture")
malignant = dat$diagnosis == "M"
points(dat[malignant, ]$texture_mean, dat[malignant, ]$radius_mean, col = "red")

#radius mean
par(mfrow=c(2,5))
plot(density(dat$radius_mean))
plot(density(dat$texture_mean))
plot(density(dat$perimeter_mean))
plot(density(dat$area_mean))
plot(density(dat$smoothness_mean))
plot(density(dat$compactness_mean))
plot(density(dat$concavity_mean))
plot(density(dat$concave.points_mean))
plot(density(dat$symmetry_mean))
plot(density(dat$fractal_dimension_mean))