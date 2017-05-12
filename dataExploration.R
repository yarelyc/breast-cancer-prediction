dat = read.csv("/Users/yarelychino/Desktop/breast-cancer-prediction/data.csv")
dat$X = NULL
dat$output = ifelse( dat$diagnosis == "M", 1, 0)

#radius mean
par(mfrow=c(2,5))
plot(density(dat$radius_se))
plot(density(dat$texture_se))
plot(density(dat$perimeter_se))
plot(density(dat$area_se))
plot(density(dat$smoothness_se))
plot(density(dat$compactness_se))
plot(density(dat$concavity_se))
plot(density(dat$concave.points_se))
plot(density(dat$symmetry_se))
plot(density(dat$fractal_dimension_se))


#plot Worst Area, Worst Smoothness
plot(dat$area_worst, dat$texture_mean, col = "blue", main = "Worst Area vs Worst Smoothness")
malignant = dat$diagnosis == "M"
points(dat[malignant, ]$area_worst, dat[malignant, ]$texture_mean, col = "red")



#plot Worst Area, Worst Smoothness
plot(dat$area_worst, dat$smoothness_mean, col = "blue", main = "Worst Area vs Worst Smoothness")
malignant = dat$diagnosis == "M"
points(dat[malignant, ]$area_worst, dat[malignant, ]$smoothness_mean, col = "red")


#plot Worst Area, Worst Smoothness
plot(dat$radius_worst, dat$texture_worst, col = "blue", main = "Worst Area vs Worst Smoothness")
malignant = dat$diagnosis == "M"
points(dat[malignant, ]$radius_worst, dat[malignant, ]$texture_worst, col = "red")


plot(dat$output ~ dat$radius_worst)

fit = glm(output ~ radius_worst, data=dat, family=binomial)

bp = data.frame(radius_worst=seq(5,40,10))
probs = predict(fit, newdata=bp, type="response")

plot(diagnosis ~ radius_worst, data=dat, col="red4",
     xlab="restbp", ylab="P(heart disease)", pch=20)
lines(bp$radius_worst, probs, col="green4", lwd=2)

par(mfrow=c(3,3))
plot(output ~ radius_se, data = dat)
plot(output ~ texture_se, data = dat)
plot(output ~ perimeter_se, data = dat)
plot(output ~ area_se, data = dat)
plot(output ~ smoothness_se, data = dat)
plot(output ~ compactness_se, data = dat)
plot(output ~ concavity_se, data = dat)
plot(output ~ concave.points_se, data = dat)
plot(output ~ symmetry_se, data = dat)
plot(output ~ fractal_dimension_se, data = dat)



# ######## radius_mean, perimeter_mean, area_mean, concavity.points_mean, area_worst, radius_worst, concavity.points_worst, texture_worse
# 
# heart = read.table("/Users/yarelychino/Downloads/heart.dat", quote = "/")
# names(heart) = c("AGE", "SEX", "CHESTPAIN", "RESTBP", "CHOL", "SUGAR", "ECG", "MAXHR", "ANGINA", "DEP", "EXERCISE", "FLUOR", "THAL", "OUTPUT")
# names(heart) = tolower(names(heart))
# heart$chestpain = factor(heart$chestpain)
# heart$output = heart$output - 1    # convert to 0-1 

# 
# fit = glm(output ~ restbp, data=heart, family=binomial)
# # plot the result
# bp = data.frame(restbp=seq(80,200,10))
# probs = predict(fit, newdata=bp, type="response")
# plot(output ~ restbp, data=heart, col="red4",
#      xlab="restbp", ylab="P(heart disease)", pch=20)
# lines(bp$restbp, probs, col="green4", lwd=2)
# 
# 
# 
# 




