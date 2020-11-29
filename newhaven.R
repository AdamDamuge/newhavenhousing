#
# Group final explore & model
#
# 8/18/2020
#

x <- read.csv("NH_last.csv", as.is = TRUE)
summary(x)

x$logassessed <- log(x$assessed)

########################Categorical & Quantitative############################
# 1. yearbuilt

table(x$yearbuilt)
plot(logassessed ~ yearbuilt, data = x)
# quantitative
lm.yearbuilt <- lm(logassessed ~ yearbuilt, data = x)
summary(lm.yearbuilt)


# 2. pctgood
table(x$pctgood)
# pctgood no categorization eg:68 has 8331
summary(x$pctgood)
plot(logassessed ~ pctgood, data = x)
# quantitative


# 3. grade
table(x$grade)
plot(logassessed ~ grade, data = x) # infinite value, fail
# linear model:
x$grade.f <- factor(x$grade, c("Other", "BelowAverage", "Average", "Ave/Good",
                               "AboveAverage", "Good", "VeryGood", "Excellent", 
                               "Excellent+"))  
levels(x$grade.f )
nlevels(x$grade.f )

# BelowAverage is set as Intercept, the controls after the period are all positive
x$grade.f <- relevel(x$grade.f, "BelowAverage") 

lm.grade <- lm(logassessed ~ grade.f, data = x)
summary(lm.grade)   # R^2 = 0.4736, p-value: < 2.2e-16
# log: R^2 = 0.3582
plot(logassessed ~ grade.f, data = x)
# categorical


# 4. neighborhood
table(x$neighborhood)
plot(logassessed ~ neighborhood, data = x)

x$neighborhood.f <- factor(x$neighborhood)
levels(x$neighborhood.f )
nlevels(x$neighborhood.f )

# 2000 is set as Intercept, the controls after the period are all positive
x$neighborhood.f <- relevel(x$neighborhood.f, "800") 

lm.neighborhood <- lm(logassessed ~ neighborhood.f, data = x)
summary(lm.neighborhood)   # R^2 = 0.5969, < 2.2e-16
# log: R^2 = 0.5324
plot(logassessed ~ neighborhood.f, data = x)
# categorical


# 5.zone
table(x$zone)
plot(logassessed ~ zone, data = x) # infinite value, fail

# linear model:
x$zone.f <- factor(x$zone)   # Special data structure!
levels(x$zone.f )
nlevels(x$zone.f )

# PDU75 is set as Intercept, the controls after the period are all positive
x$zone.f <- relevel(x$zone.f, "BB/RS2")

lm.zone <- lm(logassessed ~ zone.f, data = x)
summary(lm.zone)   # R^2  = 0.3154, p-value: < 2.2e-16
# log: R^2 =  0.3154
plot(logassessed ~ zone.f, data = x)
# categorical


# 6.style
table(x$style)
plot(logassessed ~ style, data = x)  # infinite value, fail

# linear model:
x$style.f <- factor(x$style)   # Special data structure!
levels(x$style.f )
nlevels(x$style.f )

# Other can't make any sense, so choose Bungalow
# Bungalow is set as Intercept, the controls after the period are all positive
x$style.f <- relevel(x$style.f, "Bungalow")

lm.style <- lm(logassessed ~ style.f, data = x)
summary(lm.style)   # R^2 = 0.1002, p-value: < 2.2e-16
# log: R^2 = 0.174
plot(logassessed ~ style.f, data = x)
# categorical


# 7.livingarea
table(x$livingarea)
plot(logassessed ~ livingarea, data = x)
plot(logassessed ~ log(livingarea), data = x)
# log
# quantitative

# why plot has two masses?
plot(logassessed ~ log(livingarea), data = x, 
     col = factor(x$neighborhood))
plot(logassessed ~ factor(neighborhood), data = x)

# maybe neigborhood ?

a <- data.frame(x$pid, x$assessed, x$logassessed, x$neighborhood, x$livingarea)


a$x.neighborhood[which(x$neighborhood %in% 
                       c("1000", "101", "1100", "1650", "1700", "1800", "1900",
                         "200", "2000", "2200", "2300", "2400", "2500", "2600",
                         "2700", "2800", "2900", "300", 
                         "500", "600", "700", "900"))] <- 'Other'

plot(x.logassessed ~ log(x.livingarea), data = a, 
     col = factor(a$x.neighborhood), cex = 0.3)

legend("top", legend = unique(factor(a$x.neighborhood)), 
       ncol=5, cex=0.8, bty="n", col = unique(factor(a$x.neighborhood)), 
       lty=1,lwd=4)

# Neighhood's box plot range is large near the upper 
# This is a important valuable


# 8. description
table(x$description)
plot(logassessed ~ description, data = x)  # infinite value, fail

# linear model:
x$description.f <- factor(x$description)   # Special data structure!
levels(x$description.f )
nlevels(x$description.f )

# Condominium  ois set as Intercept, the controls after the period are all positive
x$description.f <- relevel(x$description.f, "Condominium")


lm.description <- lm(logassessed ~ description.f, data = x)
summary(lm.description)   # R^2 = 0.02603, p-value: < 2.2e-16
                          # log: R^2 = 0.151
plot(logassessed ~ description.f, data = x)
# categorical


# 9.landsize
table(x$landsize)
plot(logassessed ~ landsize, data = x)
# quantitative


# 10. occupancy
table(x$occupancy)
plot(logassessed ~ occupancy, data = x)

# linear model:
x$occupancy.f <- factor(x$occupancy)   # Special data structure!
levels(x$occupancy.f )
nlevels(x$occupancy.f )

# 1 is set as Intercept, the controls after the period are all positive
x$occupancy.f <- relevel(x$occupancy.f, "1")

lm.occupancy <- lm(logassessed ~ occupancy.f, data = x)
summary(lm.occupancy)   # R^2 = 0.02754, p-value: < 2.2e-16
                         # log: R^2 = 0.04641
plot(logassessed ~ occupancy.f, data = x)
# categorical


# 11. heattype
table(x$heattype)
plot(logassessed ~ heattype, data = x)  # infinite value, fail

# linear model:
x$heattype.f <- factor(x$heattype)   # Special data structure!
levels(x$heattype.f )
nlevels(x$heattype.f )

lm.heattype <- lm(logassessed ~ heattype.f, data = x)
summary(lm.heattype)   # R^2 = 0.01928, p-value: < 2.2e-16
                       # log: R^2 = 0.06357

plot(logassessed ~ heattype.f, data = x)
# categorical


# 12.actype
table(x$actype)
plot(logassessed ~ actype, data = x)  # infinite value, fail

# linear model:
x$actype.f <- factor(x$actype)   # Special data structure!
levels(x$actype.f )
nlevels(x$actype.f )

x$actype.f <- relevel(x$actype.f, "Unit/AC")

lm.actype <- lm(logassessed ~ actype.f, data = x)
summary(lm.actype)   # R^2 = 0.03459, p-value: < 2.2e-16
# log: R^2 = 0.02674
plot(logassessed ~ actype.f, data = x)
# categorical

# 13. bedrooms
table(x$bedrooms)
plot(logassessed ~ bedrooms, data = x)

# linear model:
x$bedrooms.f <- factor(x$bedrooms)   # Special data structure!
levels(x$bedrooms.f )
nlevels(x$bedrooms.f )

# 2 is set as Intercept, the controls after the period are all positive
x$bedrooms.f <- relevel(x$bedrooms.f, "2")

lm.bedrooms <- lm(logassessed ~ bedrooms.f, data = x)
summary(lm.bedrooms)   #  R^2 = 0.08139, p-value: < 2.2e-16
                       # log: R^2 = 0.1858

plot(logassessed ~ bedrooms.f, data = x)
# categorical


# 14.bathrooms
table(x$bathrooms)
plot(logassessed ~ bathrooms, data = x)
# quantitative


# 15.halfbathrooms
table(x$halfbathrooms)
plot(logassessed ~ halfbathrooms, data = x)
# quantitative


# 16. allrooms
table(x$allrooms)
plot(logassessed ~ allrooms, data = x)
# quantitative


# 17.replacement
table(x$replacement)
plot(logassessed ~ log(replacement), data = x)
# quantitative


# 18. model
table(x$model)
plot(logassessed ~ model, data = x)  # infinite value, fail

# linear model:
x$model.f <- factor(x$model)   # Special data structure!
levels(x$model.f )
nlevels(x$model.f )
 
x$model.f <- relevel(x$model.f, "ResCondo ")
 
lm.model <- lm(logassessed ~ model.f, data = x)
summary(lm.model)   # R^2 = 0.03609,  p-value: < 2.2e-16
                    # log: R^2 = 0.1201
plot(logassessed ~ model.f, data = x)
# categrocial

###########################Select Model#######################################

# Part 1
# occupancy, description, model are the same thing
table(x$occupancy)
table(x$description)
table(x$model)

# description is more detail
# choose description

# Part 2
# choose variable

## 1.yearbuilt

lm.full1 <- lm(logassessed ~ yearbuilt + pctgood + grade.f + neighborhood.f +
                 zone.f + style.f + log(livingarea) + description.f + 
                 sqrt(landsize) + heattype.f + actype.f +
                 bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1) # Adjusted R-squared:  0.9404  
                  # Residual standard error: 0.1389
hist(lm.full1$residuals)
qqnorm(lm.full1$residuals)

plot(lm.full1$residuals ~ lm.full1$fitted.values)

# Remove yearbuilt
lm.full1.1 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                 zone.f + style.f + log(livingarea) + description.f + 
                 sqrt(landsize) + heattype.f + actype.f +
                 bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1.1) # Adjusted R-squared:  0.9404 
                    # Residual standard error: 0.1391

hist(lm.full1.1$residuals)
qqnorm(lm.full1.1$residuals)

plot(lm.full1.1$residuals ~ lm.full1.1$fitted.values)

# Removing yearbuilt does not affect the model 
# yearbulit is not required.

## 2.pctgood

lm.full1 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                 zone.f + style.f + log(livingarea) + description.f + 
                 sqrt(landsize) + heattype.f + actype.f +
                 bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1) # Adjusted R-squared:  0.9404 
                  # Residual standard error: 0.1391

hist(lm.full1$residuals)
qqnorm(lm.full1$residuals)

plot(lm.full1$residuals ~ lm.full1.1$fitted.values)

# Remove pctgood
lm.full1.2 <- lm(logassessed ~ grade.f + neighborhood.f +
                   zone.f + style.f + log(livingarea) + description.f + 
                   sqrt(landsize) + heattype.f + actype.f +
                   bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1.2) # Adjusted R-squared:  0.9166
                    # Residual standard error: 0.1644 
hist(lm.full1.2$residuals)
qqnorm(lm.full1.2$residuals)

plot(lm.full1.2$residuals ~ lm.full1.2$fitted.values)

# Removing pctgood affect the model 
# pctgood is required.


## 3.grade

lm.full1 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                 zone.f + style.f + log(livingarea) + description.f + 
                 sqrt(landsize) + heattype.f + actype.f +
                 bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1) # Adjusted R-squared:  0.9404 
                  # Residual standard error: 0.1391

hist(lm.full1$residuals)
qqnorm(lm.full1$residuals)

plot(lm.full1$residuals ~ lm.full1$fitted.values)

# Remove grade
lm.full1.3 <- lm(logassessed ~  pctgood + neighborhood.f +
                   zone.f + style.f + log(livingarea) + description.f + 
                   sqrt(landsize) + heattype.f + actype.f +
                   bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1.3) # Adjusted R-squared:  0.9336 
                    # Residual standard error: 0.1468
hist(lm.full1.3$residuals)
qqnorm(lm.full1.3$residuals)

plot(lm.full1.3$residuals ~ lm.full1.3$fitted.values)

# Removing grade affect the model 
# grade is required.


## 4.zone

lm.full1 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                 zone.f + style.f + log(livingarea) + description.f + 
                 sqrt(landsize) + heattype.f + actype.f +
                 bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1) # Adjusted R-squared:  0.9404 
                  # Residual standard error: 0.1391

hist(lm.full1$residuals)
qqnorm(lm.full1$residuals)

plot(lm.full1$residuals ~ lm.full1$fitted.values)

# Remove zone
lm.full1.4 <- lm(logassessed ~  pctgood + grade.f + neighborhood.f +
                   style.f + log(livingarea) + description.f + 
                   sqrt(landsize) + heattype.f + actype.f +
                   bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1.4) # Adjusted R-squared:  0.9315
                    # Residual standard error: 0.1491

hist(lm.full1.4$residuals)
qqnorm(lm.full1.4$residuals)

plot(lm.full1.4$residuals ~ lm.full1.4$fitted.values)

# Removing zone affect the model 
# zone is required.

## 5.style

lm.full1 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                 zone.f + style.f + log(livingarea) + description.f + 
                 sqrt(landsize) + heattype.f + actype.f +
                 bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1) # Adjusted R-squared:  0.9404 
                  # Residual standard error: 0.1391

hist(lm.full1$residuals)
qqnorm(lm.full1$residuals)

plot(lm.full1$residuals ~ lm.full1$fitted.values)

# Remove style
lm.full1.5 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                   zone.f + log(livingarea) + description.f + 
                   sqrt(landsize) + heattype.f + actype.f +
                   bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1.5) # Adjusted R-squared:  0.9386
                    # Residual standard error: 0.1411

hist(lm.full1.5$residuals)
qqnorm(lm.full1.5$residuals)

plot(lm.full1.5$residuals ~ lm.full1.5$fitted.values)

# Removing style affect the model 
# style is require.


## 6.livingarea

lm.full1 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                 zone.f + style.f + log(livingarea) + description.f + 
                 sqrt(landsize) + heattype.f + actype.f +
                 bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1) # Adjusted R-squared:  0.9404 
                  # Residual standard error: 0.1391

hist(lm.full1$residuals)
qqnorm(lm.full1$residuals)

plot(lm.full1$residuals ~ lm.full1$fitted.values)

# Remove livingarea
lm.full1.6 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                   zone.f + style.f + description.f + 
                   sqrt(landsize) + heattype.f + actype.f +
                   bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1.6) # Adjusted R-squared:  0.9267
                    # Residual standard error: 0.1542 

hist(lm.full1.6$residuals)
qqnorm(lm.full1.6$residuals)

plot(lm.full1.6$residuals ~ lm.full1.6$fitted.values)

# Removing livingarea affect the model 
# livingarea is required.


## 7. description

lm.full1 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                 zone.f + style.f + log(livingarea) + description.f + 
                 sqrt(landsize) + heattype.f + actype.f +
                 bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1) # Adjusted R-squared:  0.9404 
                  # Residual standard error: 0.1391

hist(lm.full1$residuals)
qqnorm(lm.full1$residuals)

plot(lm.full1$residuals ~ lm.full1$fitted.values)

# Remove description
lm.full1.7 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                   zone.f + style.f + log(livingarea) + 
                   sqrt(landsize) + heattype.f + actype.f +
                   bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1.7) # Adjusted R-squared:  0.9403
                    # Residual standard error: 0.1392

hist(lm.full1.7$residuals)
qqnorm(lm.full1.7$residuals)

plot(lm.full1.7$residuals ~ lm.full1.7$fitted.values)

# Removing description affect the model 
# description is required.


## 8.landsize

lm.full1 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                 zone.f + style.f + log(livingarea) + description.f + 
                 sqrt(landsize) + heattype.f + actype.f +
                 bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1) # Adjusted R-squared:  0.9404 
                  # Residual standard error: 0.1391

hist(lm.full1$residuals)
qqnorm(lm.full1$residuals)

plot(lm.full1$residuals ~ lm.full1$fitted.values)

# Remove landsize
lm.full1.8 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                   zone.f + style.f + log(livingarea) + 
                   description.f + heattype.f + actype.f +
                   bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1.8) # Adjusted R-squared:  0.9397 
                    # Residual standard error: 0.1399

hist(lm.full1.8$residuals)
qqnorm(lm.full1.8$residuals)

plot(lm.full1.8$residuals ~ lm.full1.8$fitted.values)

# Removing landsize affect the model 
# landsize is required.

## 9.heattype

lm.full1 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                 zone.f + style.f + log(livingarea) + description.f + 
                 sqrt(landsize) + heattype.f + actype.f +
                 bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1) # Adjusted R-squared:  0.9404 
# Residual standard error: 0.1391

hist(lm.full1$residuals)
qqnorm(lm.full1$residuals)

plot(lm.full1$residuals ~ lm.full1$fitted.values)

# Remove heattype
lm.full1.9 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                   zone.f + style.f + log(livingarea) + 
                   description.f + sqrt(landsize)+ actype.f +
                   bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1.9) # Adjusted R-squared:  0.9379
                    # Residual standard error: 0.1419

hist(lm.full1.9$residuals)
qqnorm(lm.full1.9$residuals)

plot(lm.full1.9$residuals ~ lm.full1.9$fitted.values)

# Removing heattype affect the model 
# heattype is required.


## 10.actype

lm.full1 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                 zone.f + style.f + log(livingarea) + description.f + 
                 sqrt(landsize) + heattype.f + actype.f +
                 bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1) # Adjusted R-squared:  0.9404 
                  # Residual standard error: 0.1391

hist(lm.full1$residuals)
qqnorm(lm.full1$residuals)

plot(lm.full1$residuals ~ lm.full1$fitted.values)

# Remove actype
lm.full1.10 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                   zone.f + style.f + log(livingarea) + 
                   description.f + sqrt(landsize)+ heattype.f +
                   bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1.10) # Adjusted R-squared:  0.9402 
                     # Residual standard error: 0.1392

hist(lm.full1.10$residuals)
qqnorm(lm.full1.10$residuals)

plot(lm.full1.10$residuals ~ lm.full1.10$fitted.values)

# Removing actype affect the model 
# actype is required.



## 14. replacement

lm.full1 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                 zone.f + style.f + log(livingarea) + description.f + 
                 sqrt(landsize) + heattype.f + actype.f +
                 bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1) # Adjusted R-squared:  0.9401
# Residual standard error: 0.1394

hist(lm.full1$residuals)
qqnorm(lm.full1$residuals)

plot(lm.full1$residuals ~ lm.full1$fitted.values)

# Remove replacement
lm.full1.14 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                    zone.f + style.f + log(livingarea) + 
                    description.f + sqrt(landsize)+ heattype.f +
                    actype.f + bedrooms + bathrooms + halfbathrooms, data = x)
summary(lm.full1.14) # Adjusted R-squared:  0.9328 
# Residual standard error: 0.1477

hist(lm.full1.14$residuals)
qqnorm(lm.full1.14$residuals)

plot(lm.full1.14$residuals ~ lm.full1.14$fitted.values)

# Removing replacement do not affect the model 
# replacement is not required.



## 11.bedrooms

lm.full1 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                 zone.f + style.f + log(livingarea) + description.f + 
                 sqrt(landsize) + heattype.f + actype.f +
                 bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1) # Adjusted R-squared:  0.9404 
                  # Residual standard error: 0.1391

hist(lm.full1$residuals)
qqnorm(lm.full1$residuals)

plot(lm.full1$residuals ~ lm.full1$fitted.values)

# Remove bedrooms
lm.full1.11 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                    zone.f + style.f + log(livingarea) + 
                    description.f + sqrt(landsize)+ heattype.f +
                    actype.f + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1.11) # Adjusted R-squared:  0.9404 
                     # Residual standard error: 0.1391

hist(lm.full1.11$residuals)
qqnorm(lm.full1.11$residuals)

plot(lm.full1.11$residuals ~ lm.full1.11$fitted.values)

# Removing bedrooms does not affect the model 
# But bedrooms is required?


## 12. bathrooms

lm.full1 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                 zone.f + style.f + log(livingarea) + description.f + 
                 sqrt(landsize) + heattype.f + actype.f +
                 bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1) # Adjusted R-squared:  0.9404 
                  # Residual standard error: 0.1391

hist(lm.full1$residuals)
qqnorm(lm.full1$residuals)

plot(lm.full1$residuals ~ lm.full1$fitted.values)

# Remove bathrooms
lm.full1.12 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                    zone.f + style.f + log(livingarea) + 
                    description.f + sqrt(landsize)+ heattype.f +
                    actype.f + bedrooms + halfbathrooms + replacement, data = x)
summary(lm.full1.12) # Adjusted R-squared:  0.9404 
                     # Residual standard error: 0.1391

hist(lm.full1.12$residuals)
qqnorm(lm.full1.12$residuals)

plot(lm.full1.12$residuals ~ lm.full1.12$fitted.values)

# Removing bathrooms does not affect the model.
# But bathrooms is required ? 


## 13. halfbathrooms

lm.full1 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                 zone.f + style.f + log(livingarea) + description.f + 
                 sqrt(landsize) + heattype.f + actype.f +
                 bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1) # Adjusted R-squared:  0.9401
                  # Residual standard error: 0.1394

hist(lm.full1$residuals)
qqnorm(lm.full1$residuals)

plot(lm.full1$residuals ~ lm.full1$fitted.values)

# Remove halfbathrooms
lm.full1.13 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                    zone.f + style.f + log(livingarea) + 
                    description.f + sqrt(landsize)+ heattype.f +
                    actype.f + bedrooms + bathrooms + replacement, data = x)
summary(lm.full1.13) # Adjusted R-squared:  0.9404 
# Residual standard error: 0.1391

hist(lm.full1.13$residuals)
qqnorm(lm.full1.13$residuals)

plot(lm.full1.13$residuals ~ lm.full1.13$fitted.values)

# Removing halfbathrooms affect the model 
# halfbathrooms is required.


# Part 3
# bedrooms, bathrooms, halfbathrooms
# Will the three have a big impact on the model?

lm.full1 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                 zone.f + style.f + log(livingarea) + description.f + 
                 sqrt(landsize) + heattype.f + actype.f + 
                 bedrooms + bathrooms + halfbathrooms + replacement, data = x)
summary(lm.full1) # Adjusted R-squared:  0.9404 
# Residual standard error: 0.1391
hist(lm.full1.1$residuals)
qqnorm(lm.full1.1$residuals)

plot(lm.full1$residuals ~ lm.full1$fitted.values)

# Remove bedrooms, bathrooms, halfbathrooms
lm.full1.16 <- lm(logassessed ~  pctgood + neighborhood.f +
                    zone.f + style.f + log(livingarea) + description.f + 
                    sqrt(landsize) + heattype.f + actype.f +
                    replacement, data = x)
summary(lm.full1.16) # Adjusted R-squared:  0.9332 
# Residual standard error: 0.1473

hist(lm.full1.16$residuals)
qqnorm(lm.full1.16$residuals)

plot(lm.full1.16$residuals ~ lm.full1.16$fitted.values)
# Removing bedrooms, bathrooms, halfbathrooms affect the model 
# The three is required.




## 15.neighborhood

lm.full1 <- lm(logassessed ~ pctgood + grade.f + neighborhood.f +
                 zone.f + style.f + log(livingarea) + description.f + 
                 sqrt(landsize) + heattype.f + actype.f + 
                 bedrooms + bathrooms + halfbathrooms, data = x)
summary(lm.full1) # Adjusted R-squared:  0.9404 
# Residual standard error: 0.1391
hist(lm.full1.1$residuals)
qqnorm(lm.full1.1$residuals)

plot(lm.full1$residuals ~ lm.full1$fitted.values)

# Remove neighborhood
lm.full1.15 <- lm(logassessed ~  pctgood + 
                   zone.f + style.f + log(livingarea) + description.f + 
                   sqrt(landsize) + heattype.f + actype.f +
                   bedrooms + bathrooms + halfbathrooms , data = x)
summary(lm.full1.15) # Adjusted R-squared:  0.6848 
                     # Residual standard error: 0.3198

hist(lm.full1.15$residuals)
qqnorm(lm.full1.15$residuals)

plot(lm.full1.15$residuals ~ lm.full1.15$fitted.values)
# Removing grade affect the model 
# grade is required.

# the neighborhood is a important  variable


###############################Final model#####################################

m.full1 <- lm(logassessed ~  pctgood +grade.f + neighborhood.f +
                style.f + log(livingarea) + description.f + 
                sqrt(landsize) + heattype.f + actype.f +
                bedrooms + bathrooms + halfbathrooms, data = x)

summary(lm.full1) 
plot(lm.full1$residuals ~ lm.full1$fitted.values)

# Model check

# These aren't bad really!
hist(lm.full1$residuals)

par(mfrow = c(2,2))
plot(lm.full1, which = c(1:4))

# predict
x$fitted.values <- lm.full1$fitted.values
x$predicted <- predict(lm.full1)
x$residuals.calc <- x$assessed - x$predicted
x$residuals.model <- lm.full1$residual