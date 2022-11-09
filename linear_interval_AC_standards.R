# Dayrell RLC, Cawthray GR, Lambers H, Ranathunge K. (2022) Using activated charcoal to remove substances interfering with the colorimetric assay of inorganic phosphate in plant extracts. Plant and Soil, 476:755–764.           

## Visual assessment of the linear interval of the calibration curve of AC treated standards compared to a nonlinear model.

data=read.csv("raw.curve.csv")

require(reshape2)
require(drc)

data.mean=aggregate(data[,4],by=list(data$treatment,data$concentration),mean)

data.sd=aggregate(data[,4],by=list(data$treatment,data$concentration),sd)

data.sum= merge(data.mean, data.sd, by=c('Group.1','Group.2'))

colnames(data.sum)[1]<- 'treatment'
colnames(data.sum)[2]<- 'concentration'
colnames(data.sum)[3]<- 'absorbance'
colnames(data.sum)[4]<- 'sd'

charcoal=subset(data.sum, treatment=="charcoal")

############################################################################
############################################################################
## 4-parameter Log-logistic model - see calibration_curve_AC_treated.R script for details

m.nl <- drm(absorbance~concentration,
                  fct=LL.4(),
                  data=charcoal)

plot(m.nl, type = "average",log = "")

############################################################################
############################################################################
## Visual assessment of linear part of the calibration curve

m.0<-lm(absorbance~concentration,data=charcoal)
plot(m.nl, type = "average",log = "")
curve(predict(m.0, newdata = data.frame(concentration=x)), col= 'red',add=TRUE)
### visually a poor fit overall and compared with the 4-parameter Log-logistic model


charcoal2=charcoal[charcoal$concentration >= 2, ]
m.2<-lm(absorbance~concentration,data=charcoal2)
plot(m.nl, type = "average",log = "")
curve(predict(m.2, newdata = data.frame(concentration=x)), col= 'blue',add=TRUE)
abline(v=2,col='grey55')
### visually a poor fit for the the 2 microM concentration

charcoal4=charcoal2[charcoal2$concentration >= 4, ]
m.4<-lm(absorbance~concentration,data=charcoal4)
plot(m.nl, type = "average",log = "")
curve(predict(m.4, newdata = data.frame(concentration=x)), col= 'green',add=TRUE)
abline(v=4,col='grey55')
### visually OK, but at this concentration the Pi loss still represented the largest portion of the total [Pi] so needs to be further assessed.

charcoal6=charcoal4[charcoal4$concentration >= 6, ]
m.6<-lm(absorbance~concentration,data=charcoal6)
plot(m.nl, type = "average",log = "")
curve(predict(m.6, newdata = data.frame(concentration=x)), col= 'purple',add=TRUE)
abline(v=6,col='grey55')
### visually OK. No need to check higher concentrations. 

plot(absorbance ~ concentration, charcoal)
curve(predict(m.4, newdata = data.frame(concentration=x)), col= 'green',add=TRUE)
curve(predict(m.6, newdata = data.frame(concentration=x)), col= 'purple',add=TRUE)
abline(v=4,col='grey55')
### lines from m.4 and m.6 visually overlap. 

############################################################################
############################################################################
