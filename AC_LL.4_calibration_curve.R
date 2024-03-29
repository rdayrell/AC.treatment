# Dayrell RLC, Cawthray GR, Lambers H, Ranathunge K. (2022) Using activated charcoal to remove substances interfering with the colorimetric assay of inorganic phosphate in plant extracts. Plant and Soil, 476:755–764.


### Modelling of light absorbance as a function of P concentration after activated charcoal treatment

require(lawstat)
require(drc)

data=read.csv("raw.curve.csv")

charcoal=subset(data, treatment=="charcoal")

m.charcoal <- drm(absorbance~concentration,
                  fct=LL.4(),
                  data=charcoal)

# For details see https://doi.org/10.1371/journal.pone.0146021

summary(m.charcoal)

plot(m.charcoal, type = "average",log = "")

##############################################################################
## checking assumptions of nonlinear model according to 
## Ritz C & Streibig JC (2009) Nonlinear Regression with R. Springer New York

### Residual plot
plot(fitted(m.charcoal), residuals(m.charcoal),
     xlab = "Fitted Values", ylab = "Residuals")
abline(a = 0, b = 0)

### F test
modelFit(m.charcoal)
# If the test is non-significant, then there is no evidence against the nonlinear regression model. 

### Normal distribution of standardised residuals
standardRes <- residuals(m.charcoal, typeRes = 'standardised')
qqnorm(standardRes, main = "")
abline(a = 0, b = 1)

### Shapiro-Wilk test
shapiro.test(standardRes)

### Variance homogeneity
with(charcoal, levene.test(absorbance,as.factor(concentration)))

### Independence
plot(residuals(m.charcoal), c(residuals(m.charcoal)[-1], NA), xlab = "Residuals", ylab = "Lagged residuals")

