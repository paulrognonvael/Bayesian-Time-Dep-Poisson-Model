#Skin cancer example

#The example was taken from Applied Regression Analysis and Multivariable Methods, 4th Edition.
#Skin cancer dataset from “Non-melanoma skin cancer among Caucasians in four areas of the United States.” (www.pubmed.org/4422100).
#Dataset is availabe at http://academic.cengage.com/resource_uploads/downloads/0495384968_88614.zip (melanom.txt). As it is small it was included below.
#It describes the number of cases of non-melanoma skin cancer among residents of two cities in each age category.
## Create a dataset manually
nonmel <- read.table(header = TRUE,
                     text = "
                     cases city u1 u2 u3 u4 u5 u6 u7      n
                     1      1    0  1  0  0  0  0  0  0 172675
                     2     16    0  0  1  0  0  0  0  0 123065
                     3     30    0  0  0  1  0  0  0  0  96216
                     4     71    0  0  0  0  1  0  0  0  92051
                     5    102    0  0  0  0  0  1  0  0  72159
                     6    130    0  0  0  0  0  0  1  0  54722
                     7    133    0  0  0  0  0  0  0  1  32185
                     8     40    0  0  0  0  0  0  0  0   8328
                     9      4    1  1  0  0  0  0  0  0 181343
                     10    38    1  0  1  0  0  0  0  0 146207
                     11   119    1  0  0  1  0  0  0  0 121374
                     12   221    1  0  0  0  1  0  0  0 111353
                     13   259    1  0  0  0  0  1  0  0  83004
                     14   310    1  0  0  0  0  0  1  0  55932
                     15   226    1  0  0  0  0  0  0  1  29007
                     16    65    1  0  0  0  0  0  0  0   7583
                     ")

## Create age.range variable and city variable
nonmel <- within(nonmel, {
  age.range <- rep(c("15_24","25_34","35_44","45_54","55_64","65_74","75_84","85+"), 2)
  age.range <- factor(age.range)
  age.range <- relevel(age.range, ref = "85+")
  
  city <- factor(city, 0:1, c("Minneapolis", "Dallas"))
})

## drop unnecessary columns
nonmel <- nonmel[c("cases","n","city","age.range")]

## Check data
nonmel

#Poisson regression modeling

## Including offset(log(n)) in the right hand side
model.1 <- glm(cases ~ city + age.range + offset(log(n)), family = poisson(link = "log"), data = nonmel)
## Using the offset option
model.1 <- glm(cases ~ city + age.range, offset = log(n), family = poisson(link = "log"), data = nonmel)

## Results from regular Poisson
summary(model.1)
#Confidence interval for coefficients
confint(model.1)
#Confidence interval for RR
data.frame(RR=exp(model.1$coefficients),Lower=exp(confint(model.1))[,1],Upper=exp(confint(model.1))[,2])


## quasi-Poisson to allow the scale parameter to change from 1. Show the dispersion parameter.
model.1q <- glm(cases ~ city + age.range, offset = log(n), family = quasipoisson(link = "log"), data = nonmel)
summary(model.1q)


#Negative binomial regression

## Load MASS
library(MASS)
## Negative binomial regression
model.1nb <- glm.nb(cases ~ city + age.range + offset(log(n)), data = nonmel)
summary(model.1nb)



#Lung cancer example from Introductory Statistics with R (ISwR) Book
#E.B. Andersen (1977), Multiplicative Poisson models with unequal cell rates, Scandinavian Journal of Statistics, 4:153-158.
#Data preparation
## Load ISwR package
install.packages("ISwR")
library(ISwR)
## Load data
data(eba1977)
eba1977

# Lung cancer incidence in four Danish cities 1968-1971
# 
# Description:
#   This data set contains counts of incident lung cancer cases and
# population size in four neighbouring Danish cities by age group.
# 
# Format:
#   A data frame with 24 observations on the following 4 variables:
#   city a factor with levels Fredericia, Horsens, Kolding, and Vejle.
#   age a factor with levels 40-54, 55-59, 60-64, 65-69, 70-74, and 75+.
#   pop a numeric vector, number of inhabitants.
#   cases a numeric vector, number of lung cancer cases.

## Fit Poisson model
model.2 <- glm(cases ~ city + age, offset = log(pop), family = poisson(link = "log"), data = eba1977)
summary(model.2)

## Check dispersion parameter with quasi-Poisson regression
model.2q <- glm(cases ~ city + age, offset = log(pop), family = quasipoisson(link = "log"), data = eba1977)
summary(model.2q)

#Goodness of fit test If the residual deviance is close enough to the residual degrees of freedom, it is a good fit. 
#It can be tested by Chi-squared test.

list(residual.deviance           = deviance(model.2),
     residual.degrees.of.freedom = df.residual(model.2),
     chisq.p.value               = pchisq(deviance(model.2), df.residual(model.2), lower = F)
)

#Negative binomial regression

## Negative binomial regression
model.2nb <- glm.nb(cases ~ city + age + offset(log(pop)), data = eba1977)
summary(model.2nb)



