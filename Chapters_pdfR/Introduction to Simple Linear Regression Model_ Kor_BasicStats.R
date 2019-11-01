#Intoduction to Simple Linear Regression Model

library(here)
library(ggplot2)
library(kableExtra)
library(ezpickr)
library(tidyverse)
rm(list = ls())
here::here() %>% setwd()
GSScut <- pick("example.data/GSScut.dta")
glimpse(GSScut)

GSScut$female <- ifelse(GSScut$sex == 1, 1, 0)
table(GSScut$female)
GSScut$white <- ifelse(GSScut$race == 1, 1, 0)
table(GSScut$white)


cor(x = GSScut$hrs1, y = GSScut$educ, use = "pairwise.complete.obs")

ggplot(GSScut, aes(x = educ, y = hrs1)) + geom_point()

ggplot(data = GSScut) + 
  geom_point(aes(x = educ, y = hrs1)) +
  geom_smooth(aes(x=educ, y=hrs1), method='lm')

model1 <- lm(hrs1 ~ educ, data = GSScut)
model1 %>% summary()

plot(x = GSScut$educ, y = GSScut$hrs1)
abline(lm(model1))

plot(x = model1$fitted.values, y = model1$residuals) 
abline(0,0)

stpred1 <- (model1$fitted.values - mean(model1$fitted.values)) / 
  sd(model1$fitted.values)
stres1 <- (model1$residuals - mean(model1$residuals)) / 
  sd(model1$residuals)
plot(x = stpred1, y = stres1)
abline(0,0)

hist(stres1, freq = FALSE) 
curve(dnorm, add = TRUE)  

qqnorm(stres1)
abline(0,1)

library(lmtest)
bptest(model1)

# install.packages("sandwich")
library(sandwich)
coeftest(model1, vcov = vcovHC(model1, type = "HC1"))

cooksd <- cooks.distance(model1)
plot(cooksd, main="Influential Obs by Cooks distance")
abline(h = 1, col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>1,names(cooksd),""),
     col="red")

plot(cooksd, main="Influential Obs by Cooks distance")  
abline(h = 4/length(cooksd), col="red")
text(x=1:length(cooksd)+1, y=cooksd, 
     labels=ifelse(cooksd>4/length(cooksd), 
                   names(cooksd),""), col="red")

model1 <- lm(hrs1 ~ educ, data = GSScut[-(4030),])


tidy <- model1 %>% broom::tidy()
tidy %>% knitr::kable()

model2 <- lm(hrs1 ~ educ + female, data = GSScut)
summary(model2)

model2$coefficients
plot(x = GSScut$educ, y = GSScut$hrs1)
abline(model2$coefficients[1], model2$coefficients[2] )
abline((model2$coefficients[1] + 
          model2$coefficients[3]), model2$coefficients[2] )


ggplot(data = GSScut) + 
  geom_point(aes(x = educ, y = hrs1, color = as.factor(female))) + 
  geom_smooth(aes(x = educ, y = hrs1, color = as.factor(female)), method = "lm")

model3 <- lm(hrs1 ~ educ * female, data = GSScut) 
summary(model3)


model3$coefficients

plot(x = GSScut$educ, y = GSScut$hrs1)

abline(model3$coefficients[1], model3$coefficients[2])
abline((model3$coefficients[1] + model3$coefficients[3]), 
       (model3$coefficients[2] + model3$coefficients[4]))


QOG <- pick(file = "http://www.qogdata.pol.gu.se/data/qog_std_cs_jan19.dta")

QOGcut <- QOG %>% select(ccodecow, cname, ti_cpi, dr_ig)

QOGcut %>% select(ti_cpi, dr_ig) %>% summary() %>% knitr::kable()

QOGcut <- QOGcut %>% mutate(
  corrupt = 100 - ti_cpi
)

QOGcut %>% select(corrupt, dr_ig) %>% summary() %>% knitr::kable()

model0 <- lm(dr_ig ~ corrupt, data = QOGcut) 
summary(model0)


temp.data <- QOGcut %>% drop_na() 
y <- temp.data$dr_ig   
x <- temp.data$corrupt 

b1 <- (sd(y) / sd(x)) * cor(y, x)
b0 <- mean(y) - b1 * mean(x)

x.matrix <- as.matrix(cbind(rep(1, nrow(temp.data)), x))

b.vector <- solve(t(x.matrix) %*% x.matrix) %*% (t(x.matrix) %*% y)

y.hat <- b0 + b1 * x

plot(y.hat, y)
abline(0, 1)

resid <- y - y.hat

hist(resid)
mean(resid)

cor(cbind(resid, x))
plot(x, resid)
plot(y.hat, resid)

cor(y, x)^2

SSE <- sum((y.hat - mean(y))^2)
SSR <- sum((y - y.hat)^2)
R2 <- SSE / (SSE + SSR)
R2

n <- length(y)
k <- 1
adj.R2 <- 1 - (((1 - R2) * (n - 1)) / (n - k - 1))

dfm <- k
dfr <- n - k - 1

MSM <- SSE/dfm
MSR <- SSR/dfr 
F <- MSM / MSR

1 - pf(F, dfm, dfr) 
sqrt(MSR) 
sd(y)


vcov <- 1 / (n - k - 1) * as.numeric(t(resid) %*% resid) * 
  solve(t(x.matrix) %*% x.matrix)
vcov

SE.b0 <- sqrt(vcov[1,1])
SE.b1 <- sqrt(vcov[2,2])
t.b0 <- (b0 - 0) / SE.b0
1 - pt(t.b0, df = n - k - 1)
t.b1 <- (b1 - 0) / SE.b1
pt(t.b1, df = n - k - 1)


summary(model0)
manual <- as_tibble(bind_cols(
  Term = c("(Intercept)", "Corrupt"),
  Estimate = c(b0, b1),
  Std.Error = c(SE.b0, SE.b1),
  R.sq = c(R2, NA),
  F = c(F, NA)))
manual %>% knitr::kable()

