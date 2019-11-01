## **************************************** ##
##      R Script for Probability into R     ##
## **************************************** ##
# Probability into R

rm(list=ls()) # 현재 콘솔 창에 저장되어 있는 모든 값과 모델 등을 삭제 


library(here)
library(knitr)
library(dplyr)
library(tidyverse)
library(kableExtra)
here::here() %>% setwd()

## 주사위굴리기 게임!

die <- as.integer(runif(1, min=1, max=7))
die

dice <- (as.integer(runif(1, min=1, max=7))) +
  (as.integer(runif(1, min=1, max=7)))
dice

# 주사위 두 개를 100번 던져보기
dice100 <-  (as.integer(runif(100, min=1, max=7))) +
  (as.integer(runif(100, min=1, max=7)))

# 주사위 두 개를 1,000번 던져보기
dice1000 <-  (as.integer(runif(1000, min=1, max=7))) +
  (as.integer(runif(1000, min=1, max=7)))

# 주사위 두 개를 100만 번 던져보기
dice1M <-  (as.integer(runif(1000000, min=1, max=7))) + 
  (as.integer(runif(1000000, min=1, max=7)))

par(mfrow = c(1, 3))
hist(dice1000)
hist(dice100)
hist(dice1M)

## 동전 던지기

coin <- rbinom(1, 1, .5)
coin


# 동전 100개 던지기
coin100 <- rbinom(100, 1, .5)
coin100 %>% table() %>% kable()

#동전 1000개 던지기
coin1000 <- rbinom(1000, 1, .5)
coin1000 %>% table() %>% kable()

par(mfrow = c(1, 2))
hist(coin100)
hist(coin1000)

coin1Mx <- rep(NA, 1000000)
for(i in 1:1000000){
  coin1Mx[i] <- sum(rbinom(100, 1, .5))
}
hist(coin1Mx, 
     freq = FALSE, 
     main = "Distribution of heads\n in 100 coin tosses", 
     xlab = "Number of heads")

table(coin1Mx[coin1Mx > 60])
coin1Mx[coin1Mx > 60] %>% table() %>% t() %>% kable()

length(coin1Mx[coin1Mx > 60])/length(coin1Mx)
sum(table(coin1Mx[coin1Mx > 60]))/1000000

## 독립사건 시뮬레이션

rand1 <- runif(100, min = 0, max = 10)
summary(rand1)
rand2 <- rnorm(100, mean = 0, sd = 2)
summary(rand2)
plot(rand1, rand2)

## 종속사건 시뮬레이션

rand3 <- 4 + 0.75 * rand1 + rnorm(100, mean = 0, sd = 2)

plot(rand1, rand3)

rand4 <- 4 + 0.75 * rand1 + rnorm(100, mean = 0, sd = 5)
plot(rand1, rand4)
cor(rand1, rand4)

## 분포(Distribution)

### 정규분포 (The normal distribution)

pnorm(70, mean = 50, sd = 10, lower.tail = TRUE)
pnorm(70, mean = 50, sd = 10, lower.tail = FALSE)
1 - pnorm(70, mean = 50, sd = 10, lower.tail = TRUE)

## 첫 번째 방법
pnorm(70, mean = 50, sd = 10, lower.tail = FALSE) + 
  pnorm(30, mean = 50, sd = 10, lower.tail = TRUE)

## 두 번째 방법
2 * pnorm(70, mean = 50, sd = 10, lower.tail = FALSE)

qnorm(0.9772499, mean = 50, sd = 10, lower.tail = TRUE)

dnorm(70, mean = 50, sd = 10)

x <- rnorm(70, mean = 50, sd = 10)
x

normal5 <- rnorm(n = 10000, mean = 5, sd = 3)
normal50 <- rnorm(n = 10000, mean = 50, sd = 10)
normal20 <- rnorm(n = 10000, mean = 20, sd = 1)
norm <- bind_rows(tibble(x = normal5, Mean = 5, SD = 3),
                  tibble(x = normal50, Mean = 50, SD = 10),
                  tibble(x = normal20, Mean = 20, SD = 1))
norm$Mean <- as.factor(norm$Mean)

ggplot(norm, aes(x = x)) +
  geom_density(aes(fill = as.factor(Mean)), adjust = 4, alpha = 1/2) +
  guides(color=guide_legend(title = "Mean, SD")) +
  guides(fill=guide_legend(title = "Mean, SD")) +
  scale_color_discrete(labels = c("5, 3", "20, 1", "50, 10")) +
  scale_fill_discrete(labels = c("5, 3", "20, 1", "50, 10")) +
  ggtitle("Probability Density Function\nNormal Distribution")


### 이항분포 (Binomial distribution)

pbinom(27, size=100, prob=0.25, lower.tail = TRUE)
qbinom(0.7223805, size = 100, prob = 0.25, lower.tail = TRUE)
dbinom(27, size=100, prob=0.25)
choose(100, 27)*.25^27*(1-.25)^(100-27)

rbinom(27, size=100, prob=0.25)
sd(rbinom(27, size=100, prob=0.25))

binom10 <- rbinom(n = 10000, p = .5, size = 10)
binom50 <- rbinom(n = 10000, p = .5, size = 50)
binom100 <- rbinom(n = 10000, p = .5, size = 100)

binom <- bind_rows(tibble(k = binom10, Size = 10), 
                   tibble(k = binom50, Size = 50), 
                   tibble(k = binom100, Size = 100))
binom$Size <- as.factor(binom$Size)

ggplot(binom, aes(x = k)) +
  geom_density(aes(group = Size, color = Size, fill = Size), 
               adjust = 4, alpha = 1/2) +
  ggtitle("Probability Mass Function\nBinomial Distribution")


### 포와송 분포 (Poison distribution)

pois1 <- rpois(n = 10000, lambda = 1)
pois2 <- rpois(n = 10000, lambda = 2)
pois5 <- rpois(n = 10000, lambda = 5)
pois20 <- rpois(n = 10000, lambda = 20)
pois <- tibble(Lambda.1 = pois1, 
               Lamnda.2 = pois2,
               Lambda.5 = pois5,
               Lambda.20 = pois20)


library(reshape2)
library(stringr)
pois <- melt(data = pois, 
             variable.name = "Lambda", 
             value.name = "x")

pois$Lambda <- 
  as.factor(as.numeric(str_extract(string = 
                                     pois$Lambda, 
                                   pattern = "\\d+")))

ggplot(pois, aes(x = x)) +
  geom_density(aes(group = Lambda, 
                   color = Lambda, 
                   fill = Lambda), 
               adjust = 4, alpha = 1/2) +
  ggtitle("Probability Mass Function\nPoisson Distribution")


### 음이항 분포 (Negative Binomial Distribution)

nbinom1 <- rnbinom(n = 10000, p = .3, size = 1)
nbinom5 <- rnbinom(n = 10000, p = .3, size = 5)
nbinom10 <- rnbinom(n = 10000, p = .3, size = 10)
nbinom <- bind_rows(tibble(x = nbinom1, Size = 1), 
                    tibble(x = nbinom5, Size = 5), 
                    tibble(x = nbinom10, Size = 10))
nbinom$Size <- as.factor(nbinom$Size)

ggplot(nbinom, aes(x = x)) +
  geom_density(aes(group = Size, color = Size, fill = Size), 
               adjust = 4, alpha = 1/2) +
  ggtitle("Probability Mass Function\nNegative Binomial Distribution")


### F 분포 (F Distribution)

fa <- rf(n = 10000, df1 = 1, df2 = 50)
fb <- rf(n = 10000, df1 = 5, df2 = 100)
fc <- rf(n = 10000, df1 = 50, df2 = 50)
fd <- rf(n = 10000, df1 = 50, df2 = 500)
f <- bind_rows(tibble(x = fa, DF1 = 5, DF2 = 5), 
               tibble(x = fb, DF1 = 5, DF2 = 10), 
               tibble(x = fc, DF1 = 10, DF2 = 5),
               tibble(x = fd, DF1 = 10, DF2 = 10))

f <- subset(f, x <= 6)
f$DF <- f$DF1 * 100 + f$DF2
f$DF <- as.factor(f$DF)

ggplot(f, aes(x = x)) +
  geom_density(aes(color = DF, fill = DF), adjust = 4, alpha = 1/2) +
  guides(color=guide_legend(title = "DF1, DF2")) +
  guides(fill=guide_legend(title = "DF1, DF2")) +
  scale_color_discrete(labels = c("1, 50", "5, 500", "50, 50", "50, 500")) +
  scale_fill_discrete(labels = c("1, 50", "5, 500", "50, 50", "50, 500")) +
  ggtitle("Probability Density Function\nF Distribution")

