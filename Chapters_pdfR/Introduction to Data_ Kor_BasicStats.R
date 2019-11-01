## **************************************** ##
##      R Script for Introduction to Data   ##
## **************************************** ##

## 작업 디렉토리 설정

getwd() #현재 R이 인식하고 있는 작업 디렉토리가 어딘지 알려준다.
setwd("C:/Users/phere") #원하는 장소로 디렉토리를 변경한다.

dir.create(path= "figures")
dir.create(path= "tables")
dir.create(path= "datasets")
dir.create(path= "references")
dir.create(path= "tex")

## 무작위 추출(Random Draws)

MP <- rbinom(50, 1, .7) #70%의 승진확률로 무작위로 추출한 50명의 남성
WP <- rbinom(50, 1, .7) #70%의 승진확률로 무작위로 추출한 50명의 여성
sum(MP) #총 승진한 남성의 수
sum(WP) #총 승진한 여성의 수
sum(MP)/50 #전체 남성 승진후보자에 대한 승진한 남성의 비율
sum(WP)/50 #전체 여성 승진후보자에 대한 승진한 여성의 비율
DP <- sum(MP) - sum(WP) #DP는 Difference in Promotion, 승진자 수의 차이입니다.


trial.size <- 10 #시뮬레이션을 시도할 횟수
Test10 <- rep(NA, trial.size) ##rep는 replicate, 즉 반복하라는 함수입니다.
##즉, 이 함수는 trial.size의 수만큼 NA를 반복해서
##Test10이라는 벡터에 담으라는 의미입니다.
Test10 #위의 함수를 통해 얻게 되는 Test10 벡터의 값은 아래와 같습니다.

## 루프-반복(Loops)

for (a in 1:trial.size) { # a라는 벡터에 1부터 trial.size까지의 수를 넣으라는 명령
  MP <- rbinom(50, 1, .7) # MP를 계산하라, 총 50명이 0.7의 확률로 1을 가질 것
  WP <- rbinom(50, 1, .7) # WP를 계산하라, 총 50명이 0.7의 확률로 1을 가질 것
  Test10[a] <- (sum(MP) - sum(WP))/50
}
hist(Test10)

trial.size <- 100
Test100 <- rep(NA, trial.size)
for (b in 1:trial.size) {
  MP <- rbinom(50, 1, .7)
  WP <- rbinom(50, 1, .7)
  Test100[b] <- (sum(MP) - sum(WP))/50
}
hist(Test100)

trial.size3 <- 1000000
Test1M <- rep(NA, trial.size3)
for (c in 1:trial.size3) {
  MP <- rbinom(50, 1, .7)
  WP <- rbinom(50, 1, .7)
  Test1M[c] <- (sum(MP) - sum(WP))/50
}
hist(Test1M)

length(Test1M[Test1M >= 0.3]) / length(Test1M)

## Rplots를 파일의 형태로 저장하기

pdf("figures/histogram.pdf", width=8, height=6) 
## 아까 만든 figures에 histogram.pdf라는 이름으로 저장하게 합니다.
hist(Test1M, xlab = "Margin", main = NULL)  #표 전체 제목은 NULL, 없습니다
##Test1M, 100만번 시뮬레이팅한 히스토그램 X축에는 Margin이라고 레이블을 달 것입니다.
dev.off()

png("figures/histogram.png", width=720, height=480)
hist(Test1M, xlab = "Margin", main = NULL)
dev.off()

## 데이터 다루기 기본

library(tidyverse) # 티블을 사용하기 위해서는 tidyverse 패키지를 불러와줘야 합니다.
data1 <- tibble(name = c("Jane", "John", "Jen", "James"),
                height = c(60, 70, 65, 68),
                eye.color = c("blue", "blue", "brown", "brown"),
                gender = c("female", "male", "female", "male"),
                highest.degree = c("college", 
                                   "high school", 
                                   "post graduate", 
                                   "college"))
glimpse(data1)

data1$female <- NA


data1$female <- ifelse(data1$gender == "female", 1, 0)
## 해석: data1에 female이라는 변수에 우측 함수에 따른 값을 배정하라.
##      ifelse(만약 ~ 면, A를, ~가 아니라면, B를) 배정하라.
## 따라서 위의 함수는 data1의 gender 변수가 "female"이라는 문자일 경우 새로운 female
## 변수에 1을, "female"이 아닌 경우에는 0을 주어라.
data1$female # 더미 변수의 이름을 지을 때에는 기준값(reference value)이 헷갈리지 않게
# 1의 값을 갖는 라벨(label)로 변수 이름을 짓는게 좋다 (TIP)
data1$gender <- NULL # 이제 사용하지 않을 gender 변수는 결측치로 변경.

data1$name <- as.character(data1$name) 
# name 변수의 자료들은 문자형(STATA에서 string이라고 하는)으로 이루어져 있는데, 
# 이를 요인형으로 바꾸는 것이다.
glimpse(data1)
knitr::kable(summary(data1))

GPA <- c("3.0", "4.0", "3.8", "2.2")
## 만약 "" 인용부호를 제외하고 벡터로 입력하면 GPA는 숫자형 자료가 될 것입니다.
## 기존의 data1 데이터프레임에다가 방금 만든 GPA를 새로운 열로 추가해보겠습니다.
data1 <- cbind(data1, GPA) #cbind는 열로 묶으라는 것입니다, 행으로 묶는 것은 rbind()
glimpse(data1)
knitr::kable(summary(data1))

data1$GPA <- as.numeric(as.character(data1$GPA))
names(data1)[6] <- "GPA.num" # 기존 요인형 GPA랑 새롭게 만든 숫자형 GPA 비교를 위해
# .num(numeric 약자)을 붙여 새로운 변수로 만듭니다.
table(data1$GPA.num)

data1 <- cbind(data1, GPA)
data1$GPA <- as.numeric(data1$GPA) # 이렇게 하면 문제가 생김
glimpse(data1$GPA)

### 다른 유형의 데이터 불러오기 (Loading data in different formats)

## STATA 파일을 불러오기 위해서는 "foreign" 패키지가 필요합니다.
## install.packages("foreign") # 저는 이미 설치가 되어 있습니다.
library(foreign) # 설치만 해서는 안되고 패키지를 불러와야 합니다.
## STATA 파일을 불러와 보겠습니다.
here::here() %>% setwd()
QOG <- read.dta(file = "example.data/qog_std_cs_jan19_ver13.dta", 
                convert.underscore = TRUE)
## foreign 함수로는 STATA 버전 13 이전의 자료만 불러들일 수 있습니다. 즉, 아래 코드는 불가.
QOG <- read.dta(file = "example.data/qog_std_cs_jan19_ver15.dta", 
                convert.underscore = TRUE)
## 그렇다면 버전 13 이후는 무슨 패키지를 사용해야 할까요?
## 버전 13 이후는 "readstata13" 로 불러올 수 있습니다.
# install.packages("readstata13")
library(readstata13)
QOG.v2 <- read.dta13(file = "example.data/qog_std_cs_jan19_ver15.dta", 
                     convert.underscore = TRUE)

## 또 다른 방법이 있다. 바로 "haven" 패키지를 이용하는 것입니다.
## install.packages("haven")
library(haven)
QOG.v3 <- read_stata("example.data/qog_std_cs_jan19_ver15.dta")

## 근데 저는 foreign 이나 haven 패키지 모두 안 씁니다.
## 더 효율적인 패키지를 찾았거든요. 바로 ezpickr 입니다.
## install.packages("ezpickr")
library(ezpickr)
QOG.v4 <- pick("example.data/qog_std_cs_jan19_ver15.dta")

### 자료 머징하기 (Using Data Merging)

## names(QOG) # QOG 데이터프레임의 변수명을 나열하라는 함수입니다.
## 변수가 엄청 많습니다.
length(names(QOG)) # 1983개의 변수
QOG.tomerge <- subset(QOG, select = c(ccodecow, wdi.pop))
## QOG.tomerge라는 하위 셋을 만들라는 명령입니다.
## QOG라는 자료에서 ccodecow와 wdi.pop라는 두 변수만을 선택(select)하여 만듭니다.
QOG.tomerge <- subset(QOG, ccodecow > 100, select = c(ccodecow, wdi.pop))
## QOG.tomerge라는 하위 셋을 만들어라. 이 경우에는 앞의 QOG.tomerge를 대체(replace)합니다.
## QOG라는 자료에서 ccodecode가 100보다 큰 경우에 한하여(조건)
## ccodecow와 wdi.pop라는 변수를 선택하여 하위 셋을 만듭니다.
## 결과적으로 QOG.tomerge는 cowcode가 100보다 큰 국가들의 세계발전지표 상의 인구
## 지표들을 가지게 됩니다.

QOG.dyad <- merge(x = QOG.tomerge, y = QOG.tomerge, by = NULL)

names(QOG.dyad)
QOG.dyad <- subset(QOG.dyad, ccodecow.x != ccodecow.y)
## QOG.dyad 자료에서 ccodecow.x가 ccodecow.y와 다른 경우만 다시 저장합니다.
QOG.dyad$pop.dif <- QOG.dyad$wdi.pop.x - QOG.dyad$wdi.pop.y
## QOG.dyad 데이터프레임에 pop.dif, 인구차이라는 변수를 새로 만듭니다.
## 인구 차이는 x국가의 wdi.pop.x에서 wdi.pop.y 를 감한 값입니다.
## 이 변수는 x국가와 y국가 간 인구 차이, 즉 두 국가 간의 역동적 관계를 보여줍니다.

QOG.nddyad <- subset(QOG.dyad, ccodecow.x < ccodecow.y)
QOG.nddyad$pop.dif.nd <- abs(QOG.nddyad$wdi.pop.x - QOG.nddyad$wdi.pop.y)
## abs는 absolute value, 부호를 고려하지 않기 위해서 절대값으로 만들라는 것입니다.

## COW 국가-연도 목록을 불러왔습니다.
stateyear <-
  read.csv("http://correlatesofwar.org/data-sets/state-system-membership/system2016",
           head = TRUE, sep = ",")# Look at country codes
unique(stateyear$ccode) # 중복되지 않는 국가코드만 보이게 했습니다.
table(stateyear$ccode)  # 각 국가코드가 몇 개의 관측치를 가지는지를 보여줍니다.

stateyear <- subset(stateyear, select = c(year, ccode))

## install.packages("countrycode")
library(countrycode)
stateyear$countryname <- countrycode(stateyear$ccode, "cown", "country.name")
## stateyear 자료에서 ccode 변수를 숫자형("cown")에서 문자형("country.name")으로 변경합니다.
## stateyear 자료에 countryname 이라는 새로운 이름 변수를 만들어 바꾼 자료를 배정합니다.

## 이 자료에서 결측치(missing values)를 제거해보겠습니다. 
## complete.cases는 결측치를 제외한 변수들 짝이 완전하게 맞는 사례들만을 선별하라는 옵션입니다.
stateyear <- subset(stateyear, complete.cases(stateyear))
head(stateyear)

dyadyear <- merge(x=stateyear, y=stateyear, by.x=c("year"), by.y=c("year"))
head(dyadyear)

dyadyear <- dyadyear[dyadyear$countryname.x != dyadyear$countryname.y, ]
head(dyadyear)

table(dyadyear$countryname.x == dyadyear$countryname.y)

## 대괄호 [] 를 이용한 방법
dyadyearp80 <- dyadyear[dyadyear$year >= 1980,] 
glimpse(dyadyearp80)
## subset 함수를 이용한 방법
dyadyearp80 <- subset(dyadyear, dyadyear$year >= 1980)
glimpse(dyadyearp80)

## 기본 함수를 이용해 변수명 바꾸기
## names(dyadyearp80)[변수 순서] <- "바꿀 변수 이름"
## 2번째 변수부터 5번째 변수들의 이름을 바꿔보자.
names(dyadyearp80)[2:5] <- c("ccode1", "countryname1", 
                             "ccode2", "countryname2")
head(dyadyearp80)
## install.packages("plyr")
library(plyr)
dyadyearp80 <- rename(dyadyearp80, c("ccode.x" = "ccode1", 
                                     "ccode.y" = "ccode2",
                                     "countryname.x" = "countryname1",
                                     "countryname.y" = "countryname2"))

### 예제: correlatesofwar.org에서 capabilities 데이터셋을 불러오기[^3-3]

cinc.link <- 
  "http://correlatesofwar.org/data-sets/national-material-capabilities/nmc-v4-data"
cinc <-
  read.csv(file = cinc.link,
           head = TRUE,
           sep = ",",
           na = c(-9))

cinc.cut <- cinc[c("ccode", "year", "cinc")]
dyadcap <- merge(x = dyadyearp80,
                 y = cinc.cut,
                 by.x = c("ccode1", "year"),
                 by.y = c("ccode", "year"))
dyadcap <- rename(dyadcap, c("cinc" = "cinc1"))
head(dyadcap)
dyadcap <- merge(x = dyadcap, 
                 y = cinc.cut, 
                 by.x = c("ccode2", "year"), 
                 by.y = c("ccode", "year"))
head(dyadcap)
dyadcap <- rename(dyadcap, c("cinc" = "cinc2"))
dyadcap$caprat <- dyadcap$cinc1/dyadcap$cinc2

dyadcap %>% 
  dplyr::filter(ccode1 %in% c(2, 731), 
                ccode2 %in% c(2, 731), 
                year == 2000) %>% knitr::kable()
dyadcap %>% 
  dplyr::filter(ccode1 %in% 2, 
                ccode2 %in% 731, 
                year == 2000) %>% knitr::kable()
dyadcapnd <- dyadcap[dyadcap$ccode1 < dyadcap$ccode2,]
dyadcapnd$caprat <- pmax(dyadcapnd$cinc1, dyadcapnd$cinc2)/
  pmin(dyadcapnd$cinc1, dyadcapnd$cinc2)
summary(dyadcapnd$caprat)
library(stargazer)
dyadcapnd <- subset(dyadcapnd, 
                    select = -c(countryname1, countryname2))
glimpse(dyadcapnd)

stat.table <- stargazer(dyadcapnd, 
                        covariate.labels = 
                          c("Country code 1", 
                            "Country code 2", "Year", 
                            "CINC 1", "CINC 2", 
                            "Capability ratio"), 
                        title = "Summary Statistics",
                        label = "stat.table")
write(x = stat.table, file = "tables/table1.tex")

## 서로 다른 분석수준(lower & higher) 통합하기

download.file(url = 
                "http://correlatesofwar.org/data-sets/bilateral-trade/cow_trade_3.0",
              destfile = "COWTrade3.0.zip", mode="wb")
unzip("COWTrade3.0.zip", exdir = getwd())

btrade <- read.csv(file = "COW_Trade_3.0/dyadic_trade_3.0.csv")
names(btrade)
glimpse(btrade)

btrade$source2 <- 
  btrade$bel_lux_alt_flow1 <- 
  btrade$bel_lux_alt_flow2 <- 
  btrade$china_alt_flow1 <- 
  btrade$china_alt_flow2 <- 
  btrade$version <- NULL
glimpse(btrade)

btrade$flow1[btrade$flow1 == -9] <- NA # 대괄호는 조건(condition)을 의미합니다.
btrade$flow2[btrade$flow2 == -9] <- NA
summary(btrade$flow1) # -9이 사라졌습니다.

btrade$tottrade1 <- btrade$flow1 + btrade$flow2 # 잘못된 결과를 얻게 됩니다.
summary(btrade$tottrade1)

btrade$tottrade2 <- ifelse(is.na(btrade$flow1), btrade$flow2,
                           ifelse(is.na(btrade$flow2), btrade$flow1,
                                  btrade$flow1 + btrade$flow2))
summary(btrade$tottrade2)

library(plyr)
btrade <- ddply(btrade, .(ccode1, year), 
                transform, styrtrade1 = sum(tottrade2, na.rm = TRUE))
btrade$share1 <- btrade$tottrade2 / btrade$styrtrade1 