## **************************************** ##
##      R Script for Introduction to R      ##
## **************************************** ##
## 1+1을 계산해봅시다.
1 + 1  # 답은 2

## 제곱해보기: a^b라고 할 때, a를 b만큼 곱해주는 것
3^2 #9
2^3 #8
## 나머지 구하기 : a%%b라고 할 때, a를 b로 나누고 몫이 아닌 나머지를 보여줍니다.
27 %% 7 #6
## 마지막으로 연산자를 가지고 계산할 때, 그리고 연산자 뿐 아니라 코딩 전체에 있어서
## 순서를 잘 고려하여 코딩해야 합니다. 아래 두 계산은 완전히 결과가 다릅니다.
3 + 4 / 7
(3 + 4) / 7

## **R**의 기본적인 자료 유형

## 사과라는 변수에다가 값을 집어넣어 보겠습니다
apples <- 4
## 사과에 담긴 값을 출력합니다.
apples
## 오렌지라는 변수에다가 값을 집어넣어 보겠습니다.
oranges <- 6 
## 오렌지에는 6이 들어가 있고 사과에는 4가 들어가 있습니다. 두 개를 더해 보겠습니다.
apples + oranges
## 오렌지 객체에 문자열 자료를 다시 저장해봅니다.
oranges <- "six" 
## 오렌지와 사과를 더해 보겠습니다.
apples + oranges # 에러메세지를 확인할 수 있습니다.

### 논리형 연산자 (Logical operators)
## a와 v에 값이 저장되어 있지않으면 논리형 연산자는 작동하지 않습니다.

a < b   # a가 b보다 작다는 것을 보여줍니다.
a <= b  # a가 b보다 작거나 같다는 것을 보여줍니다.
a > b   # a가 b보다 크다는 것을 보여줍니다.
a >= b  # a가 b보다 크거나 같다는 것을 보여줍니다.
a == b  # a와 b가 같다/동일하다는 것을 보여줍니다.
!a      # a가 아니라는 의미입니다.

## 1이 2보다 작을까?
1 < 2 # TRUE, 사실이라는 결과를 얻을 것입니다.
## 1 더하기 1이 3일까?
1 + 1 == 3 # FALSE, 거짓이라는 결과를 얻을 것입니다.

## 벡터 (Vectors)

## 숫자형 자료들이 담긴 벡터를 만들어 보겠습니다.
num_vec <- c(1, 2, 3)
## 여러 자료 유형을 이용하여 벡터를 만들어 보겠습니다.
mix_vec <- c(1, "Hi", TRUE)
class(mix_vec)  # Character라는 답을 얻게 됩니다.
typeof(mix_vec)
c(1, 2, 3) + c(4, 5, 6) 
c(1 + 4, 2 + 5, 3 + 6)
c(1, 2, 3) * c(4, 5, 6)     # 4, 10, 18의 결과값을 얻게 될 것입니다
c(1, 2) + c(4, 5, 6, 7, 8)  # 5, 7, 7, 9, 9의 결과를 얻게 되고, 두 벡터의 길이가
# 다르다는 경고 메시지를 보게 될 것입니다.
c(1, 2) * c(4, 5, 6, 7, 8)  # 4, 10, 6, 14, 8

num_vec <- c(11, 21, 63, 44, 95, 86)
num_vec[3]        # 63이라는 값, 벡터의 세 번째 값을 얻게 됩니다.
num_vec[c(1,4)]   # c(1, 4)는 첫 번째와 네 번째의 값을 산출하라는 뜻으로 11, 44라는
# 결과를 얻게될 것입니다.

## 매트릭스 (Matrices)

matrix(1:12, byrow=TRUE, nrow=3)
c1 <- 1:3  # 1, 2, 3
c2 <- 4:6  # 4, 5, 6
c3 <- 7:9  # 7, 8, 9
cbind(c1,c2,c3)
rbind(c1,c2,c3)
matrix <- matrix(1:12, byrow=TRUE, nrow=3) # matrix라는 객체에 결과를 저장합니다.
matrix[1, 2]
matrix[1:2, 2:3]  # 더 작은 형태의 매트릭스로 추출되는 것을 확인할 수 있습니다.
11 + matrix #아까 저장해 둔 matrix 객체에 11을 더하면 모든 요소에 11이 더해집니다.

## 데이터프레임 (Data frame)

# R에 내장되어 있는 데이터 프레임을 불러들여 보겠습니다
mtcars

## 티블 (Tibbles)

## install.packages("tidyverse") # 저는 이미 설치되어 있는 상태라 코멘트 처리합니다.
library(tidyverse)
mtcars <- as_tibble(mtcars)
mtcars

## 패키지 (Packages)

## R 패키지 설치 예시
## foreign 함수는 version 12 이하의 STATA 파일(.dta)을 로딩할 수 있게 도와줍니다.
## install.packages("foreign")
## plyr 함수는 좀 더 복잡하고 고급스러운 자료 조작(manipulation)을 가능하게 합니다.
## install.packages("plyr")
## ggplot2 함수는 함수 가시화(visualization)를 돕습니다.
## install.packages("ggplot2")

# 설치한 패키지들을 사용하기 위하여 라이브러리(libraries)를 로드한다.
library(foreign)
library(plyr)
library(ggplot2)

## install.packages("session")
library(session)
save.session(file="test.Rda") # 현재까지 불러온 패키지와 객체들이 R 스크립트가 저장된
# 디렉토리에 test.Rda라는 이름으로 저장됩니다.
## 나중에 Rstudio 종료 후 다시 켰을 때,
restore.session(file="test.Rda") # 기존에 저장되었던 test.Rda를 불러옵니다.
## 이때, 주의해야할 점은 R 스크립트가 저장된 디렉토리가 세션 정보를 담은 Rda가 저장된
## 디렉토리와 같아야 한다는 점입니다. 만약 다르다면 file="다른 디렉토리/file.Rda"로 
## 별도로 지정해주어야 합니다.

## 디렉토리 생성 코드

## 현재 R 콘솔에 저장된 모든 값, 모델 등을 제거하는 코드
rm(list=ls())

## 현재 작업중인 디렉토리가 어딘지 확인하는 코드
getwd()

## 새롭게 작업 디렉토리를 설정하는 코드
## 작업하고자 하는 폴더 우클릭 후 경로보기 하면 나옴
setwd("/Users/Documents")

## 표와 그래프를 위한 폴더를 만들기
dir.create("./tables")
dir.create("./figures")

## 용례 (A working example)

## 먼저 깔끔하게 R-콘솔 창을 정리합니다.
rm(list = ls())

## diamonds라는 데이터셋을 로드합니다. 이 데이터셋을 불러오려면 먼저 ggplot2를 설치하고
## 로드해야 합니다. ggplot2라는 패키지에 포함된 예제 데이터셋이기 때문입니다.
## install.packages("ggplot2") # 저는 이미 설치되어 있습니다.
library(ggplot2)
data(diamonds)
names(diamonds) # 데이터셋에 포함된 변수들의 이름을 확인할 수 있습니다.
head(diamonds) # 맨 위 몇 개 행의 특성을 간략하게 보여줍니다.
str(diamonds) # 데이터셋의 구조(관측치의 수, 변수의 수, 자료유형 등)를 보여줍니다.
summary(diamonds) # 데이터셋의 요약통계치(평균, 중간값, 분위수 등)를 보여줍니다.

## 라벨을 포함한 R 히스토그램
hist(diamonds$carat, main = "Carat Histogram", xlab = "Carat")

## R애 내장된 기본 함수가 아니라 ggplot2를 이용해서 똑같은 히스토그램 만들어 보겠습니다.
ggplot(data = diamonds) + geom_histogram(aes(x = carat))

## ggplot2는 "+"를 이용해서 다양한 형태의 추가적인 정보를 레이어 형식으로 더할 수 있습니다.
ggplot(data = diamonds) + 
  geom_histogram(aes(x = carat), fill = "grey50") + # 히스토그램 막대색 변경
  ylab("Frequency") + xlab("Carots") +
  ggtitle("Count of diamonds by size") +
  theme_bw() # 그래프 배경색 변경

## 아까 만들었던 그래프 폴더에 그래프를 저장할 수 있습니다.
# ggsave(file="./figures/figure1.pdf", width=6.5, height=5)
# ggsave(file="./figures/figure1a.png", width=6.5, height=5, device = "png")
```
```{r, eval = FALSE}
## 표 폴더 만든 것에다가 요약통계표를 저장하기
library(stargazer) # 통계표를 작성하는 데 특화된 패키지입니다.

## 세 가지 변수에 대해 요약통계치를 확인하기
diamonds <- subset(diamonds, select = c("carat", "depth", "price"))
## 몇몇 R 예제 데이터들은 티블로 저장되어 있지 않을 수 있습니다. 
## 이 경우에는 자료를 먼저 티블 유형으로 바꿔주고 시작하는 게 좋습니다.
## 자료 유형을 확인하는 함수는 class(), 혹은 typeof()입니다.
## library(tidyverse) # 아까 불러왔지만, 여기서는 로드하지 않았다고 가정합시다.
class(diamonds)
diamonds <- as_tibble(diamonds)
sum.table1 <- stargazer(diamonds, 
                        covariate.labels=c("Size (carats)", 
                                           "Cut", "Color", 
                                           "Clarity"), 
                        title = "Summary stats for diamond data", 
                        label = "table:summary1")
# write(x=sum.table1, file="./tables/Summary1.tex") # LaTex로 열고 편집할 수 있습니다.

