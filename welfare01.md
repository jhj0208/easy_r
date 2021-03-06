정현진
July 30, 2020

# 한국인의 삶의 이해 ‘복지패널데이터’

한국보건사회연구원에서 가구의 경제활동을 연구해 정책 지원에 반영할 목적으로 발간하는 조사 자료 전국 7000여 가구 선정,
경제활동, 생활실태, 복지욕구 등 천여개 변수로 구성 엄밀한 절차로 수집, 다양한 변수를 담고 있다

## 1\. 한국복지패널데이터 분석 준비하기

### 데이터 분석 준비하기

#### 1\. 데이터 준비하기

koweps\_hpc10\_2015.betal.sav파일 다운로드해 프로젝트 폴더에 삽입

#### 2\. 패키지 설치 및 로드하기

해당 파일은 spss전용 파일로 되어있다 foreign패키지를 이용해 파일 불러오기

``` r
#eval=f 문서에만 담고 실행하지 않겠다(앞에 있으니)
install.packages("foreign")  # foreign 패키지 설치
library(foreign)             # SPSS 파일 로드
library(dplyr)               # 전처리
library(ggplot2)             # 시각화
library(readxl)              # 엑셀 파일 불러오기
```

#### 3\. 데이터 불러오기

foreign패키지의 read.spss()를 이용해 복지패널데이터 불러오기 복사본 활용

``` r
#데이터 불러오기
raw_welfare <- read.spss(file = "koweps_hpc10_2015_beta1.sav", to.data.frame = T)

#복사본 만들기
welfare <- raw_welfare
```

#### 4\. 데이터 검토하기

``` r
head(welfare)
tail(welfare)
View(welfare)
```

데이터의 구조와 특징을 파악하자

#### 5\. 변수명 바꾸기

분석에 사용할 몇개의 변수를 이해하기 쉬운 변수명으로 바꾸자

``` r
welfare <- rename(welfare,
                  sex = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_region = h10_reg7)
```

### 데이터 분석 절차

  - 1단계. 변수 검토 및 전처리 가장 먼저 분석에 사용할 변수들을 전처리 한다. 변수의 특성을 파악하고 이상치를 정제한 다음
    파생변수를 만든다. 전처리는 분석에 활용할 변수 각각에 대해 실시한다

  - 2단계. 변수 간 관계 분석 전처리가 완료되면 본격적으로 변수 간 관계를 파악하는 분석을 한다
    ![](img/09_01.png)
