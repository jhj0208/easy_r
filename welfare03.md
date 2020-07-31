나이와 월급의 관계
================
정현진
July 30, 2020

## 3\. 나이와 월급의 관계

나이 변수를 검토하고 전처리, 월급 변수는 생략 후 변수간 관계 분석

### 분석 절차

변수 검토 및 전처리(나이, 월급) -\> 변수간 관계 분석(나이에 따른 월급 평균표 만들기, 그래프 만들기)

#### 1\. 변수 검토하기

나이와 월급의 관계를 분석하려면 나이 변수가 필요한데 없다. 년도 변수를 이용해 검토한 후 나이 변수 만들기

``` r
class(welfare$birth)
```

    ## [1] "numeric"

``` r
summary(welfare$birth)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    1907    1946    1966    1968    1988    2014

``` r
qplot(welfare$ birth)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](welfare03_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

#### 2\. 전처리

``` r
summary(welfare$birth) #이상치 확인
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    1907    1946    1966    1968    1988    2014

``` r
table(is.na(welfare$birth)) #결측치확인
```

    ## 
    ## FALSE 
    ## 16664

``` r
#이상치 결측 처리
welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))
```

    ## 
    ## FALSE 
    ## 16664

\-\> 이상치와 결측치 없음

#### 3\. 파생변수 만들기 - 나이

2015년에 조사가 진행, 2015년-태어난 년도+1 = 나이

``` r
welfare$age <- 2015 - welfare$birth +1
summary(welfare$age)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    2.00   28.00   50.00   48.43   70.00  109.00

``` r
qplot(welfare$age)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](welfare03_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### 나이와 월급의 관계 분석하기

#### 1\. 나이에 따른 월급 평균표 만들기

``` r
age_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
head(age_income)
```

    ## # A tibble: 6 x 2
    ##     age mean_income
    ##   <dbl>       <dbl>
    ## 1    20        121.
    ## 2    21        106.
    ## 3    22        130.
    ## 4    23        142.
    ## 5    24        134.
    ## 6    25        145.

#### 2\. 그래프 만들기

``` r
ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_line()
```

![](welfare03_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
