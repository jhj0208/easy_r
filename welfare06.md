직업별 월급 차이
================
정현진
July 31, 2020

## 6\. 직업별 월급 차이

어떤 직업이 월급을 제일 많이 받을까?

### 분석 절차

변수 검토 및 전처리 -\> 변수 간 관계 분석  
\-직업 -직업별 월급 평균표 만들기  
\-월급 -그래프 만들기

### 직업 변수 검토 및 전처리하기

#### 1\. 변수 검토하기

직업을 나타내는 code\_job 변수(직업코드) -\> 직업분류코드를 이용해 직업 명칭 변수 만들기

``` r
class(welfare$code_job)
table(welfare$code_job)
```

#### 2\. 전처리

``` r
list_job <- read_excel("Koweps_Codebook.xlsx", col_names=T, sheet = 2)
head(list_job)
dim(list_job)
```

#### 2-1. job변수를 welfare에 결합

welfare와 list\_job데이터 프레임에 공통으로 들어있는 code\_job 변수를 기준으로 결합 후 확인

``` r
welfare <- left_join(welfare, list_job, id = "code_job")
```

    ## Joining, by = "code_job"

``` r
##Joining, by = code_job

welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10)
```

    ##    code_job                                job
    ## 1       942                   경비원 및 검표원
    ## 2       762                             전기공
    ## 3       530 방문 노점 및 통신 판매 관련 종사자
    ## 4       999        기타 서비스관련 단순 종사원
    ## 5       312                    경영관련 사무원
    ## 6       254             문리 기술 및 예능 강사
    ## 7       510                        영업 종사자
    ## 8       530 방문 노점 및 통신 판매 관련 종사자
    ## 9       286   스포츠 및 레크레이션 관련 전문가
    ## 10      521                   매장 판매 종사자

### 직업별 월급 차이 분석하기

월급, 직업 변수의 전처리가 끝. 변수 간 관계 분석

#### 1\. 직업별 월급 평균표 만들기

무직, 월급 없는 사람 제외

``` r
job_income <- welfare %>% 
              filter(!is.na(job) & !is.na(income)) %>% 
              group_by(job) %>% 
              summarise(mean_income = mean(income))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
head(job_income)
```

#### 2\. 상위 10개 추출

어떤 직업의 월급이 많을지, 내림차순 정렬

``` r
top10 <- job_income %>% 
         arrange(desc(mean_income)) %>% 
         head(10)
top10
```

    ## # A tibble: 10 x 2
    ##    job                                  mean_income
    ##    <chr>                                      <dbl>
    ##  1 금속 재료 공학 기술자 및 시험원             845.
    ##  2 의료진료 전문가                             844.
    ##  3 의회의원 고위공무원 및 공공단체임원         750 
    ##  4 보험 및 금융 관리자                         726.
    ##  5 제관원 및 판금원                            572.
    ##  6 행정 및 경영지원 관리자                     564.
    ##  7 문화 예술 디자인 및 영상 관련 관리자        557.
    ##  8 연구 교육 및 법률 관련 관리자               550.
    ##  9 건설 전기 및 생산 관련 관리자               536.
    ## 10 석유 및 화학물 가공장치 조작원              532.

#### 3\. 그래프 만들기

앞의 표를 그래프로, 직업 이름이 기니까 그래프를 90도 회전시킨다

``` r
ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income)) + geom_col() + coord_flip()
```

![](welfare06_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

#### 4\. 하위 10위 추출

``` r
bottom10 <- job_income %>% 
  arrange(mean_income) %>% 
  head(10)
bottom10
```

    ## # A tibble: 10 x 2
    ##    job                          mean_income
    ##    <chr>                              <dbl>
    ##  1 가사 및 육아 도우미                 80.2
    ##  2 임업관련 종사자                     83.3
    ##  3 기타 서비스관련 단순 종사원         88.2
    ##  4 청소원 및 환경 미화원               88.8
    ##  5 약사 및 한약사                      89  
    ##  6 작물재배 종사자                     92  
    ##  7 농립어업관련 단순 종사원           102. 
    ##  8 의료 복지 관련 서비스 종사자       104. 
    ##  9 음식관련 단순 종사원               108. 
    ## 10 판매관련 단순 종사원               117.

#### 5\. 그래프 만들기

top10과 bottom10을 비교할 수 있게 y축을 0부터 850까지 (최대, 최소값)

``` r
ggplot(data = bottom10, aes(x = reorder(job, -mean_income), y = mean_income)) + geom_col() + coord_flip() + ylim(0, 850)
```

![](welfare06_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

\-\> 가장 많은 월급을 받는 사람? 금속 재료 공학 기술자 및 시험원

가장 적은 월급을 받는 사람? 가사 및 육아 도우미
