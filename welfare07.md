성별로 어떤 직업이 가장 많을까?
================
정현진
August 1, 2020

## 7\. 성별 직업 빈도

성별, 직업 변수 전처리 작업은 미리 함, 변수간 관계 분석

### 분석 절차

변수 검토 및 전처리 -성별 -직업

변수 간 관계 분석 -성별 직업 빈도표 만들기 - 그래프 만들기

### 성별 직업 빈도 분석하기

#### 1\. 성별 직업 빈도표 만들기

각 성별로 직업별 빈도를 구해 상위 10개 추출 무직이나 월급 없는 사람 제외

#### 1-1. 남성 직업 빈도 상위 10개 추출

``` r
job_male <- welfare %>% 
            filter(!is.na(job) & sex == "male") %>%   
            group_by(job) %>% 
            summarise(n = n()) %>% 
            arrange(desc(n)) %>% 
            head(10)
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
job_male
```

#### 1-2. 여성 직업 빈도 상위 10개 추출 \`\`\`

``` r
job_female <- welfare %>% 
            filter(!is.na(job) & sex == "female") %>%   
            group_by(job) %>% 
            summarise(n = n()) %>% 
            arrange(desc(n)) %>% 
            head(10)
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
job_female
```

#### 2\. 그래프 만들기

#### 2-1. 남성 직업 빈도 상위 10개 직업 그래프

``` r
ggplot (data = job_male, aes(x = reorder(job, n), y= n)) + geom_col() + coord_flip()
```

![](welfare07_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

#### 2-2. 여성 직업 빈도 상위 10개 직업 그래프

``` r
ggplot (data = job_female, aes(x = reorder(job, n), y= n)) + geom_col() + coord_flip()
```

![](welfare07_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
