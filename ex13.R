
# 지도시각화
library(ggiraphExtra)
str(USArrests)
dim(USArrests)
# Murder 숫자 
# Assault 정수
# UrabanPop정수
# Rape 숫자

head(USArrests)

# 행의 이름을 ==> 컬럼명을 변환
library(tibble)
library(maps)
crime <- rownames_to_column(USArrests, var="state")
# 소문자로 바꿔 다시 저장
crime$state <- tolower(crime$state)

str(crime)

# 위도,경도정보를 포함 maps패키지 --> dataframe형으로
# map_data()를 이용해서 dataFrame형으로 바꿈
library(ggplot2)
states_map <- map_data("state")
str(states_map)
# long 경도(longitude): -87.5 -87.5 -87.5 
# lat 위도(Latitude): 30.4 30.4 30.4
head(states_map)

# crime범재율 데이터 프레임과 states_map지도정보 데이터
# 프레임을 ggChoropleth() 함수를 이용해 매핑.
ggChoropleth(data = crime,        #지도에 표현할 데이터 
             aes(fill = Murder,   #색깔로 표현할 변수
                 map_id = state), #지역 기준변수
             map = states_map)    #지도 데이터
str(states_map)
#crime의 state변수와 states_map의 region의변수 매칭

#인터랙티브: 마우스 움직임에 반응
ggChoropleth(data = crime,        #지도에 표현할 데이터 
             aes(fill = Murder,   #색깔로 표현할 변수
                 map_id = state), #지역 기준변수
             map = states_map,    #지도 데이터
             interactive = T)     #인터랙티브:마우스움직임반응  


# 11-2 대한민국 시도별 인구, 결핵 환자 수 단계 구분도 만들기
library(stringi)
library(devtools)
devtools::install_github("cardiomoon/kormaps2014")
library(kormaps2014)
str(changeCode(korpop1)) # 인코딩

# 변수명 바꾸기
library(dplyr)
korpop1 <- rename(korpop1,
                  pop= 총인구_명,
                  name=행정구역별_읍면동)

str(changeCode(kormap1))
# 단계구분도 만들기
library(ggplot2)
ggChoropleth(data = korpop1,
             aes(fill = pop,
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)
# 결핵환자 수 단계구분도 만들기
str(changeCode(tbc))
ggChoropleth(data = tbc,
             aes(fill = NewPts,
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)

# 12-1인터랙티브 그래프 :
# 마우스의 움직임에 반응해 실시간 형태가 변하는 그래프
# ggplotly()
library(plotly)
p <- ggplot(data = mpg, aes(x=displ, y=hwy, col=drv)) +
  geom_point()
ggplotly(p)

p <- ggplot(data = diamonds, aes(x=cut, fill = clarity)) +
  geom_bar(position = "dodge")
ggplotly(p)


# 12-2 인터렉티브 시계열 그래프만들기
# dygraphs()
library(dygraphs)
economics <-ggplot2::economics
head(economics)
library(xts)
eco <- xts(economics$unemploy, order.by = economics$date)
head(eco)
dygraph(eco) #그래프 생성
# 범위선택이 가능한 시계열 그래프
dygraph(eco) %>% dyRangeSelector()
# 저축률
eco_a <- xts(economics$psavert, order.by = economics$date)
# 실업자 수
# 차이가 커서 비교하기 쉽도 록 1000나누어 축소
eco_b <- xts(economics$unemploy/1000, order.by = economics$date)
# 데이터결합
eco2 <- cbind(eco_a, eco_b)
#변수명 변경
colnames(eco2) <- c("psavert", "unemploy")
head(eco2)
dygraph(eco2) %>% dyRangeSelector()


# 13 통계적 가설 감정
# 유의하다 : 우연 발생할 가능성이 낮다면 유의하다
# 유의확률 : 우연히 발생할 확률
# p-value : 0.05 기준

mpg <- as.data.frame(ggplot2::mpg)
library(dplyr)
mpg_diff <- mpg %>%
  select(class,cty) %>%
  filter(class %in% c("compact", "suv"))
head(mpg_diff)
table(mpg_diff$class)

# t 검정하기
t.test(data = mpg_diff, cty ~ class, var.equal = T)
# p-value < 2.2e-16 
# -> 5%, 0.05보다 작기때문에  결과 compact와 suv간 평균 도시연비
#    차이가 통계적으로 유의함
# compact     mean in group suv 
# 20.12766              13.50000 
# -> suv보다 compact의 도시 연비가 더 높음

# 일반휘발유와 고급휘발유의 도시연비 t검정
mpg_diff2 <- mpg %>%
  select(fl, cty) %>%
  filter(fl %in% c("r", "p"))
table(mpg_diff2$fl)
t.test(data = mpg_diff2, cty ~ fl, var.equal = T)
# p-value = 0.2875
# p-value가 0.05보다 큰 0.2875이므로 실제로는 차이가 없는데 우연에
# 의해 이런 차이가 관찰될 확률이 28.75% 라는 의미
# 따라서 일반휘발유와 고급휘발유를 사용하는 자동차간 도시 연비
# 차이는 통계적으로 유의하지 않음
# mean in group p mean in group r 
# 17.36538        16.73810 
# -> 0.6 차이뿐이므로 우연히 발생했을 가능성이 크다고 해석






