# 13 통계분석기법을 이용한 가설 검정
# 상관관계 : 두 연속 변수가 얼만큼 관계 있느냐 ?
# correlation :
# cor.test() : 상관관계를 측정하는 함수

# 상관계수 : 0 ~ 1 사이의 값을 갖으며, 1에 가까울수록 관련성이 크다.
#           값이 양수면 졍비례, 음수면 반비례관계다.

# 실업자 수와 개인소비지출의 상관관계를 측정하자.
# unemploy     pce
library(ggplot2)
library(dplyr)
economics <- as.data.frame(ggplot2::economics)
cor.test(economics$unemploy, economics$pce)
# p-value < 2.2e-16 : 0.05 미만이므로 유의하다.
# cor 0.6145176  : 정비례 관계이다.


# 상관행렬 히트맵 : DataFrame안에 모든 변수들의 상관관계를 분석하는것

library(corrplot)
head(mtcars)
car_cor <- cor(mtcars) # 상관행렬 생성
round(car_cor, 2) # 소수점 셋째 자리에서 반올림해 출력
corrplot(car_cor) # 상관행렬 히트맵 만들기
# 상관관계가 클수록 : 원의 크기가 크고 색깔이 진함
corrplot(car_cor, method = "number") # 숫자로 표현

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF",
                          "#77AADD", "#4477AA"))
# 여러가지 옵션을 사용한 히트맵
corrplot(car_cor,
         method = "color",
         col = col(200),
         type = "lower",
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = F)


# 14 R Markdown 분석 보고서 만들기



























