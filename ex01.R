
library(ggplot2)
x <-c ("a","a","b","c")
x
qplot(x) #간단한 빈도그래프 
# 빈도그래프 : 어떤 요소들 중에서 같은 요소가 몇개씩 들어있는지 
            #빈도를 나타내는 그래프
y<-c(1,1,1,1,3,3,4,2,5,5,5,5)
y
qplot(y)

#mpg데이터로 그래프 만들기
#hwy:고속도로연비 drv:구동방식(4:사륜, f:전륜, r:후륜)
qplot(data=mpg, x=cty)
qplot(data=mpg, x=drv, y=hwy)
qplot(data=mpg, x=drv, y=hwy, geom="line")
#"boxplot" 박스플롯은 매우 중요한 그래프
#박스 맨하단:1/4지점
#박스 안 라인:중앙값 참조평균
#박스 상단 라인:3/4지점
#털하단:최소값  털상단:최대값
#점박이 이상치 
qplot(data=mpg, x=drv, y=hwy, geom="boxplot")
qplot(data=mpg, x=drv, y=hwy, geom="boxplot", colour=drv)

#qplot함수 매뉴얼 출력 
?qplot

#77쪽 
y<-c(80,60,70,50,90)
y
avg<-mean(y)
avg

#<<4장.Data Frame>> 
#data frame : 행과 열로 구성된 테이블 같은 형태의 데이타
#성별  연령  학점  연봉 <== 컴럼 or 변수
#남자  26    3.8   2700 <== 행 or Case  :정보
#여자  42    4.2   4000
#남자  35    2.6   3500
#데이터프레임 만들기
# L변수<-data.frame(컴바인된 요소들) -> 이상치,결측지제거 ->분석
english<-c(90,80,60,70)
english
math<-c(50,60,100,20)
math
df_midterm<-data.frame(english, math)
df_midterm
class<-c(1,1,2,2)
class
df_midterm<-data.frame(english,math,class)
df_midterm
#분석 -> 영어와 수학의 평균구하기
mean(df_midterm$english)
mean(df_midterm$math)
#한 번에 만드는 방법
df_midterm<-data.frame(eng=c(90,80,60,70),
                       mat=c(50,60,100,20),
                       cls=c(1,1,2,2))
df_midterm
#88쪽 혼자해보기
df_fru<-data.frame(price=c(1800,1500,3000),
                   num=c(24,38,13))
df_fru
mean(df_fru$price)
mean(df_fru$num)


#외부데이터이용하기:read_excel()
#설치
install.packages("readxl")
#로드
library(readxl)
#데이터가 홈경로에 있으면 경로를 기술하지 않아도 된다
df_exam<-read_excel("excel_exam.xlsx")
df_exam
#영어평균
mean(df_exam$english)
#과학평균
mean(df_exam$science)
#컬럼명이 없을 때 -> col_names = F(false)
df_noexam<-read_excel("excel_exam_novar.xlsx",col_names = F)
df_noexam
#엑셀에 시트가 있는 파일은 시트번호도 명기한다 
df_sheet<-read_excel("excel_exam_sheet.xlsx",sheet = 3)
df_sheet
#csv는 내장함수로도 로드할 수 있다.
df_csv_exam<-read.csv("csv_exam.csv")
df_csv_exam

#심플한 요약 통계:summary()
#factor란 : 범주형변수(데이터종류몇개)
df_csv_exam<-read.csv("blood.csv",stringsAsFactors = F)
df_csv_exam
summary(df_csv_exam) # Length:10  Class:character  Mode:character 
factor(df_csv_exam) #Levels: c("A", "B", "AB", "O")
#각 종류마다 몇 개씩 있는가 (빈도)
df_csv_exam<-read.csv("blood.csv")
df_csv_exam
summary(df_csv_exam) # A :3  AB:2  B :3  O :2  
factor(df_csv_exam) #Levels: c(1, 3, 2, 4)

#컬럼이 없는 csv파일 -> header = F(false)
df_csv_exam<-read.csv("csv_exam.csv", header = F)
df_csv_exam
#문자가 들어있는 csv파일 -> stringsAsFactors = F
df_csv_exam<-read.csv("csv_exam.csv", stringsAsFactors = F)
df_csv_exam

#데이터프레임 csv파일로 저장하기
df_midterm
write.csv(df_midterm, file = "df_midterm.csv")
big2008<-read.csv("D:\\bigdata\\2008.csv")
big2008

# RData : R의 전용데이터 파일 
#         읽고 / 쓰고 / 작응용량 / 빠름
#RData파일로 저장하기 save()
save(df_midterm, file = "df_midterm.rda")
#RData 파일 불러오기 : 사용하던 데이터프레임에 자동으로 만들어짐
load("df_midterm.rda") 
df_midterm 
rm(df_midterm) # 객체삭제

#[ 데이터파악하기 ]
exam<-read.csv("csv_exam.csv")
#head() : 데이터 앞부분출력
head(exam)    #무옵션 시 앞에서 6개출력
head(exam,10) #앞에서 10개출력
#tail() : 데이터 윗부분출력
tail(exam)    #무옵션시 뒤에서 6개 출력
tail(exam,10) #뒤에서 10개출력
#View() : 뷰어창에서 데이터확인
View(exam)
#dim() : 데이터 행,열 출력 <-중요
dim(exam)
#str() : 데이터  형 확인 <-중요
str(exam)
blood<-read.csv("blood.csv")
str(blood) #데이타형:factor w/ 4 levels "A","AB","B","O": 1 1 3 3 2 4 1 2 4 3
blood<-read.csv("blood.csv",stringsAsFactors = F)
str(blood) #데이타형:chr "A" "A" "B" "B"
#summary() : 최대/최소/평균/중앙값/1쿼터/3쿼터
summary(exam)

#106쪽
#manufacthurer model displ   year  cyl   trans   drv
# 제조업체     모델  배기량  년도  기통  기아    구동식
#cty       hwy           fl    class
#도시연비  고속도로연비  기름  크기
mpg<-as.data.frame(ggplot2::mpg)
head(mpg)
head(mpg,8)
tail(mpg)
tail(mpg,15)
dim(mpg) #행,열
str(mpg) #형
summary(mpg)







