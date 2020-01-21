# Rhadoop 이란 : R에서 하둡 파티션에 있는 데이터를 분석하는 것

# 준비 : window형 hadoop, R, RStudio 이미 설치되어 있다.
# 패키지 : rmr, rhdfs 필요함
# 다운로드 : https://github.com/RevolutionAnalytics/RHadoop/wiki/Downloads
# Rtools35.exe 필요
# 윈도우에서 tar.gz 형식의 r패키지를 설치할 때 필요함

# 시스템 환경변수 확인 :
# Rtool, R, hadoop, jdk

# R 실행파일에 호환성 : vista 호환, 관리자권한 추가
# hadoop-streaming-2.6.0.jar 를 복사하여 아래처럼 위치시킨다.
# D:\hadoop-2.6.0_64x\bin\hadoop-streaming-2.6.0.jar

# R에서 환경변수
Sys.setenv(JAVA_HOME="C:/Java/jdk1.8.0_221")
Sys.setenv(HADOOP_PREFIX="D:/hadoop-2.6.0_64x")
Sys.setenv(HADOOP_CMD="D:/hadoop-2.6.0_64x/bin/hadoop")
Sys.setenv(HADOOP_STREAMING="D:/hadoop-2.6.0_64x/bin/hadoop-streaming-2.6.0.jar")
Sys.setenv(Rscript="C:/Program Files/R/R-3.4.4/bin/Rscript")

# 의존성 라이브러리 추가 설치
install.packages(c("rJava","Rcpp","RJSONIO","bitops","digest",
                   "functional","stringr","plyr","dplyr","R.methodsS3",
                   "Hmisc","reshape2","memoise","lazyeval","rjson",
                   "Rtools","qplots"))
install.packages(c("Hmisc","qplots"))
# 설정
system.file(package = "functional")
library(rJava)
# rmr과 rhdfs의 설치
# packages에서 install : cran -> tar.gz, zip 변경
######################################
# 테스트
library(rhdfs)
hdfs.init() # 초기화
library(plyr)
library(rmr2)
# 저장소를 로컬로 지정함
rmr.options(backend = 'local')

# 저장소 1에서 10까지 저장하고 small.ints에 저장
small.ints <- to.dfs(1:10)
from.dfs(small.ints)
# 하둡 명령어를 실행해 본다
system("hdfs dfs -rm -R /rsmall2")
result <- mapreduce(
  input = small.ints, output = "/rsmall2",
  map = function(key,val) cbind(val,val^2)
)

out <- from.dfs(result)
out
#============================================================
# 데이터 준비
groups = rbinom(100,n=500,prob=0.5) #이항분포함수
groups = to.dfs(groups)
fromdfs <- from.dfs(groups)
fromdfs
# 분석
system("hdfs dfs -rm -R /out2")
result = mapreduce(
  input = groups,
  output = "/out2",
  output.format = "native",
  map = function(k,v) keyval(v,1),
  reduce = function(k,vv) keyval(k,length(vv))
)
print(result) # 저장위치 출력
print(from.dfs(result)) 
from.dfs("/out2/part-00000")
# 시스템
rmr.options(backend = 'hadoop')
from.dfs("/out2/part-00000")
system("hdfs dfs -cat /out2/part-00000")
system("hdfs dfs -ls -R /")
