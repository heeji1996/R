# Rhadoop �̶� : R���� �ϵ� ��Ƽ�ǿ� �ִ� �����͸� �м��ϴ� ��

# �غ� : window�� hadoop, R, RStudio �̹� ��ġ�Ǿ� �ִ�.
# ��Ű�� : rmr, rhdfs �ʿ���
# �ٿ�ε� : https://github.com/RevolutionAnalytics/RHadoop/wiki/Downloads
# Rtools35.exe �ʿ�
# �����쿡�� tar.gz ������ r��Ű���� ��ġ�� �� �ʿ���

# �ý��� ȯ�溯�� Ȯ�� :
# Rtool, R, hadoop, jdk

# R �������Ͽ� ȣȯ�� : vista ȣȯ, �����ڱ��� �߰�
# hadoop-streaming-2.6.0.jar �� �����Ͽ� �Ʒ�ó�� ��ġ��Ų��.
# D:\hadoop-2.6.0_64x\bin\hadoop-streaming-2.6.0.jar

# R���� ȯ�溯��
Sys.setenv(JAVA_HOME="C:/Java/jdk1.8.0_221")
Sys.setenv(HADOOP_PREFIX="D:/hadoop-2.6.0_64x")
Sys.setenv(HADOOP_CMD="D:/hadoop-2.6.0_64x/bin/hadoop")
Sys.setenv(HADOOP_STREAMING="D:/hadoop-2.6.0_64x/bin/hadoop-streaming-2.6.0.jar")
Sys.setenv(Rscript="C:/Program Files/R/R-3.4.4/bin/Rscript")

# ������ ���̺귯�� �߰� ��ġ
install.packages(c("rJava","Rcpp","RJSONIO","bitops","digest",
                   "functional","stringr","plyr","dplyr","R.methodsS3",
                   "Hmisc","reshape2","memoise","lazyeval","rjson",
                   "Rtools","qplots"))
install.packages(c("Hmisc","qplots"))
# ����
system.file(package = "functional")
library(rJava)
# rmr�� rhdfs�� ��ġ
# packages���� install : cran -> tar.gz, zip ����
######################################
# �׽�Ʈ
library(rhdfs)
hdfs.init() # �ʱ�ȭ
library(plyr)
library(rmr2)
# ����Ҹ� ���÷� ������
rmr.options(backend = 'local')

# ����� 1���� 10���� �����ϰ� small.ints�� ����
small.ints <- to.dfs(1:10)
from.dfs(small.ints)
# �ϵ� ���ɾ ������ ����
system("hdfs dfs -rm -R /rsmall2")
result <- mapreduce(
  input = small.ints, output = "/rsmall2",
  map = function(key,val) cbind(val,val^2)
)

out <- from.dfs(result)
out
#============================================================
# ������ �غ�
groups = rbinom(100,n=500,prob=0.5) #���׺����Լ�
groups = to.dfs(groups)
fromdfs <- from.dfs(groups)
fromdfs
# �м�
system("hdfs dfs -rm -R /out2")
result = mapreduce(
  input = groups,
  output = "/out2",
  output.format = "native",
  map = function(k,v) keyval(v,1),
  reduce = function(k,vv) keyval(k,length(vv))
)
print(result) # ������ġ ���
print(from.dfs(result)) 
from.dfs("/out2/part-00000")
# �ý���
rmr.options(backend = 'hadoop')
from.dfs("/out2/part-00000")
system("hdfs dfs -cat /out2/part-00000")
system("hdfs dfs -ls -R /")