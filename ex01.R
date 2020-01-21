
library(ggplot2)
x <-c ("a","a","b","c")
x
qplot(x) #������ �󵵱׷��� 
# �󵵱׷��� : � ��ҵ� �߿��� ���� ��Ұ� ��� ����ִ��� 
            #�󵵸� ��Ÿ���� �׷���
y<-c(1,1,1,1,3,3,4,2,5,5,5,5)
y
qplot(y)

#mpg�����ͷ� �׷��� �����
#hwy:���ӵ��ο��� drv:�������(4:���, f:����, r:�ķ�)
qplot(data=mpg, x=cty)
qplot(data=mpg, x=drv, y=hwy)
qplot(data=mpg, x=drv, y=hwy, geom="line")
#"boxplot" �ڽ��÷��� �ſ� �߿��� �׷���
#�ڽ� ���ϴ�:1/4����
#�ڽ� �� ����:�߾Ӱ� �������
#�ڽ� ��� ����:3/4����
#���ϴ�:�ּҰ�  �л��:�ִ밪
#������ �̻�ġ 
qplot(data=mpg, x=drv, y=hwy, geom="boxplot")
qplot(data=mpg, x=drv, y=hwy, geom="boxplot", colour=drv)

#qplot�Լ� �Ŵ��� ��� 
?qplot

#77�� 
y<-c(80,60,70,50,90)
y
avg<-mean(y)
avg

#<<4��.Data Frame>> 
#data frame : ��� ���� ������ ���̺� ���� ������ ����Ÿ
#����  ����  ����  ���� <== �ķ� or ����
#����  26    3.8   2700 <== �� or Case  :����
#����  42    4.2   4000
#����  35    2.6   3500
#������������ �����
# L����<-data.frame(�Ĺ��ε� ��ҵ�) -> �̻�ġ,���������� ->�м�
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
#�м� -> ����� ������ ��ձ��ϱ�
mean(df_midterm$english)
mean(df_midterm$math)
#�� ���� ����� ���
df_midterm<-data.frame(eng=c(90,80,60,70),
                       mat=c(50,60,100,20),
                       cls=c(1,1,2,2))
df_midterm
#88�� ȥ���غ���
df_fru<-data.frame(price=c(1800,1500,3000),
                   num=c(24,38,13))
df_fru
mean(df_fru$price)
mean(df_fru$num)


#�ܺε������̿��ϱ�:read_excel()
#��ġ
install.packages("readxl")
#�ε�
library(readxl)
#�����Ͱ� Ȩ��ο� ������ ��θ� ������� �ʾƵ� �ȴ�
df_exam<-read_excel("excel_exam.xlsx")
df_exam
#�������
mean(df_exam$english)
#�������
mean(df_exam$science)
#�÷����� ���� �� -> col_names = F(false)
df_noexam<-read_excel("excel_exam_novar.xlsx",col_names = F)
df_noexam
#������ ��Ʈ�� �ִ� ������ ��Ʈ��ȣ�� �����Ѵ� 
df_sheet<-read_excel("excel_exam_sheet.xlsx",sheet = 3)
df_sheet
#csv�� �����Լ��ε� �ε��� �� �ִ�.
df_csv_exam<-read.csv("csv_exam.csv")
df_csv_exam

#������ ��� ���:summary()
#factor�� : ����������(�����������)
df_csv_exam<-read.csv("blood.csv",stringsAsFactors = F)
df_csv_exam
summary(df_csv_exam) # Length:10  Class:character  Mode:character 
factor(df_csv_exam) #Levels: c("A", "B", "AB", "O")
#�� �������� �� ���� �ִ°� (��)
df_csv_exam<-read.csv("blood.csv")
df_csv_exam
summary(df_csv_exam) # A :3  AB:2  B :3  O :2  
factor(df_csv_exam) #Levels: c(1, 3, 2, 4)

#�÷��� ���� csv���� -> header = F(false)
df_csv_exam<-read.csv("csv_exam.csv", header = F)
df_csv_exam
#���ڰ� ����ִ� csv���� -> stringsAsFactors = F
df_csv_exam<-read.csv("csv_exam.csv", stringsAsFactors = F)
df_csv_exam

#������������ csv���Ϸ� �����ϱ�
df_midterm
write.csv(df_midterm, file = "df_midterm.csv")
big2008<-read.csv("D:\\bigdata\\2008.csv")
big2008

# RData : R�� ���뵥���� ���� 
#         �а� / ���� / �����뷮 / ����
#RData���Ϸ� �����ϱ� save()
save(df_midterm, file = "df_midterm.rda")
#RData ���� �ҷ����� : ����ϴ� �����������ӿ� �ڵ����� �������
load("df_midterm.rda") 
df_midterm 
rm(df_midterm) # ��ü����

#[ �������ľ��ϱ� ]
exam<-read.csv("csv_exam.csv")
#head() : ������ �պκ����
head(exam)    #���ɼ� �� �տ��� 6�����
head(exam,10) #�տ��� 10�����
#tail() : ������ ���κ����
tail(exam)    #���ɼǽ� �ڿ��� 6�� ���
tail(exam,10) #�ڿ��� 10�����
#View() : ���â���� ������Ȯ��
View(exam)
#dim() : ������ ��,�� ��� <-�߿�
dim(exam)
#str() : ������  �� Ȯ�� <-�߿�
str(exam)
blood<-read.csv("blood.csv")
str(blood) #����Ÿ��:factor w/ 4 levels "A","AB","B","O": 1 1 3 3 2 4 1 2 4 3
blood<-read.csv("blood.csv",stringsAsFactors = F)
str(blood) #����Ÿ��:chr "A" "A" "B" "B"
#summary() : �ִ�/�ּ�/���/�߾Ӱ�/1����/3����
summary(exam)

#106��
#manufacthurer model displ   year  cyl   trans   drv
# ������ü     ��  ��ⷮ  �⵵  ����  ���    ������
#cty       hwy           fl    class
#���ÿ���  ���ӵ��ο���  �⸧  ũ��
mpg<-as.data.frame(ggplot2::mpg)
head(mpg)
head(mpg,8)
tail(mpg)
tail(mpg,15)
dim(mpg) #��,��
str(mpg) #��
summary(mpg)






