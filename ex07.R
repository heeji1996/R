# �Լ����� Ư�� �Ӽ��� �̿��ؼ� ������ ���� �� �����ϱ�
# sum(data,na.rm=T) ,
# mean(data,na.rm=T) ���� na.rm=T ������ �߰��Ͽ� 

exam <- read.csv("csv_exam.csv")
exam[c(3,8,15), "math"] <- NA #3,8,15�� math�� NA�ֱ�
exam
#����ġ �����Ͽ� ��ձ��ϱ�
exam %>% summarise(mean_math=mean(math,na.rm = T))
exam %>% summarise(mean_math=mean(math,na.rm = T),
                   sum_math=sum(math,na.rm = T),
                   median_math=median(math,na.rm = T))

# ������ ��ü�ϱ�
# : �����Ͱ� �ۼ� ����ġ�� ���� ��� �����ϰ� �Ǹ� ���� �����Ͱ� �ս� �� 

# 1.��հ����� ������ ��ü
# NA�̸� 55�� ��ü�ϱ� 
mean(exam$math, na.rm = T)
exam$math <- ifelse(is.na(exam$math), 55, exam$math)
table(is.na(exam$math)) # ����ġ ��ǥ
exam

# 170
mpg <- as.data.frame(ggplot2::mpg)
mpg[c(65,124,131,153,212), "hwy"] <- NA
#Q1
table(is.na(mpg$dry)) #����ġ:0
table(is.na(mpg$hwy)) #����ġ:5
#Q2
mpg %>% filter(!is.na(hwy)) %>% #����ġ ����
  group_by(drv) %>%
  summarise(mean(hwy))

# 2006���� Cancelled ���,CancellationCode ��� ���������� Ƚ���� ���Ͻÿ�
#1)�ν��Ͻ� ����
air <- read.csv("2006.csv", stringsAsFactors = F)
#2)�� �÷��� NA�� �ִ� ��Ȯ��
table(is.na(air$Cancelled))
table(is.na(air$CancellationCode))
#3)NA���� �� Ƚ�� ���ϱ�
air %>% select(Cancelled,CancellationCode) %>%
  filter(Cancelled==1) %>% #��ҵȰ͸�
  group_by(CancellationCode) %>% 
  summarise(count=n())

# �̻�ġ(outlier) �����ϱ�
# : ������ֿ��� ũ�� ��� ���� ���Ѵ�
# �̻�ġ�� ����� �м������ ũ�� �ְ��Ų��
# �̻�ġ ==> ����ġ ==> ����ġ����
outlier <- data.frame(sex=c(1,2,1,3,2,1),
                      score=c(5,4,3,4,2,6))
outlier
table(outlier$sex)
table(outlier$score)
# sex,score �� �̻�ġ�� ����ġ�� �ٲٱ�
outlier$sex <- ifelse(outlier$sex==3, NA, outlier$sex)
outlier$score <- ifelse(outlier$score>5, NA, outlier$score)
# is.na(�÷�), is.omit(��ü), �Ӽ� is.rm = T
# ������ ���� �� ������ ���� score���ϱ�
outlier %>% na.omit() %>%
  group_by(sex) %>%
  summarise(avg_score=mean(score))
#
outlier %>% filter(!is.na(sex) & !is.na(score)) %>%
  group_by(sex) %>%
  summarise(mean_score=mean(score))


# �̻�ġ�����ϱ� - �ش����� ��(�ش�ġ)
# : ���������δ� ������ �� ������ �ش������� ũ�ų� ���� ��
library(ggplot2)
boxplot(mpg$hwy) # ������� ������ �ش�ġ ������
# qplot ��, hist ������, boxplot�ش�ġ
qplot(mpg$hwy)
hist(mpg$hwy)
# ���� �׸� ���ġ ���
boxplot(mpg$hwy)$stats
# [1,]   12 <== ���� ������ �̻�ġ�� �Ǵ�
# [2,]   18
# [3,]   25
# [4,]   27 
# [5,]   37 <== ���� ũ�� �̻�ġ�� �Ǵ�
# �̻�ġ�� ������ �� ���ӵ��� ��տ��� ���Ͻÿ�
mpg$hwy <- ifelse(mpg$hwy<12 | mpg$hwy>37, NA, mpg$hwy) 
table(is.na(mpg$hwy)) # ����ġ : 8
mpg %>% group_by(drv) %>% 
  summarise(mean_hwy=mean(hwy,na.rm = T))

# drv�� ���ú� ��տ���
boxplot(mpg$cty)$stats
mpg$cty <- ifelse(mpg$cty>26 | mpg$cty<9, NA, mpg$cty)
mpg %>% group_by(drv) %>%
  summarise(mean_cty=mean(cty,na.rm = T))

#178
mpg<-as.data.frame(ggplot2::mpg)
mpg[c(10,14,58,93), "drv"] <-"k" #drv �̻�ġ �Ҵ�
mpg[c(229,43,129,203), "cty"] <- c(3,4,39,42) #cty �̻�ġ �Ҵ�
#Q1.�̻�ġ Ȯ�� �� ó�� %in% ���
mpg$drv <- ifelse(mpg$drv %in% c("4","f","r"), mpg$drv, NA)
table(is.na(mpg$drv)) # ����ġ :4
#Q2. cty �̻�ġ�� boxplot�� �̿��ؼ� �̻�ġ�� ����ġ�� ��ȯ
boxplot(mpg$cty)$stats
mpg$cty<- ifelse(mpg$cty<9 | mpg$cty>26, NA, mpg$cty)
table(is.na(mpg$cty)) # ����ġ :9#Q3. drv�� cty ����� ���Ͻÿ�. dplyr�� Ȱ��
mpg %>% filter(!is.na(cty)& !is.na(drv)) %>% 
  group_by(drv) %>%
  summarise(mean_cty=mean(cty))

















