library(dplyr)
is.na() #na�̰� �ִ��� �˻� /����ġ
table() #������

filter()
select() 
group_by() #�׷� ����
summarise() #������
mutate() #�÷��߰�
boxplot() #�̻�ġ
boxplot()$stats #�ش�ġ
# �ش�ġ ó��: �ش�ġ  --> NA --> ����ġ ����

#181
# �������� �׷��� �׸���
library(ggplot2)
mpg <- as.data.frame(ggplot2::mpg)
midwest <- as.data.frame(ggplot2::midwest)
# ������ scater Plot : ���ӵ� �� ���� ���踦 ǥ���� �� 
                      # ���� ? ����� �ݺ��....
# �׷��� �׸��� ����
# 1�ܰ� : ������,�༳�� - x��, y�� : ggplot(data=, aes(x=,y=))
# 2�ܰ� : �׷������� - �� ���� �� �� : + geom_point()
# 3�ܰ� : ���μ��� - �����, ��, ǥ�� �� : + xlim() + ylim()
ggplot(data = mpg, aes(x=displ, y=hwy)) + 
  geom_point()  +
  xlim(3,6) + ylim(10,30)
#188
#Q1
ggplot(data = mpg, aes(x=cty,y=hwy)) +
  geom_point()

#Q2
ggplot(data = midwest,aes(x=poptotal, y=popasian)) +
  geom_point() +
  xlim(10000,500000) + ylim(1000,10000)

# ���� �׷���geom_col() - ���ܰ��� ���̸� ǥ�� 
# ��) �ڵ��� ������ ����
# ������ĺ� ��� ���ӵ��� ����
# (�׷��Լ��� �׷���ȭ �� ��)
df_mpg <- mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy=mean(hwy))
df_mpg
ggplot(data = df_mpg,aes(x=drv, y=mean_hwy)) +geom_col()
# ũ������� �����ϱ� reoder() -��ȣ�� ���̸� ������������ ����
ggplot(data=df_mpg,aes(x=reorder(drv, -mean_hwy), y=mean_hwy)) +
  geom_col()

# geom_bar() : �� �Ѱ��� ���� x���� �󵵱׷��� �׸� ��
              # x���� ������ �� ���� ������ ��Ÿ���� �󵵷� ���
ggplot(data = mpg, aes(x=drv)) + geom_bar()
ggplot(data = mpg, aes(x=hwy)) + geom_bar()

#193
#Q1. ȸ�纰 suv�� cty��� ���� ������ ���� �׷�����
mpg_suv <- mpg %>%
  filter(class=="suv") %>%
  group_by(manufacturer) %>%
  summarise(mean_cty=mean(cty)) %>%
  arrange(desc(mean_cty)) %>%
  head(5)
mpg_suv
ggplot(data = mpg_suv, aes(x=reorder(x=manufacturer, -mean_cty), y=mean_cty))+
  geom_col()
#Q2. classd�� ��
ggplot(data = mpg, aes(x=class)) + geom_bar()

# �����׷��� geom_line : �ð迭 �����͸� ǥ��
# �ð迭 �����Ͷ�? ������ �ð� ������ �ΰ� ������ �����͸� ���Ѵ�.
                # ex) ȯ��, �ְ�, 
economics <-as.data.frame(ggplot2::economics)
dim(economics)
str(economics)
head(economics,6)
ggplot(data = economics, aes(x=date, y=unemploy)) + 
  geom_line()
#Q1. �ð��� ���� psavert(���������) ��ȭ
ggplot(data = economics, aes(x=date, y=psavert)) +
  geom_line()
#Q2. �Ǿ��ڿ� �������� ����
ggplot(data = economics, aes(x=psavert, y=unemploy)) +
  geom_point()

# ���ڱ׸��׷��� - ���� �� ���� ���� ǥ��
#�ڵ��� ������ĺ� ���ӵ��� ���� �����׷���
ggplot(data = mpg, aes(x=drv, y=hwy)) + 
  geom_boxplot()

# ��) ���ܰ��� ���� -����׷���
#�ڵ��� ������ĺ��� ���ӵ��� ���� �׷���
ggplot(data = mpg, aes(x=drv, y=hwy)) +
  geom_col()

#198
#Q1
mpg_class <- mpg %>%
  filter(class %in% c("compact","subcompact","suv"))
ggplot(data = mpg_class, aes(x=class, y=cty)) +
  geom_boxplot()

# ���������� �м�
# �ѱ������гε����� �м�
install.packages("foreign")
library(foreign) # SPSS���� �ҷ�����
library(dplyr)   # ��ó��
library(ggplot2) # �ð�ȭ  
library(readxl)  # ���� ���� �ҷ�����

# �����ͺҷ�����
raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav",
                         to.data.frame = T)
#���纻 �����
welfare <- raw_welfare

# ������ �����ϱ�
head(welfare,1)
tail(welfare)
dim(welfare)
str(welfare)

# �м��� �����ϰ� �ϱ����� �÷��� ����
welfare <- rename(welfare,
                  sex = h10_g3,         #����
                  birth = h10_g4,       #�¾ ����
                  marriage = h10_g10,   #ȥ�λ���
                  religion = h10_g11,   #����
                  income = p1002_8aq1,  #����
                  code_job = h10_eco9,  #�����ڵ�
                  code_region = h10_reg7)#�����ڵ� 
# ������ ��ó��
table(welfare$sex)
table(is.na(welfare$income))
table(is.na(welfare$birth))




