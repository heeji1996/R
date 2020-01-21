
# �����ð�ȭ
library(ggiraphExtra)
str(USArrests)
dim(USArrests)
# Murder ���� 
# Assault ����
# UrabanPop����
# Rape ����

head(USArrests)

# ���� �̸��� ==> �÷����� ��ȯ
library(tibble)
library(maps)
crime <- rownames_to_column(USArrests, var="state")
# �ҹ��ڷ� �ٲ� �ٽ� ����
crime$state <- tolower(crime$state)

str(crime)

# ����,�浵������ ���� maps��Ű�� --> dataframe������
# map_data()�� �̿��ؼ� dataFrame������ �ٲ�
library(ggplot2)
states_map <- map_data("state")
str(states_map)
# long �浵(longitude): -87.5 -87.5 -87.5 
# lat ����(Latitude): 30.4 30.4 30.4
head(states_map)

# crime������ ������ �����Ӱ� states_map�������� ������
# �������� ggChoropleth() �Լ��� �̿��� ����.
ggChoropleth(data = crime,        #������ ǥ���� ������ 
             aes(fill = Murder,   #����� ǥ���� ����
                 map_id = state), #���� ���غ���
             map = states_map)    #���� ������
str(states_map)
#crime�� state������ states_map�� region�Ǻ��� ��Ī

#���ͷ�Ƽ��: ���콺 �����ӿ� ����
ggChoropleth(data = crime,        #������ ǥ���� ������ 
             aes(fill = Murder,   #����� ǥ���� ����
                 map_id = state), #���� ���غ���
             map = states_map,    #���� ������
             interactive = T)     #���ͷ�Ƽ��:���콺�����ӹ���  


# 11-2 ���ѹα� �õ��� �α�, ���� ȯ�� �� �ܰ� ���е� �����
library(stringi)
library(devtools)
devtools::install_github("cardiomoon/kormaps2014")
library(kormaps2014)
str(changeCode(korpop1)) # ���ڵ�

# ������ �ٲٱ�
library(dplyr)
korpop1 <- rename(korpop1,
                  pop= ���α�_��,
                  name=����������_���鵿)

str(changeCode(kormap1))
# �ܰ豸�е� �����
library(ggplot2)
ggChoropleth(data = korpop1,
             aes(fill = pop,
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)
# ����ȯ�� �� �ܰ豸�е� �����
str(changeCode(tbc))
ggChoropleth(data = tbc,
             aes(fill = NewPts,
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)

# 12-1���ͷ�Ƽ�� �׷��� :
# ���콺�� �����ӿ� ������ �ǽð� ���°� ���ϴ� �׷���
# ggplotly()
library(plotly)
p <- ggplot(data = mpg, aes(x=displ, y=hwy, col=drv)) +
  geom_point()
ggplotly(p)

p <- ggplot(data = diamonds, aes(x=cut, fill = clarity)) +
  geom_bar(position = "dodge")
ggplotly(p)


# 12-2 ���ͷ�Ƽ�� �ð迭 �׷��������
# dygraphs()
library(dygraphs)
economics <-ggplot2::economics
head(economics)
library(xts)
eco <- xts(economics$unemploy, order.by = economics$date)
head(eco)
dygraph(eco) #�׷��� ����
# ���������� ������ �ð迭 �׷���
dygraph(eco) %>% dyRangeSelector()
# �����
eco_a <- xts(economics$psavert, order.by = economics$date)
# �Ǿ��� ��
# ���̰� Ŀ�� ���ϱ� ���� �� 1000������ ���
eco_b <- xts(economics$unemploy/1000, order.by = economics$date)
# �����Ͱ���
eco2 <- cbind(eco_a, eco_b)
#������ ����
colnames(eco2) <- c("psavert", "unemploy")
head(eco2)
dygraph(eco2) %>% dyRangeSelector()


# 13 ����� ���� ����
# �����ϴ� : �쿬 �߻��� ���ɼ��� ���ٸ� �����ϴ�
# ����Ȯ�� : �쿬�� �߻��� Ȯ��
# p-value : 0.05 ����

mpg <- as.data.frame(ggplot2::mpg)
library(dplyr)
mpg_diff <- mpg %>%
  select(class,cty) %>%
  filter(class %in% c("compact", "suv"))
head(mpg_diff)
table(mpg_diff$class)

# t �����ϱ�
t.test(data = mpg_diff, cty ~ class, var.equal = T)
# p-value < 2.2e-16 
# -> 5%, 0.05���� �۱⶧����  ��� compact�� suv�� ��� ���ÿ���
#    ���̰� ��������� ������
# compact     mean in group suv 
# 20.12766              13.50000 
# -> suv���� compact�� ���� ���� �� ����

# �Ϲ��ֹ����� �����ֹ����� ���ÿ��� t����
mpg_diff2 <- mpg %>%
  select(fl, cty) %>%
  filter(fl %in% c("r", "p"))
table(mpg_diff2$fl)
t.test(data = mpg_diff2, cty ~ fl, var.equal = T)
# p-value = 0.2875
# p-value�� 0.05���� ū 0.2875�̹Ƿ� �����δ� ���̰� ���µ� �쿬��
# ���� �̷� ���̰� ������ Ȯ���� 28.75% ��� �ǹ�
# ���� �Ϲ��ֹ����� �����ֹ����� ����ϴ� �ڵ����� ���� ����
# ���̴� ��������� �������� ����
# mean in group p mean in group r 
# 17.36538        16.73810 
# -> 0.6 ���̻��̹Ƿ� �쿬�� �߻����� ���ɼ��� ũ�ٰ� �ؼ�





