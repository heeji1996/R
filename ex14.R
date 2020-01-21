# 13 ���м������ �̿��� ���� ����
# ������� : �� ���� ������ ��ŭ ���� �ִ��� ?
# correlation :
# cor.test() : ������踦 �����ϴ� �Լ�

# ������ : 0 ~ 1 ������ ���� ������, 1�� �������� ���ü��� ũ��.
#           ���� ����� �����, ������ �ݺ�ʰ����.

# �Ǿ��� ���� ���μҺ������� ������踦 ��������.
# unemploy     pce
library(ggplot2)
library(dplyr)
economics <- as.data.frame(ggplot2::economics)
cor.test(economics$unemploy, economics$pce)
# p-value < 2.2e-16 : 0.05 �̸��̹Ƿ� �����ϴ�.
# cor 0.6145176  : ����� �����̴�.


# ������ ��Ʈ�� : DataFrame�ȿ� ��� �������� ������踦 �м��ϴ°�

library(corrplot)
head(mtcars)
car_cor <- cor(mtcars) # ������ ����
round(car_cor, 2) # �Ҽ��� ��° �ڸ����� �ݿø��� ���
corrplot(car_cor) # ������ ��Ʈ�� �����
# ������谡 Ŭ���� : ���� ũ�Ⱑ ũ�� ������ ����
corrplot(car_cor, method = "number") # ���ڷ� ǥ��

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF",
                          "#77AADD", "#4477AA"))
# �������� �ɼ��� ����� ��Ʈ��
corrplot(car_cor,
         method = "color",
         col = col(200),
         type = "lower",
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = F)


# 14 R Markdown �м� ������ �����


























