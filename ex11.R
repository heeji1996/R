
# ���ɴ�, ������/��, ��ȥ���º� ����ǥ �����
a_r_m <- welfare %>%
  filter(!is.na(group_marriage) & ageg!="young") %>%
  group_by(ageg, religion, group_marriage) %>%
  summarise(n=n()) %>%
  mutate(tot_group=sum(n)) %>%
  mutate(pct=round(n/tot_group*100,1))
a_r_m
# ���ɴ�, ������/�� �� ��ȥǥ
df_divorce <- a_r_m %>%
  filter(group_marriage=="divorce") %>%
  select(ageg,religion,pct)
df_divorce
# �׷���
ggplot(data = df_divorce, aes(x=ageg, y=pct, fill=religion)) +
  geom_col(position = "dodge")
  #position = "dodge": �и��� bar�׷����� ����]

# �м��� : ����(�׷�)�� �����ϴ� �м�.
# ���� �� ���ο� �÷�(����)�� �߰��Ͽ� ����ϴ� ��


# 9-9 ������ ���ɴ� ����
#1. ����   2. ������(��õ/���)  3. �λ�/�泲/���   4.�뱸/���   
#5. ����/�泲   6. ����/���    7.����/����/����/���ֵ�

#1.�����ϱ�
class(welfare$code_region) # numeric
table(welfare$code_region) # 7���׷�

#2.��ó�� 
#(���ڿ� �����÷��� �߰�)
list_region <- data.frame(code_region=c(1:7),
                          region=c("����","������(��õ/���)",
                          "�λ�/�泲/���", "�뱸/���",
                          "����/�泲", "����/���",
                          "����/����/����/���ֵ�"))
list_region
#�ΰ��� ������������ ����(������ ���� �߰�)
welfare <- left_join(welfare,list_region,id="code_region")
welfare %>% select(code_region,region) %>%
  head

#3.�м��ϱ�
# ������ ���ɴ� ����ǥ
region_ageg <- welfare %>% 
  group_by(region, ageg) %>% # 7x3
  summarise(n=n()) %>%       # 21����
  mutate(tot_group=sum(n)) %>%  # ������ �հ� : 7����
  mutate(pct=round(n/tot_group*100,2))
region_ageg
ggplot(data = region_ageg, aes(x=region,y=pct,fill=ageg)) +
  geom_col() + coord_flip()

# ����� ���� (��������)
list_order_old <- region_ageg %>%
  filter(ageg=="old") %>%
  select(region,pct) %>%
  arrange(pct)
list_order_old
# ������ ���� ���������
order <- list_order_old$region
order
# ����� ������ ���� ������ �׷��� �����
ggplot(data = list_order_old, aes(x=reorder(region,-pct),y=pct)) +
  geom_col() + coord_flip()

#������ ���ɴ� �׷���
#1.��������� ������ �׷��������
ggplot(data = region_ageg, aes(x=region,y=pct,fill=ageg)) +
  geom_col() + coord_flip() + 
  scale_x_discrete(limits=order)

#2.���ɴ� ������ �׷��������
class(region_ageg$ageg)
levels(region_ageg$ageg)


# 2008.csv���� 12��(Month) �������(Cancelled)�� 
# ������(CancellationCode) 4�հ踦 ���� �� ������ ����� ���ϱ�

#������ ������ ����
air_2008 <- read.csv("2008.csv",stringsAsFactors = T)
air <- air_2008
# ����Ȯ�� : ��������? NA����?
class(air$Month)            #integer
class(air$Cancelled)        #integer
class(air$CancellationCode) #factor
table(air$Cancelled)        # 1-���
table(air$CancellationCode) # A B C D
table(is.na(air$Cancelled)) #NA ����
table(is.na(air$CancellationCode)) #NA ����

# ��ó��
# Month ���� ����
list_Month <- data.frame(Month=c(1:12),
                Month_h=c("1��","2��","3��","4��",
                  "5��","6��","7��","8��",
                  "9��","10��","11��","12��"))
list_Month
air <- left_join(air, list_Month, code="Month")
# "" ->  NA�� ����
#air <- ifelse(air$CancellationCode =="", NA, air$CancellationCode)

cancel_code <- air %>%
  filter(Month==12 & Cancelled==1 ) %>%
  group_by(Month,CancellationCode) %>%
  select(Month, Cancelled, CancellationCode) %>%
  summarise(code_count=n())
cancel_code

# �м�
cancel <- air %>% 
  filter(Cancelled==1) %>%
  group_by(Month_h,CancellationCode) %>%
  select(Month_h,CancellationCode) %>%
  summarise(n=n()) %>%
  mutate(tot_code=sum(n)) %>%
  mutate(pct=round(n/tot_code*100,1))
cancel
# �ð�ȭ
ggplot(data = cancel, aes(x=Month_h, y=pct, fill=CancellationCode)) +
  geom_col() +coord_flip()


































