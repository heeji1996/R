
# 연령대, 종교유/무, 결혼상태별 비율표 만들기
a_r_m <- welfare %>%
  filter(!is.na(group_marriage) & ageg!="young") %>%
  group_by(ageg, religion, group_marriage) %>%
  summarise(n=n()) %>%
  mutate(tot_group=sum(n)) %>%
  mutate(pct=round(n/tot_group*100,1))
a_r_m
# 연령대, 종교유/무 별 이혼표
df_divorce <- a_r_m %>%
  filter(group_marriage=="divorce") %>%
  select(ageg,religion,pct)
df_divorce
# 그래프
ggplot(data = df_divorce, aes(x=ageg, y=pct, fill=religion)) +
  geom_col(position = "dodge")
  #position = "dodge": 분리된 bar그래프로 생성]

# 분석법 : 집계(그룹)를 포함하는 분석.
# 집계 후 새로움 컬럼(변수)을 추가하여 출력하는 법


# 9-9 지역별 연령대 비율
#1. 서울   2. 수도권(인천/경기)  3. 부산/경남/울산   4.대구/경북   
#5. 대전/충남   6. 강원/충북    7.광주/전남/전북/제주도

#1.검토하기
class(welfare$code_region) # numeric
table(welfare$code_region) # 7개그룹

#2.전처리 
#(문자열 지역컬럼을 추가)
list_region <- data.frame(code_region=c(1:7),
                          region=c("서울","수도권(인천/경기)",
                          "부산/경남/울산", "대구/경북",
                          "대전/충남", "강원/충북",
                          "광주/전남/전북/제주도"))
list_region
#두개의 데이터프레임 병합(지역명 변수 추가)
welfare <- left_join(welfare,list_region,id="code_region")
welfare %>% select(code_region,region) %>%
  head

#3.분석하기
# 지역별 연령대 비율표
region_ageg <- welfare %>% 
  group_by(region, ageg) %>% # 7x3
  summarise(n=n()) %>%       # 21가지
  mutate(tot_group=sum(n)) %>%  # 지역별 합계 : 7가지
  mutate(pct=round(n/tot_group*100,2))
region_ageg
ggplot(data = region_ageg, aes(x=region,y=pct,fill=ageg)) +
  geom_col() + coord_flip()

# 노년층 비율 (내림차순)
list_order_old <- region_ageg %>%
  filter(ageg=="old") %>%
  select(region,pct) %>%
  arrange(pct)
list_order_old
# 지역명 순서 변수만들기
order <- list_order_old$region
order
# 노년층 비율이 높은 순으로 그래프 만들기
ggplot(data = list_order_old, aes(x=reorder(region,-pct),y=pct)) +
  geom_col() + coord_flip()

#지역별 연령대 그래프
#1.노년층비율 순으로 그래프만들기
ggplot(data = region_ageg, aes(x=region,y=pct,fill=ageg)) +
  geom_col() + coord_flip() + 
  scale_x_discrete(limits=order)

#2.연령대 순으로 그래프만들기
class(region_ageg$ageg)
levels(region_ageg$ageg)


# 2008.csv에서 12월(Month) 예약취소(Cancelled)된 
# 사유별(CancellationCode) 4합계를 구한 후 사유별 취소율 구하기

#데이터 프레임 생성
air_2008 <- read.csv("2008.csv",stringsAsFactors = T)
air <- air_2008
# 변수확인 : 데이터형? NA포함?
class(air$Month)            #integer
class(air$Cancelled)        #integer
class(air$CancellationCode) #factor
table(air$Cancelled)        # 1-취소
table(air$CancellationCode) # A B C D
table(is.na(air$Cancelled)) #NA 없음
table(is.na(air$CancellationCode)) #NA 없음

# 전처리
# Month 월로 조인
list_Month <- data.frame(Month=c(1:12),
                Month_h=c("1월","2월","3월","4월",
                  "5월","6월","7월","8월",
                  "9월","10월","11월","12월"))
list_Month
air <- left_join(air, list_Month, code="Month")
# "" ->  NA로 변경
#air <- ifelse(air$CancellationCode =="", NA, air$CancellationCode)

cancel_code <- air %>%
  filter(Month==12 & Cancelled==1 ) %>%
  group_by(Month,CancellationCode) %>%
  select(Month, Cancelled, CancellationCode) %>%
  summarise(code_count=n())
cancel_code

# 분석
cancel <- air %>% 
  filter(Cancelled==1) %>%
  group_by(Month_h,CancellationCode) %>%
  select(Month_h,CancellationCode) %>%
  summarise(n=n()) %>%
  mutate(tot_code=sum(n)) %>%
  mutate(pct=round(n/tot_code*100,1))
cancel
# 시각화
ggplot(data = cancel, aes(x=Month_h, y=pct, fill=CancellationCode)) +
  geom_col() +coord_flip()



































