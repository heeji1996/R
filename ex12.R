
# < �ؽ�Ʈ ���̴�> : �ڿ��� ó��, �� ���ڷ� �� �����Ϳ��� 
#                     ��ġ�ִ� ������ ��� ���� ��
# ���¼� �м� ==> ���� ���� ������� �ǹ̸� ���� �ܾ ��ŭ 
#                 �����ϴ��� �ľ��ϴ� ��
# ũ�Ѹ� : sns�� ������Ʈ�� �ö�� �����͸� �����ϴ� ��.
# �ʿ��� ��Ű�� : rJava, memoise, KoNLP

# ��Ű���ε�
library(KoNLP)
library(dplyr)
library(stringr) #Ư������ ����
useNIADic() # �ѱ��� ���� �ε�

# �ؽ�Ʈ �ҷ�����
txt <- readLines("hiphop.txt")
head(txt)

# Ư�� ���������ϱ�(��ó�� �۾�)
txt <- str_replace_all(txt,"\\W"," ")

# ���� ���� ���� �ܾ� �˾ƺ���
#1.��������
extractNoun("���ѹα��� ����� �ѹݵ��� �� �μӵ����� �Ѵ�")
nouns <- extractNoun(txt)
table(unlist(nouns))
#1.��Į��scala - ũ��
#2.����vector - ���⼺
#3.��Ʈ����matrix - ����⼺
#4.�ټ�tensor - ��ü��������

# nulist(nouns) ��� ���� ��Ҹ� vector����ȭ ��Ų��.
#2.������ ���� list�� ���ڿ� ���ͷ� ��ȯ, �ܾ ��ǥ ���� 
wordcount <- table(unlist(nouns))
wordcount
#3.������ ���������� ��ȯ
df_word <- as.data.frame(wordcount,stringsAsFactors = F)
#4.������ ����
df_word <- rename(df_word, word=Var1, freq=Freq)
head(df_word,10)
#5.���� ���Ǵ� �ܾ� ��ǥ �����
# �α��� �̻� �ܾ� ����
df_word <- filter(df_word, nchar(word) >=2 )
top_20 <- df_word %>% 
  arrange(desc(freq)) %>%
  head(20)
top_20


# ����Ŭ���� �����
# ����Ŭ���� : �ܾ��� �󵵸� ����������� ǥ���� �׷���
#               �󸶳� ���� ���Ǿ����� �Ѵ��� �ľ�
# ��Ű�� : wordcloud, RcolorBrewer(���ڻ���ǥ��)
library(wordcloud)
library(RColorBrewer)
pal <- brewer.pal(8, "Dark2") #8������
set.seed(1234)
wordcloud(words = df_word$word, # �ܾ�
          freq = df_word$freq,  # ��
          min.freq = 2,         # �ּ� �ܾ� ��
          max.words = 200,      # ǥ�� �ܾ� ��
          random.order = F,     # ���� �ܾ� �߾� ��ġ
          rot.per = .1,         # ȸ�� �ܾ� ����
          scale = c(4, 0.3),    # �ܾ� ũ�� ����
          colors = pal)         # ���� ���


# �ؽ�Ʈ ���̴�
#1.�����ͷε�
twitter <- read.csv("twitter.csv",
                    header = T,
                    stringsAsFactors = F,
                    fileEncoding = "UTF-8")
#2.������ ����
twitter <- rename(twitter,
                  no=��ȣ,
                  id=�����̸�,
                  date=�ۼ���,
                  tw=����)
#3.Ư������ ����
twitter$tw <- str_replace_all(twitter$tw, "\\W"," ")
str(twitter)
head(twitter$tw)
#4.��������
nouns <- extractNoun(twitter$tw)
#5.������ ���� list�� ���ڿ� ���ͷ� ��ȯ, �ܾ ��ǥ ����
wordcount <- table(unlist(nouns))
#6.���������������� ��ȯ
df_word <- as.data.frame(wordcount,stringsAsFactors = F)
#7.������ ����
df_word <- rename(df_word,
                  word=Var1,
                  freq=Freq)
#8.�� ���� �̻� �ܾ� ����
df_word <- filter(df_word, nchar(word)>=2)
#9.���� 20�� ����
top20 <- df_word %>%
  arrange(desc(freq)) %>%
  head(20)
top20

# �ܾ� �� ���� �׷��� �����
library(ggplot2)
order <- arrange(top20,freq)$word #�󵵼��� ���� ����

ggplot(data = top20, aes(x=word,y=freq)) +
  ylim(0,2500) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit=order) +
  geom_text(aes(label=freq), hjust= -0.3)

# ����Ŭ���� �����
#pal <- brewer.pal(8,"Dark2")
pal <- brewer.pal(9,"Blues")[5:9]
set.seed(123)

wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 10,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(6, 0.2),
          colors = pal)
warnings()



















