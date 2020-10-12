install.packages("rJava")
install.packages("memoise")
install.packages("multilinguer")
install.packages(c("stringr", "hash", "tau", "sejong", "RSQLite", "devtools"), type = "binary")
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = 'never', INSTALL_opts=c("--no-multiarch"))

Sys.setenv(JAVA_HOME='C:Program Files/java/jre1.8.0_211')

library(wordcloud)
library(KoNLP)
library(RColorBrewer)

setwd("C:\\R_project")
text <- readLines("mis_document.txt", encoding = "UTF-8") #파일 읽기
buildDictionary(ext_dic = "woorimalsam") # '우리말씀' 한글사전 로딩
pal <- brewer.pal(8, "Dark2") # 팔레트 생성
noun <- sapply(text, extractNoun, USE.NAMES=F) # 명사추출
noun


## 막대 그래프로 표시하기

noun2 <- unlist(noun) # 추출된 명사 통합합
wordcount <- table(noun2) # 단어 빈도수 계산산
temp <- sort(wordcount, decreasing=T)[1:10] # 빈도수 높은 단어 10개 추출
temp
temp <- temp[-1] # 공백 단어 제거거
barplot(temp,
        names.arg = names(temp),       # 막대 이름을 단어로 표시
        col = "lightblue",
        main = "빈도수 높은 단어", ylab = "단어 빈도수")



## 워드클라우드 작성하기
wordcloud(names(wordcount),   # 단어들
          freq=wordcount,     # 단어들의 빈도
          scale=c(6,0.7),     # 단어들 폰트 크기
          min.freq=3,         # 단어들 최소 빈도
          random.order=F,     # 단어들 출력 위치
          rot.per=.1,         # 90도 회전 단어 비율
          colors=pal)         # 단어의 색)

## 워드클라우드 수정하기

# 빈도수 높은데 워드클라우드에 없으면 사용자 사전에 추가
buildDictionary(ext_dic = "woorimalsam",
                user_dic=data.frame("정치", "ncn"),
                replace_usr_dic = T)
noun <- sapply(text,extractNoun, USE.NAMES=F)
noun2 <- unlist(noun)                        # 추출된 명사 통합

# 무의미한 단어 제거
noun2 <- noun2[nchar(noun2)>1]   # 1글자 단어 제거
noun2 <- gsub("하지","", noun2)  # '하지' 제거
noun2 <- gsub("대문","", noun2)  # '때문' 제거

wordcount <- table(noun2)    # 단어 빈도수 계산산
wordcloud(names(wordcount),
          freq=wordcount,
          scale=c(6,0.7),
          min.freq=3,
          random.order=F,
          rot.per=.1,
          colors=pal)
