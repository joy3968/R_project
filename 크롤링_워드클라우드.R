## 크롤링 통해 데이터 가져오기

install.packages('rvest')
library(rvest)

html <- read_html("https://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=134963&target=after&page=1")
html

# html 체계 확인하기
guess_encoding(html)

comment <- html_nodes(html,'.title')%>%
  html_text()
comment <- gsub('\n','',comment)
comment <- gsub('\t','',comment)
comment


## 평점 출력

rate <- html_nodes(html,'.list_netizen_score')%>%
  html_text()

rate
library(stringr)
x <- unlist(str_extract_all(rate,'[[:digit:]]{1,}'))
x

rrate <- c()
j <- 0
for(i in 1:length(x)){
  if(i%%2==0){
    j <- j+1
    rrate[j] <- x[i]
  }
}
rrate

point <- c()
for (i in 1:10){
  point <- c(point, str_extract_all(rate,'[[:digit:]]{1,}')[[i]][2])
}
point


## 글쓴이/ 날짜 (각각)출력

wridate <- html_nodes(html,'.num')%>%
  html_text() 

wridate

wridate1 <- c()
j <- 1
for(i in 1:length(wridate)){
  if(i%%2==0){
    wridate1[j] <- wridate[i]
    j <- j+1
  }
}

wridate1 <- wridate1[1:10]

wridate

wridate1

# 정규표현식을 활영해서 더욱 간단하기 추출 가능
x <- unlist(str_extract_all(wridate,'\\w{1,}\\*{1,}\\d{2}\\.\\d{2}\\.\\d{2}'))
id <- unlist(str_extract_all(wridate,'\\w{1,}\\*{1,}'))
id
date <- unlist(str_extract_all(wridate,'\\d{2}\\.\\d{2}\\.\\d{2}'))
date

## xpath를 활용한 데이터 추출
//*[@id="old_content"]/table/tbody/tr[1]/td[1]/text()

comment <- html_nodes(html,xpath='//*[@id="old_content"]/table/tbody/tr[1]/td[2]/text()')
comment


## 평점 출력 (xpath 활용)

html_nodes(html, xpath='//*[@id="old_content"]/table/tbody/tr/td[2]/div/em')%>%
  html_text()


## 더 간단하게 data.frame 만들기(html_table)
Sys.setlocale("LC_ALL", "English") # English os로 변경(이렇게 하지 않으면 오류 발생)

html <- read_html("https://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=134963&target=after&page=1")
t <- html_nodes(html, 'table')
# table 태그안의 내용 출력

View(html_table(t[[2]]))
# ---> t의 2번 인덱스를 테이블로 만들고 보여주기
review <- html_table(t[[2]])
names(review) <- c('no', 'comment', 'id.date')  
View(review)


## 여러 페이지 데이터 출력

review <- NULL
for(i in 1:3) {
  html <- read_html(paste0("https://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=134963&target=after&page=",i),encoding="CP949")
  t <- html_nodes(html, 'table')
  review <- rbind(review, html_table(t[[2]]))
}
View(review)



## 데이터 정제 작업 & 워드 클라우드

# 1)컬럼 이름 설정
names(review) <- c('no', 'comment', 'id.date')

# 2) \n 과 \t 제거
review$comment <- gsub('\n','', review$comment)
review$comment <- gsub('\t','', review$comment)

# 3) 맨 끝의 두 글자(신고) 삭제
co <- NULL
for(i in review$comment){
  co <- rbind(co,substr(i, 1, nchar(i)-2))
}
co  
review$comment
review$comment <- co


# 4) 맨 앞의 '라라랜드별점 - 총 10점 중' 삭제
review$comment <- gsub('라라랜드별점 - 총 10점 중','',review$comment)

# 5) 맨 앞의 점수 삭제 (10인 경우와 아닌 경우로 나누어서)
comment <- NULL
for(i in review$comment){
  if(substr(i,1,2)=='10'){
    comment <- rbind(comment,substr(i,3,nchar(i)))
  } else {
    comment <- rbind(comment, substr(i,2,nchar(i)))
  }
}

review$comment <- comment

# 6) 특수문자 삭제

text <- review$comment
text

##### 명사뽑기 위한 과정(추가) ####
buildDictionary(ext_dic = "woorimalsam") # '우리말씀' 한글사전 로딩
pal <- brewer.pal(8, "Dark2") # 팔레트 생성
noun <- sapply(text, extractNoun, USE.NAMES=F) # 명사추출
noun
noun2 <- unlist(noun)
noun2

noun2 <- noun2[nchar(noun2)>1]   # 1글자 단어 제거
noun2
#################################



test <- str_replace_all(text,"[[:punct:]]","")

# 7) 단어로 분류

word <- unlist(str_split(test,' '))
word <- word[nchar(word)>1]  # 1글자 이상
word
#df <- data.frame(table(word))
df
df <- data.frame(table(noun2))
df
# 8) 워드클라우드


install.package("wordcloud")
library(wordcloud)
# wordcloud(df$word ,df$Freq,random.order=TRUE, min.freq = 2)
wordcloud(df$noun2 ,df$Freq,random.order=TRUE, min.freq = 2)

