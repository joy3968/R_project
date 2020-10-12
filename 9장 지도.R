library(ggmap)
register_google(key='AIzaSyC31LJo1ssXL32_aWrAJPUeu4YeXm4QGkc') # 구글 키 등록록

gc <- geocode(enc2utf8("오산시")) # 지점의 경도 위도
gc

cen <- as.numeric(gc) # 경도 위도를 숫자로로
cen

map <- get_googlemap(center=cen, zoom=13, size=c(640,640), maptype = "roadmap") # 지도 생성성
ggmap(map)


## 지도 위에 마커와 텍스트 표시
gc <- geocode(enc2utf8('용인'))
cen <- as.numeric(gc)
map <- get_googlemap(center=cen,
                     maptype = 'roadmap',
                     marker=gc) # 마커의 위치치
ggmap(map)


## 지도의 여러 지점에 마커와 텍스트 표시

names <- c("용두암", "성산일출봉", "정방폭포",
           "중문관광단지", "한라산1100고지", "차귀도")
addr <- c("제주시 용두암길 15",
          "서귀포시 성산읍 성산리",
          "서귀포시 동홍동 299-3",
          "서기포시 중문동 2624-1",
          "서귀포시 색달동 산1-2",
          "제주시 한경면 고산리 125")
gc <- geocode(enc2utf8(addr))
gc

# 관광지 명칭과 좌표값으로 데이터프레임 생선
df <- data.frame(name=names,
                 lon=gc$lon,
                 lat=gc$lat)

cen <- c(mean(df$lon),mean(df$lat))
map <- get_googlemap(center = cen,
                    maptype = "roadmap",
                    zoom = 10,
                    size = c(640,640),
                    marker = gc)
ggmap(map)

# 명소 이름 지도 위에 표시하기
gmap <- ggmap(map)
gmap+geom_text(data=df,
               aes(x=lon,y=lat),         # 텍스트 위치
               size=3,                   # 텍스트 크기
               label=df$name)            # 텍스트 내용



## 지도 위에 데아터 표시

# 데이터 준비
sp <- sample(1:nrow(wind),50)
df <- wind[sp,]
head(df)

cen <- c(mean(df$lon), mean(df$lat))     # 지도의 중심점 계산
gc <- data.frame(lon=df$lon, lat=df$lat) # 측정위치 좌표값 데이터
head(gc)

# 측정 위치에 마커 표시하기기
map <- get_googlemap(center=cen,
                     maptype="roadmap",
                     zoom=6,
                     marker=gc)
ggmap(map)

# 풍속을 원의 크기로 표시하기      
map <- get_googlemap(center = cen,               # 마커 없는 지도 가져오기기
                     maptype = "roadmap",
                     zoom = 6)
gmap <- ggmap(map)
gmap+geom_point(data=df,                            # 풍속을 원의 크기로 표시
                aes(x=lon,y=lat,size=spd),
                alpha=0.5,
                col="blue") +
  scale_size_continuous(range = c(1, 14))   # 원의 크기 조절
