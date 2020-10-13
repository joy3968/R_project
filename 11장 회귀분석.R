## 01.단순 회귀분석
head(cars)
# 종속변수 ~ 독립변수
plot(dist~speed, data=cars)

model <- lm(dist~speed, cars) # 회귀모델 구하기기
model    # (Intercept) Y=wX + b 에서 b의 값(즉 y절편), 그 오른쪽은 기울기

# 회귀선을 산점도에 표시하기
abline(model)
coef(model)[1]   # b값 출력 -> Y절편
coef(model)[2]   # W값 출력 -> 기울기



# 주행속도에 따른 제동거리 구하기
b <- coef(model)[1]
W <- coef(model)[2]

speed <- 30             # 주행속도가 30일 때
dist <- W*speed + b     # 제동거리 구하기
dist                    # speed가 30일때 제동거리


# speed가 35일때 제동거리 예측
speed <- 35
dist <- W*speed + b
dist



# 예상 제동거리, 실제 제동거리, 오차구하기
speed <- cars[,1]        # 주행속도
pred <- W * speed + b
pred                     # 속도에 따른 예상 제동거리
                     
compare <- data.frame(pred, cars[,2], pred-cars[,2]) #예측, 실제, 오차
colnames(compare) <- c('예상', '실제', '오차')
head(compare)
compare


## 02.다중선형 회귀분석
library(car)
head(Prestige)
newdata <- Prestige[,c(1:4)]    # 회귀식 작성을 위한 데이터 준비비
plot(newdata, pch=16, col="blue",       # 산점도를  통해 변수 간 관계 확인
     main = 'Matirx Scatterplot')
# education : 교육연수
# income : 연봉
# women : 여성의 비율
# prestige : 직군에 대한 평판도
mod1 <- lm(income ~ education + prestige +
             women, data = newdata)

summary(mod1)
# 변수들의 가장 끝에 *** 은 종속변수를 설명하는 데 얼마나 중요한 변수인지를 나타낸다.
# p-value -> 구한 모델이 의미있는 모델인지를 나타낸다. p-value < 0.05 이면 모델의 신뢰수준이 95% 이상임을 나타냄
# Adjusted R-squared -> 모델의 설명력을 나타냄. 0.632 라는 것은 63%정도 설명할 수 있다는 것.


# stepAIC() 함수는 변수들을 선별할 수 있는 함수.
library(MASS)
newdata2 <- Prestige[,c(1:5)]
head(newdata2)
mod2 <- lm(income ~ education + prestige +
             women + census, data = newdata2)
mod3 <- stepAIC(mod2)           # 변수 선택 진행
mod3                            # 변수 선택 후 결과 확인인
summary(mod3)                   # 회귀모델 상세 내용 확인인
