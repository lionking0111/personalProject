# 강수량에 따른 제주도 관광객들의 소비변화 분석
# 일반적으로 비가 많이 오는 여름에 관광객들은 소비를 얼마나 하며,
# 비가 많이 오는 것에 따라 소비의 변화도 있는가

# data load
c <- read.csv('/Users/lionking/Desktop/BIG DATA/R/rsource/data/jejutravelpattern.csv',
              fileEncoding = 'euc-kr')

f <- read.csv('/Users/lionking/Downloads/rn_20200824105627.csv',
              fileEncoding = 'euc-kr')

e <- read.table('/Users/lionking/Desktop/BIG DATA/R/rsource/data/jejuTravelerCount.txt',
              header = T, sep = ',')
e
# f데이터(강수량) 정제
rain <- f[-c(1:5),]
rain
str(rain)

rainMat <- matrix(rain, ncol = 3, byrow = T)
rainMat
rainfall <- data.frame(rainMat)
rainfall
names(rainfall) <- c('Day', 'Regions', 'Rainfall')
rainfall
rainfall1 <- rainfall[-1,-2]
rainfall1 # 2014 - 2016 강수량(제주도)
str(rainfall1)
rainfall1$Rainfall <- as.numeric(rainfall1$Rainfall)
str(rainfall1)
rainfall1 <- rainfall1[-c(1:8),]
rainfall1 <- rainfall1[-c(25:28),]
str(rainfall1)

# 강수량 data 추가 컬럼(Season / Rainfall.r)
rainfall1$Season[rainfall1$Day == '2015-03'] <- 'Spring'
rainfall1$Season[rainfall1$Day == '2015-04'] <- 'Spring'
rainfall1$Season[rainfall1$Day == '2015-05'] <- 'Spring'
rainfall1$Season[rainfall1$Day == '2016-03'] <- 'Spring'
rainfall1$Season[rainfall1$Day == '2016-04'] <- 'Spring'
rainfall1$Season[rainfall1$Day == '2016-05'] <- 'Spring'

rainfall1$Season[rainfall1$Day == '2015-06'] <- 'Summer'
rainfall1$Season[rainfall1$Day == '2015-07'] <- 'Summer'
rainfall1$Season[rainfall1$Day == '2015-08'] <- 'Summer'
rainfall1$Season[rainfall1$Day == '2016-06'] <- 'Summer'
rainfall1$Season[rainfall1$Day == '2016-07'] <- 'Summer'
rainfall1$Season[rainfall1$Day == '2016-08'] <- 'Summer'

rainfall1$Season[rainfall1$Day == '2014-09'] <- 'Fall'
rainfall1$Season[rainfall1$Day == '2014-10'] <- 'Fall'
rainfall1$Season[rainfall1$Day == '2014-11'] <- 'Fall'
rainfall1$Season[rainfall1$Day == '2015-09'] <- 'Fall'
rainfall1$Season[rainfall1$Day == '2015-10'] <- 'Fall'
rainfall1$Season[rainfall1$Day == '2015-11'] <- 'Fall'

rainfall1$Season[rainfall1$Day == '2014-12'] <- 'Winter'
rainfall1$Season[rainfall1$Day == '2015-01'] <- 'Winter'
rainfall1$Season[rainfall1$Day == '2015-02'] <- 'Winter'
rainfall1$Season[rainfall1$Day == '2015-12'] <- 'Winter'
rainfall1$Season[rainfall1$Day == '2016-01'] <- 'Winter'
rainfall1$Season[rainfall1$Day == '2016-02'] <- 'Winter'

rainfall1

rainfall1$Rainfall.r <- ifelse(rainfall1$Rainfall < 100, 1,
                               ifelse(rainfall1$Rainfall < 150, 2,
                                      ifelse(rainfall1$Rainfall < 200, 3, 4)))
rainfall1

# c데이터 정제
View(c)
c
summary(c)
str(c)
travel <- c[,-11]
travel
str(travel)

# rainfall1 visualization
# install.packages('extrafont')
# install.packages('showtext')
library(RColorBrewer) ; library(dplyr) ; library(ggplot2)
library(extrafont) ; library(showtext)
loadfonts()
barplot(rainfall1$Rainfall, main = '14-16 제주 강수량', ylim = c(0,600), 
        names.arg = rainfall1$Day,
        col = 'lightblue', family = 'Nanum Pen Script')
  


## 1. 성별간 사용금액
filterTravel <- travel[,-c(2,3,4,9,10,11)]
filterTravel
str(filterTravel)
head(filterTravel, 20)

genderCost <- filterTravel %>% 
  group_by(기준년월, 성별) %>%
  summarise(sumCost = sum(카드이용금액)/100000)
  
genderCost
str(genderCost)

par(family = 'AppleGothic')
ggplot(data = genderCost, aes(x = 기준년월, y = sumCost, fill = 성별)) + 
       geom_col(position = 'dodge') +
  theme_minimal(base_family = "Nanum Pen Script") +
  theme(legend.position = "right")

## 2. 연령대/업종별 사용금액
# 20대
par(mfrow = c(2,2))
ageCost1 <- filterTravel %>% 
  group_by(연령대별, 업종명) %>%
  filter(연령대별 == '20대') %>%
  summarise(sumCost = sum(카드이용금액)/100000) %>% 
  mutate(perCost = (sumCost/sum(sumCost))*100)
ageCost1

pie(ageCost1$perCost, labels = paste(ageCost1$업종명,round(ageCost1$perCost,1), '%'),
    main = '업종별/연령대별 사용금액(20대)' )

# 30대
ageCost2 <- filterTravel %>% 
  group_by(연령대별, 업종명) %>%
  filter(연령대별 == '30대') %>%
  summarise(sumCost = sum(카드이용금액)/100000) %>% 
  mutate(perCost = (sumCost/sum(sumCost))*100)
ageCost2

pie(ageCost2$perCost, labels = paste(ageCost2$업종명,round(ageCost2$perCost,1), '%'),
    main = '업종별/연령대별 사용금액(30대)' )

# 40대
ageCost3 <- filterTravel %>% 
  group_by(연령대별, 업종명) %>%
  filter(연령대별 == '40대') %>%
  summarise(sumCost = sum(카드이용금액)/100000) %>% 
  mutate(perCost = (sumCost/sum(sumCost))*100)
ageCost3

pie(ageCost3$perCost, labels = paste(ageCost3$업종명, round(ageCost3$perCost,1), '%'),
    main = '업종별/연령대별 사용금액(40대)' )

# 50대
ageCost4 <- filterTravel %>% 
  group_by(연령대별, 업종명) %>%
  filter(연령대별 == '50대') %>%
  summarise(sumCost = sum(카드이용금액)/100000) %>% 
  mutate(perCost = (sumCost/sum(sumCost))*100)
ageCost4

pie(ageCost4$perCost, labels = paste(ageCost4$업종명,round(ageCost4$perCost,1), '%'),
    main = '업종별/연령대별 사용금액(50대)' )

## 3. 연령대 / 성별간 소비분석
par(mfrow = c(1,2))
ageGenderCostM1 <- filterTravel %>% 
  group_by(연령대별, 성별, 업종명) %>% 
  filter(성별 == '남') %>% 
  summarise(sumCost = sum(카드이용금액)/100000) %>% 
  mutate(perCost = (sumCost/sum(sumCost))*100)

ageGenderCostM1

ggplot(data = ageGenderCostM1, aes(x = 연령대별, y = sumCost, fill = 업종명)) + 
  geom_col(position = 'dodge') +
  labs(title="연령별 사용금액(남)") +
  theme_minimal(base_family = "Nanum Pen Script") +
  theme(legend.position = "right")

ageGenderCostW1 <- filterTravel %>% 
  group_by(연령대별, 성별, 업종명) %>% 
  filter(성별 == '여') %>% 
  summarise(sumCost = sum(카드이용금액)/100000) %>% 
  mutate(perCost = (sumCost/sum(sumCost))*100)

ageGenderCostW1

ggplot(data = ageGenderCostW1, aes(x = 연령대별, y = sumCost, fill = 업종명)) + 
  geom_col(position = 'dodge') +
  labs(title="연령별 사용금액(여)") +
  theme_minimal(base_family = "Nanum Pen Script") +
  theme(legend.position = "right")

# 4. 기간별 사용금액
dayCost <- filterTravel %>% 
  group_by(기준년월) %>%
  summarise(sumCost = sum(카드이용금액)/1000000)
dayCost  

barplot(dayCost$sumCost, main = '14-16 제주 소비금액/10000', ylim = c(0,3000), 
        names.arg = dayCost$기준년월,
        col = 'pink', family = 'Nanum Pen Script')

### 강수량과 관광객 소비금액에 대한 상관관계
### 강수량과 관광객 입도현황의 상관관계

# data load
str(e)

jeju <- data.frame(e,rainfall1)
jeju

jeju1 <- jeju[,-3]
jeju1
jeju1[c(16,19,23),]

# Season factor
table(jeju1$Season)
jeju1$Season <- as.factor(jeju1$Season)
str(jeju1)

barplot(jeju1$Traveler/1000, main = '14-16 제주 관광객 수(/1000)', ylim = c(1,1300),
        names.arg = jeju1$Day, col = 'lightgreen', family = 'Nanum Pen Script')

# 1) 강수량에 따른 여행객 변화
jeju.lm <- lm(Traveler ~ Rainfall , data = jeju1)
jeju.lm

# 선형회귀분석 모델 시각화[오류 확인]
# 산점도 그리기
plot(formula = Traveler ~ Rainfall , data = jeju1)
# 회귀선
abline(jeju.lm, col = 'red')

# 선형회귀분석 결과 보기
summary(jeju.lm)
str(summary(jeju.lm))
plot(summary(jeju.lm)$residuals)

# 계절, 강수량 범주 컬럼 추가한 후 검정실시
jeju.lm1 <- lm(Traveler ~ Season + Rainfall.r, data = jeju1)
jeju.lm1
summary(jeju.lm1)

library(lmtest)
dwtest(jeju.lm1)

par(mfrow = c(2,2))
# 선형성(빨간 실선 이 0에 가까운 수평선) & 독립성(특정한 모여있는 패턴이 발견되지 않음)
plot(jeju.lm1, which = 1)
# 정규성(직성에 가깝게 잘 모여있음)
plot(jeju.lm1, which = 2)
# 등분산성 & 독립성(적절하게 퍼져있음, 특정 패턴 없음)
plot(jeju.lm1, which = 3)
# 극단치
plot(jeju.lm1, which = 4)

#강수량과 계절
jeju.lm2 <- lm(Rainfall ~ Season, data = jeju1)
jeju.lm2
summary(jeju.lm2)

plot(formula = Rainfall ~ Season, data = jeju1)

#계절에 따른 관광객 변화
jeju.lm3 <- lm(Traveler ~ Season, data = jeju1)
jeju.lm3
summary(jeju.lm3)

plot(formula = Traveler ~ Season, data = jeju1)







