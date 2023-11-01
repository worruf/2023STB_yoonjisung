library(dplyr)
library(ggplot2)

str(congestion)  

#변수의이상치와결측치확인하고처리
summary(congestion)

#결측치개수확인
is.na(congestion)
sum(is.na(congestion))
colSums(is.na(congestion))

#결측치가있는행을제거한새로운데이터프레임생성

congestion1 <-congestion[!is.na(congestion$s0600),]
congestion1 <-congestion[!is.na(congestion$s2330),]

colSums(is.na(congestion1))

#남은결측치를0으로대체
congestion1[is.na(congestion1)] <-0
colSums(is.na(congestion1))

#이상치확인
ggplot(congestion1, aes(y=s0530))+  geom_boxplot()
summary(congestion1$s0530)

#1.지하철역의하루평균혼잡도
congestion1$day_mean <-rowMeans(congestion1[,c('s0530','s0600','s0630','s0700','s0730','s0800','s0830','s0900','s0930','s1000','s1030','s1100','s1130','s1200','s1230','s1300','s1330','s1400','s1430','s1500','s1530','s1600','s1630','s1700','s1730','s1800','s1830','s1900','s1930','s2000','s2030','s2100','s2130','s2200','s2230','s2300','s2330')])
mean(congestion1$day_mean)

#지하철호선별하루평균혼잡도
a <-congestion1 %>%group_by(line)%>%summarise(m=mean(day_mean))%>%arrange(desc(m))%>%head(10)
print(a)

#지하철호선별출근시간(07:00~09:00)대의평균혼잡도

#기술통계분석결과를포함합니다.
gotowork<-congestion1%>%group_by(line)%>%summarize('7시'=mean(s0700),'7시반'=mean(s0730),'8시'=mean(s0800),'8시 반'=mean(s0830),'9시'=mean(s0900))
summary(gotowork) 

#평균혼잡도가가장높은시간대를막대그래프로그리기
busytime<-gotowork[,c(1,4)]
barplot(height = busytime$'8시',main="8시평균혼잡도",names.arg = busytime$line,xlab = "호선",ylab = "혼잡도",col = rainbow(8),cex.lab=1.5,cex.main=2)

#평균혼잡도상위4개호선의역별기여도

morning<-congestion1[c(2,3,8,9,10,11,12)]

morning$mean1 <-rowMeans(morning[,c('s0700','s0730','s0800','s0830','s0900')])

line_pct1<-morning %>%group_by(line) %>% summarise(total=sum(mean1)) %>%mutate(all=sum(total), pct=round(total/all*100,2))
line_pct1%>%arrange(desc(pct)) %>%head(4)

#출발시간8시의지하철혼잡도범주화/범주별빈도분석
#▲good(80% 이하) ▲normal(80~130%) ▲caution(130~150%) ▲bad(150% 이상)
# 출발시간8시, 호선별“caution”의빈도와caution이전체등급에서차지하는비율계산

congestion1 %>%  mutate(s80_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution", "bad"))))%>%  group_by(s80_grade) %>% summarise(n=n())%>%  mutate(total=sum(n), pct=round(n/total*100,1))%>%  select(s80_grade,n,pct)%>%  arrange(desc(n))

#3-1. 호선별로08시지하철혼잡도범주화
congestion1 %>%  mutate(s80_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution", "bad"))))%>%  group_by(line, s80_grade) %>% summarise(n=n())%>%  mutate(total=sum(n), pct=round(n/total*100,1))%>%  filter(s80_grade=="caution")%>%  select(line, s80_grade,n,pct)%>%  arrange(desc(pct))%>% head(5)

#지하철호선별퇴근시간(18:00~20:00)대의평균혼잡도
#기술통계분석결과
gotowork1<-congestion1%>%group_by(line)%>%summarize('6시'=mean(s1800),'6시반'=mean(s1830),'7시'=mean(s1900),'7시 반'=mean(s1930),'8시'=mean(s2000))
print(gotowork1)
summary(gotowork1)

#평균혼잡도가가장높은시간대를막대그래프로그리기
busytime1<-gotowork1[,c(1,2)]
barplot(height = busytime1$'6시',main="6시평균혼잡도",names.arg = busytime$line,xlab = "호선",ylab = "혼잡도",col = rainbow(8),cex.lab=1.5,cex.main=2)

#평균혼잡도상위4개호선의역별기여도

afternoon<-congestion1[c(2,3,30,31,32,33,34)]

afternoon$mean1 <-rowMeans(afternoon[,c('s1800','s1830','s1900','s1930','s2000')])

line_pct2<-afternoon %>%group_by(line) %>% summarise(total=sum(mean1)) %>%mutate(all=sum(total), pct=round(total/all*100,2))
line_pct2%>%arrange(desc(pct)) %>%head(4)

#출발시간18시의지하철혼잡도범주화/범주별빈도분석
#▲good(80% 이하) ▲normal(80~130%) ▲caution(130~150%) ▲bad(150% 이상)

congestion1 %>%  mutate(s180_grade=ifelse(s1800<=80, "good", ifelse(s1800<=130, "normal", ifelse(s1800<=150, "caution", "bad"))))%>%  group_by(s180_grade) %>% summarise(n=n())%>%  mutate(total=sum(n), pct=round(n/total*100,1))%>%  select(s180_grade,n,pct)%>%  arrange(desc(n))

#출발시간8시, 호선별“bad”의빈도와bad가전체등급에서차지하는비율계산
congestion1 %>%  mutate(s180_grade=ifelse(s1800<=80, "good", ifelse(s1800<=130, "normal", ifelse(s1800<=150, "caution", "bad"))))%>%  group_by(line, s180_grade) %>% summarise(n=n())%>%  mutate(total=sum(n), pct=round(n/total*100,1))%>%  filter(s180_grade=="bad")%>%  select(line, s180_grade,n,pct)%>%  arrange(desc(pct))%>% head(5)
