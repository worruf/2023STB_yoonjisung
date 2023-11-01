install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)

summary(subway)

#파생변수1.정수형day변수
subway$day<-substr(subway$Date,7,8)
class(subway$day)
subway$day<-as.integer(subway$day)

#파생변수2.line변수
table(subway$Line)

subway$Line<-ifelse(subway$Line=="9호선2~3단계","9호선",subway$Line)

#파생변수3.station변수
table(subway$Station)

#파생변수4.총승하차승객수total_passenger
subway$total_passenger<-subway$on_board+subway$getting_off

str(subway)

#1.지하철역의하루평균승차/하차승객수
subway%>% summarise(on_m=mean(on_board), off_m=mean(getting_off))
max(subway$on_board)
subway%>% filter(on_board==94732)%>%select(Date, Line, Station, on_board)


#하루 평균 이용승객수 10개 역 데이터
passenger10 <-subway %>%group_by(Station)%>%summarise(m=mean(total_passenger))%>%arrange(desc(m))%>%head(10)

head(passenger10, 3)

ggplot(data=passenger10, aes(x=reorder(Station, m), y=m))+geom_col()+coord_flip()

#이용승객 많았던 3날짜 데이터
subway %>%group_by(Date) %>%  summarise(total=sum(total_passenger)) %>%arrange(desc(total)) %>%head(3)

#1호선 이용자 최대 데이터
subway %>%filter(Line=="1호선") %>%filter(total_passenger==max(total_passenger)) %>%select(Date, Station, on_board, getting_off, total_passenger)

#전체승객이 많은 노선
line_pct<-subway %>%group_by(Line) %>% summarise(total=sum(total_passenger)) %>%mutate(all=sum(total), pct=round(total/all*100,2))
line_pct%>%arrange(desc(pct)) %>%head(3)

#노선별 이용비율
line_pct10 <-line_pct%>%filter(Line%in%c("1호선","2호선","3호선","4호선","5호선","6호선","7호선","8호선","9호선","분당선" ))

ggplot(data = line_pct10, aes(x=reorder(Line,pct),y=pct))+geom_col()+coord_flip()+ggtitle("수도권지하철노선별이용비율")+xlab("노선")+ylab("이용비율")

#일별전체승객수
line_graph<-subway %>%group_by(day) %>%summarise(s=sum(total_passenger))
ggplot(data = line_graph, aes(x=day, y=s, group=1))+geom_line()+ggtitle("수도권지하철일별이용승객수")+xlab("일")+ylab("이용승객")



