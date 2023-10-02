# table()함수/구분 1개의 인자를 가지고 도수분포표 작성
table(KOTRA2023 $진출대륙명)

# table()함수/2개의 인자를 가지고 교차표를 작성
table(KOTRA2023 $진출대륙명, KOTRA2023 $진출형태)

#  상대도수 계산
ECN <- table(KOTRA2023 $진출대륙명)
prop.table(ECN)

#막대그래프
barplot(table(KOTRA2023 $진출대륙명))

entry <- table(KOTRA2023 $진출대륙명, KOTRA2023 $진출형태)
barplot(entry, legend = TRUE)

#파이차트
pie(table(KOTRA2023 $진출대륙명))
pie(table(KOTRA2023 $투자형태))

#실습
barplot(entry,main = "해외기업의 진출현황",col = c(2,3,4,5,6,7,8),xlab = '대륙별매출',xlim = c(0,5000),horiz = TRUE,ylab = "진출기업",legend=rownames(entry))
abline(v=0)

pie(table(KOTRA2023$투자형태),las=0.2,main = "해외기업 투자 형태",col = BrBG)
 
