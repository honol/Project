#데이터 불러오기
setwd('C:/Users/User/Desktop/탐색적자료분석')
data = read.csv('SkillCraft1_Dataset.csv', header = T, stringsAsFactors = F)

#데이터 타입 변환(결측존재)
data$LeagueIndex = ordered(data$LeagueIndex)
data$Age = as.numeric(data$Age)
data$HoursPerWeek = as.numeric(data$HoursPerWeek)
data$TotalHours = as.numeric(data$TotalHours)

#############결측값에 따른 더미변수 추가###############
install.packages('vcd')


par(mfrow = c(3, 1))
data$Age_NA = 0 #Age_NA라는 변수를 생성하여 0 값 입력
data[is.na(data$Age),]$Age_NA = 1
my.table = xtabs(~ data$Age_NA + data$LeagueIndex)
barplot(my.table, main = 'Age',xlab = "LeagueIndex", ylab = "Percentile", legend.text = TRUE, col = c('lightblue', 'blue'))


data$HoursPerWeek_NA = 0 #Age_NA라는 변수를 생성하여 0 값 입력
data[is.na(data$HoursPerWeek),]$HoursPerWeek_NA = 1
my.table = xtabs(~ data$HoursPerWeek_NA + data$LeagueIndex)
barplot(my.table,main = 'HoursPerWeek', xlab = "LeagueIndex", ylab = "Percentile", legend.text = TRUE, col = c('lightblue', 'blue'))


data$TotalHours_NA = 0 #Age_NA라는 변수를 생성하여 0 값 입력
data[is.na(data$TotalHours),]$TotalHours_NA = 1
my.table = xtabs(~ data$TotalHours_NA + data$LeagueIndex)
barplot(my.table, main = 'TotalHours', xlab = "LeagueIndex", ylab = "Percentile", legend.text = TRUE, col = c('lightblue', 'blue'))


#par
par(bg = 'white',col = 'black', col.main = 'black', col.axis = 'slategray4', col.lab = 'black', cex = 1.5)

################3등급별 연령 분포######################
#상자그림
par(mfrow = c(1,1))
Age_box = boxplot(data$Age, main = "Age", col = 'lightblue', horizontal = T)
text(Age_box$out, rep(1, NROW(Age_box$out)), labels = Age_box$out, pos = c(1,3))

#밀도함수 그래프
plot(density(data$Age[LeagueIndex==1]), xlab="Age",lwd=2, main='LeagueIndex', xlim = c(0, 45), ylim = c(0, 0.15))
lines(density(data$Age[LeagueIndex==2]), col = "red", lty = 2, lwd=2)
lines(density(data$Age[LeagueIndex==3]), col = "blue", lty = 3,lwd=2)      
lines(density(data$Age[LeagueIndex==4]), col = "green", lty = 4,lwd=2)
lines(density(data$Age[LeagueIndex==5]), col = "orange", lty = 5,lwd=2)   
lines(density(data$Age[LeagueIndex==6]), col = "brown", lty = 6,lwd=2)
lines(density(data$Age[LeagueIndex==7]), col = "maroon1", lty = 7,lwd=2)   
lines(density(data$Age[LeagueIndex==8]), col = "pink2", lty = 8,lwd=2)   
legend("topright", c('Bronze','Silver','Gold','Platinum','Diamond','Master','GrandMaster', 'Professional'),
       col = c("black","red", "blue",'green','orange','brown','maroon1', 'pink2'),
       lty = c(1,2,3,4,5,6,7,8), lwd = 1, bty ="n", cex = 1.5)

#높은 수준의 등급과 낮은 수준의 등급과의 나이 차이 비교
nrow(data[!is.na(data$Age),]) #Age에 결측값을 제거한 row의 숫자
LeagueIndex_cat = rep(0, 3340) #Age의 결측값을 제거한 숫자만큼 0값 생성
LeagueIndex_cat[data$LeagueIndex[data$LeagueIndex !=8 ] == 7] = 1 #그 중, 해당 등급에 해당하는 값은 1로 변경
LeagueIndex_cat[data$LeagueIndex[data$LeagueIndex !=8 ] == 6] = 1
LeagueIndex_cat = as.factor(LeagueIndex_cat)
Age = data$Age[!is.na(data$Age)]
LeagueIndex_data = data.frame(LeagueIndex_cat, Age) 

library(lawstat)
leven_test = levene.test(LeagueIndex_data$Age, LeagueIndex_data$LeagueIndex_cat)
leven_test
t.test(LeagueIndex_data$Age ~ LeagueIndex_data$LeagueIndex_cat, paired = F, var.equal = F)



#계급의 분포는 어떻게 되는가?
colors()
LeagueIndex_table = (table(data$LeagueIndex)/ 3395) * 100
LeagueIndex_table
names(LeagueIndex_table) = c("Bronze", "Silver", "Gold", "Platinum", "Diamond", "Master", "GrandMaster", "Professional leagues")
par(bg = 'white',col = 'black', col.main = 'black', col.axis = 'slategray4', col.lab = 'black', cex = 1.5)

#barplot
library(RColorBrewer)
LeagueIndex_table
LeagueIndex_bar = barplot(LeagueIndex_table, xlab = "LeagueIndex", ylab = "Percentile", 
                          col=brewer.pal(9,'Greens'), ylim = c(0, 30))
text(LeagueIndex_bar,LeagueIndex_table+1.5, labels = paste(round(LeagueIndex_table, 2),"%", sep=''))

#나이대는 어떻게 되는가?
length(which(data$Age >= 45)) #45세 이상이 없으므로 44세까지 5세 단위로 범주화
data = transform(data, Age_cat = cut(Age, breaks = c(15, 20, 25, 30, 35, 40, 45),right = F, 
                                 labels = c("15~19", "20~24", "25~29", "30~34", "35~39", "40~44")))
data$Age_cat #5세 단위로 범주화
Age_cat_table = (table(data$Age_cat)/3395) * 100
Age_table_bar = barplot(Age_cat_table, xlab = "Age", ylab = "Percentile", col=brewer.pal(5,'Blues'), ylim = c(0, 50))
text(Age_table_bar,Age_cat_table,labels = paste(round(Age_cat_table, 2),"%", sep=''), adj = c(0.5,-0.5))


################급에 따른 이용시간###################
#상자그림
par(mfrow = c(1,1))
HoursPerWeek_box = boxplot(data$HoursPerWeek, main = "HoursPerWeek", col = 'lightblue', horizontal = T)
text(HoursPerWeek_box$out, rep(1, NROW(HoursPerWeek_box$out)), labels = HoursPerWeek_box$out, pos = c(1,3))

#밀도함수 그래프
plot(density(data$HoursPerWeek[data$LeagueIndex==1]), xlab="HoursPerWeek",lwd=2, main='LeagueIndex', ylim =c(0, 0.08))
lines(density(data$HoursPerWeek[data$LeagueIndex==2]), col = "red", lty = 2, lwd=2)
lines(density(data$HoursPerWeek[data$LeagueIndex==3]), col = "blue", lty = 3,lwd=2)      
lines(density(data$HoursPerWeek[data$LeagueIndex==4]), col = "green", lty = 4,lwd=2)
data_na = data[!is.na(data$HoursPerWeek),]
lines(density(data_na$HoursPerWeek[data_na$LeagueIndex==5]), col = "orange", lty = 5,lwd=2)   
lines(density(data$HoursPerWeek[data$LeagueIndex==6]), col = "brown", lty = 6,lwd=2)
lines(density(data$HoursPerWeek[data$LeagueIndex==7]), col = "maroon1", lty = 7,lwd=2)   
lines(density(data$HoursPerWeek[data$LeagueIndex==8]), col = "pink2", lty = 8,lwd=2)   
legend("topright", c('Bronze','Silver','Gold','Platinum','Diamond','Master','GrandMaster', 'Professional'),
       col = c("black","red", "blue",'green','orange','brown','maroon1', 'pink2'),
       lty = c(1,2,3,4,5,6,7,8), lwd = 1, bty ="n", cex = 1.5)

#높은 수준의 등급과 낮은 수준의 등급과의 나이 차이 비교
nrow(data[!is.na(data$HoursPerWeek),]) #Age에 결측값을 제거한 row의 숫자
LeagueIndex_cat = rep(0, 3339) #Age의 결측값을 제거한 숫자만큼 0값 생성
LeagueIndex_cat[data$LeagueIndex[data$LeagueIndex !=8 ] == 7] = 1 #그 중, 해당 등급에 해당하는 값은 1로 변경
LeagueIndex_cat[data$LeagueIndex[data$LeagueIndex !=8 ] == 6] = 1
LeagueIndex_cat = as.factor(LeagueIndex_cat)
HoursPerWeek = data$HoursPerWeek[!is.na(data$Age)]
LeagueIndex_data = data.frame(LeagueIndex_cat, HoursPerWeek) 

library(lawstat)
leven_test = levene.test(LeagueIndex_data$HoursPerWeek, LeagueIndex_data$LeagueIndex_cat)
leven_test
t.test(LeagueIndex_data$HoursPerWeek ~ LeagueIndex_data$LeagueIndex_cat, paired = F, var.equal = F)

##########################TotalHours###############
#상자그림
par(mfrow = c(1,1))
TotalHours_box = boxplot(data$TotalHours, main = "TotalHours", col = 'lightblue', horizontal = T, ylim = c(0, 2000))
text(TotalHours_box$out, rep(1, NROW(TotalHours_box$out)), labels = TotalHours_box$out, pos = c(1,3), cex = 0.8)

#밀도함수 그래프
plot(density(data$TotalHours[data$LeagueIndex==1]), xlab="TotalHours",lwd=2, main='LeagueIndex', ylim =c(0, 0.004))
lines(density(data$TotalHours[data$LeagueIndex==2]), col = "red", lty = 2, lwd=2)
lines(density(data$TotalHours[data$LeagueIndex==3]), col = "blue", lty = 3,lwd=2)      
lines(density(data$TotalHours[data$LeagueIndex==4]), col = "green", lty = 4,lwd=2)
data_na = data[!is.na(data$TotalHours),]
lines(density(data_na$TotalHours[data$LeagueIndex==5]), col = "orange", lty = 5,lwd=2)   
lines(density(data$TotalHours[data$LeagueIndex==6]), col = "brown", lty = 6,lwd=2)
lines(density(data$TotalHours[data$LeagueIndex==7]), col = "maroon1", lty = 7,lwd=2)   
lines(density(data$TotalHours[data$LeagueIndex==8]), col = "pink2", lty = 8,lwd=2)   
legend("topright", c('Bronze','Silver','Gold','Platinum','Diamond','Master','GrandMaster', 'Professional'),
       col = c("black","red", "blue",'green','orange','brown','maroon1', 'pink2'),
       lty = c(1,2,3,4,5,6,7,8), lwd = 1, bty ="n", cex = 1.5)

#높은 수준의 등급과 낮은 수준의 등급과의 나이 차이 비교
nrow(data[!is.na(data$TotalHours),]) #Age에 결측값을 제거한 row의 숫자
LeagueIndex_cat = rep(0, 3338) #Age의 결측값을 제거한 숫자만큼 0값 생성
LeagueIndex_cat[data$LeagueIndex[data$LeagueIndex !=8 & data$LeagueIndex !=5] == 7] = 1 #그 중, 해당 등급에 해당하는 값은 1로 변경
LeagueIndex_cat[data$LeagueIndex[data$LeagueIndex !=8 & data$LeagueIndex !=5] == 6] = 1
LeagueIndex_cat = as.factor(LeagueIndex_cat)
TotalHours = data$TotalHours[!is.na(data$TotalHours)]
LeagueIndex_data = data.frame(LeagueIndex_cat, TotalHours) 

library(lawstat)
leven_test = levene.test(LeagueIndex_data$TotalHours, LeagueIndex_data$LeagueIndex_cat)
leven_test
t.test(LeagueIndex_data$TotalHours ~ LeagueIndex_data$LeagueIndex_cat, paired = F, var.equal = T)


##########################APM###############
#상자그림
par(mfrow = c(1,1))
APM_box = boxplot(data$APM, main = "APM", col = 'lightblue', horizontal = T, ylim = c(0,380))
text(APM_box$out, rep(1, NROW(APM_box$out)), labels = APM_box$out, pos = c(1,3), cex = 0.8)

#밀도함수 그래프
plot(density(data$APM[data$LeagueIndex==1]), xlab="APM",lwd=2, main='LeagueIndex', ylim =c(0, 0.025), xlim = c(0, 400))
lines(density(data$APM[data$LeagueIndex==2]), col = "red", lty = 2, lwd=2)
lines(density(data$APM[data$LeagueIndex==3]), col = "blue", lty = 3,lwd=2)      
lines(density(data$APM[data$LeagueIndex==4]), col = "green", lty = 4,lwd=2)
data_na = data[!is.na(data$TotalHours),]

lines(density(data$APM[data$LeagueIndex==5]), col = "orange", lty = 5,lwd=2)   
lines(density(data$APM[data$LeagueIndex==6]), col = "brown", lty = 6,lwd=2)
lines(density(data$APM[data$LeagueIndex==7]), col = "maroon1", lty = 7,lwd=2)   
lines(density(data$APM[data$LeagueIndex==8]), col = "pink4", lty = 8,lwd=2)   
legend("topright", c('Bronze','Silver','Gold','Platinum','Diamond','Master','GrandMaster', 'Professional'),
       col = c("black","red", "blue",'green','orange','brown','maroon1', 'pink4'),
       lty = c(1,2,3,4,5,6,7,8), lwd = 1, bty ="n", cex = 1.5)

#높은 수준의 등급과 낮은 수준의 등급과의 나이 차이 비교
nrow(data[!is.na(data$APM),]) #Age에 결측값을 제거한 row의 숫자
LeagueIndex_cat = rep(0, 3395) #Age의 결측값을 제거한 숫자만큼 0값 생성
LeagueIndex_cat[data$LeagueIndex == 6] = 1
LeagueIndex_cat[data$LeagueIndex == 5] = 1

LeagueIndex_cat = as.factor(LeagueIndex_cat)
APM = data$APM[!is.na(data$APM)]
LeagueIndex_data = data.frame(LeagueIndex_cat, APM) 

plot(density(LeagueIndex_data$APM[LeagueIndex_data$LeagueIndex_cat==1]), xlab="APM",lwd=2, main='LeagueIndex', ylim =c(0, 0.025), xlim = c(0, 400))
lines(density(LeagueIndex_data$APM[LeagueIndex_data$LeagueIndex_cat==0]), col = "red", lty = 2, lwd=2)
legend("topright", c('High', 'Low'),
       col = c("black","red"),
       lty = c(1,2), lwd = 1, bty ="n", cex = 1.5)


library(lawstat)
leven_test = levene.test(LeagueIndex_data$APM, LeagueIndex_data$LeagueIndex_cat)
leven_test
t.test(LeagueIndex_data$APM ~ LeagueIndex_data$LeagueIndex_cat, paired = F, var.equal = F)


##########################SelectByHotkeys###############
#상자그림
par(mfrow = c(1,1))
SelectByHotkeys_box = boxplot(data$SelectByHotkeys, main = "SelectByHotkeys",
                              col = 'lightblue', horizontal = T, ylim = c(0,0.04))
text(SelectByHotkeys_box$out, rep(1, NROW(SelectByHotkeys_box$out)), labels = SelectByHotkeys_box$out, pos = c(1,3), cex = 0.8)

#밀도함수 그래프
plot(density(data$SelectByHotkeys[data$LeagueIndex==1]), xlab="SelectByHotkeys",
     lwd=2, main='LeagueIndex', ylim =c(0, 650), xlim = c(0, 0.015))
lines(density(data$SelectByHotkeys[data$LeagueIndex==2]), col = "red", lty = 2, lwd=2)
lines(density(data$SelectByHotkeys[data$LeagueIndex==3]), col = "blue", lty = 3,lwd=2)      
lines(density(data$SelectByHotkeys[data$LeagueIndex==4]), col = "green", lty = 4,lwd=2)
data_na = data[!is.na(data$SelectByHotkeys),]
lines(density(data$SelectByHotkeys[data$LeagueIndex==5]), col = "orange", lty = 5,lwd=2)   
lines(density(data$SelectByHotkeys[data$LeagueIndex==6]), col = "brown", lty = 6,lwd=2)
lines(density(data$SelectByHotkeys[data$LeagueIndex==7]), col = "maroon1", lty = 7,lwd=2)   
lines(density(data$SelectByHotkeys[data$LeagueIndex==8]), col = "pink4", lty = 8,lwd=2)   
legend("topright", c('Bronze','Silver','Gold','Platinum','Diamond','Master','GrandMaster', 'Professional'),
       col = c("black","red", "blue",'green','orange','brown','maroon1', 'pink4'),
       lty = c(1,2,3,4,5,6,7,8), lwd = 1, bty ="n", cex = 1.5)

#높은 수준의 등급과 낮은 수준의 등급과의 나이 차이 비교
nrow(data[!is.na(data$APM),]) #Age에 결측값을 제거한 row의 숫자
LeagueIndex_cat = rep(0, 3395) #Age의 결측값을 제거한 숫자만큼 0값 생성
LeagueIndex_cat[data$LeagueIndex == 6] = 1
LeagueIndex_cat[data$LeagueIndex == 5] = 1

LeagueIndex_cat = as.factor(LeagueIndex_cat)
APM = data$APM[!is.na(data$APM)]
LeagueIndex_data = data.frame(LeagueIndex_cat, APM) 

plot(density(LeagueIndex_data$APM[LeagueIndex_data$LeagueIndex_cat==1]), xlab="APM",lwd=2, main='LeagueIndex', ylim =c(0, 0.025), xlim = c(0, 400))
lines(density(LeagueIndex_data$APM[LeagueIndex_data$LeagueIndex_cat==0]), col = "red", lty = 2, lwd=2)
legend("topright", c('High', 'Low'),
       col = c("black","red"),
       lty = c(1,2), lwd = 1, bty ="n", cex = 1.5)


library(lawstat)
leven_test = levene.test(LeagueIndex_data$APM, LeagueIndex_data$LeagueIndex_cat)
leven_test
t.test(LeagueIndex_data$APM ~ LeagueIndex_data$LeagueIndex_cat, paired = F, var.equal = T)


##########################AssignToHotkeys###############
#상자그림
par(mfrow = c(1,1))
AssignToHotkeys_box = boxplot(data$AssignToHotkeys, main = "AssignToHotkeys",
                              col = 'lightblue', horizontal = T, ylim = c(0,0.0015))

#밀도함수 그래프
plot(density(data$AssignToHotkeys[data$LeagueIndex==1]), xlab="AssignToHotkeys",
     lwd=2, main='LeagueIndex', xlim = c(0,0.002))
lines(density(data$AssignToHotkeys[data$LeagueIndex==2]), col = "red", lty = 2, lwd=2)
lines(density(data$AssignToHotkeys[data$LeagueIndex==3]), col = "blue", lty = 3,lwd=2)      
lines(density(data$AssignToHotkeys[data$LeagueIndex==4]), col = "green", lty = 4,lwd=2)
data_na = data[!is.na(data$SelectByHotkeys),]
lines(density(data$AssignToHotkeys[data$LeagueIndex==5]), col = "orange", lty = 5,lwd=2)   
lines(density(data$AssignToHotkeys[data$LeagueIndex==6]), col = "brown", lty = 6,lwd=2)
lines(density(data$AssignToHotkeys[data$LeagueIndex==7]), col = "maroon1", lty = 7,lwd=2)   
lines(density(data$AssignToHotkeys[data$LeagueIndex==8]), col = "pink4", lty = 8,lwd=2)   
legend("topright", c('Bronze','Silver','Gold','Platinum','Diamond','Master','GrandMaster', 'Professional'),
       col = c("black","red", "blue",'green','orange','brown','maroon1', 'pink4'),
       lty = c(1,2,3,4,5,6,7,8), lwd = 1, bty ="n", cex = 1.5)

#높은 수준의 등급과 낮은 수준의 등급과의 나이 차이 비교
nrow(data[!is.na(data$AssignToHotkeys),]) #Age에 결측값을 제거한 row의 숫자
LeagueIndex_cat = rep(0, 3395) #Age의 결측값을 제거한 숫자만큼 0값 생성
LeagueIndex_cat[data$LeagueIndex == 6] = 1
LeagueIndex_cat[data$LeagueIndex == 5] = 1
LeagueIndex_cat = as.factor(LeagueIndex_cat)
AssignToHotkeys = data$AssignToHotkeys[!is.na(data$AssignToHotkeys)]
LeagueIndex_data = data.frame(LeagueIndex_cat, AssignToHotkeys) 

plot(density(LeagueIndex_data$AssignToHotkeys[LeagueIndex_data$LeagueIndex_cat==1]), xlab="AssignToHotkeys",
     lwd=2, main='LeagueIndex')
lines(density(LeagueIndex_data$AssignToHotkeys[LeagueIndex_data$LeagueIndex_cat==0]), col = "red", lty = 2, lwd=2)
legend("topright", c('High', 'Low'),
       col = c("black","red"),
       lty = c(1,2), lwd = 1, bty ="n", cex = 1.5)


library(lawstat)
leven_test = levene.test(LeagueIndex_data$AssignToHotkeys, LeagueIndex_data$LeagueIndex_cat)
leven_test
t.test(LeagueIndex_data$AssignToHotkeys ~ LeagueIndex_data$LeagueIndex_cat, paired = F, var.equal = T)

##########################UniqueHotkeys###############
#상자그림
par(mfrow = c(1,1))
UniqueHotkeys_box = boxplot(data$UniqueHotkeys, main = "UniqueHotkeys",
                              col = 'lightblue', horizontal = T, ylim = c(0,15))

#밀도함수 그래프
plot(density(data$UniqueHotkeys[data$LeagueIndex==1]), xlab="UniqueHotkeys",
     lwd=2, main='LeagueIndex', ylim = c(0, 0.3), xlim = c(0, 18))
lines(density(data$UniqueHotkeys[data$LeagueIndex==2]), col = "red", lty = 2, lwd=2)
lines(density(data$UniqueHotkeys[data$LeagueIndex==3]), col = "blue", lty = 3,lwd=2)      
lines(density(data$UniqueHotkeys[data$LeagueIndex==4]), col = "green", lty = 4,lwd=2)
lines(density(data$UniqueHotkeys[data$LeagueIndex==5]), col = "orange", lty = 5,lwd=2)   
lines(density(data$UniqueHotkeys[data$LeagueIndex==6]), col = "brown", lty = 6,lwd=2)
lines(density(data$UniqueHotkeys[data$LeagueIndex==7]), col = "maroon1", lty = 7,lwd=2)   
lines(density(data$UniqueHotkeys[data$LeagueIndex==8]), col = "pink4", lty = 8,lwd=2)   
legend("topright", c('Bronze','Silver','Gold','Platinum','Diamond','Master','GrandMaster', 'Professional'),
       col = c("black","red", "blue",'green','orange','brown','maroon1', 'pink4'),
       lty = c(1,2,3,4,5,6,7,8), lwd = 1, bty ="n", cex = 1.5)

#높은 수준의 등급과 낮은 수준의 등급과의 나이 차이 비교
nrow(data[!is.na(data$AssignToHotkeys),]) #Age에 결측값을 제거한 row의 숫자
LeagueIndex_cat = rep(0, 3395) #Age의 결측값을 제거한 숫자만큼 0값 생성
LeagueIndex_cat[data$LeagueIndex == 6] = 1
LeagueIndex_cat[data$LeagueIndex == 5] = 1

LeagueIndex_cat = as.factor(LeagueIndex_cat)
AssignToHotkeys = data$AssignToHotkeys[!is.na(data$AssignToHotkeys)]
LeagueIndex_data = data.frame(LeagueIndex_cat, AssignToHotkeys) 

plot(density(LeagueIndex_data$AssignToHotkeys[LeagueIndex_data$LeagueIndex_cat==1]), xlab="AssignToHotkeys",
     lwd=2, main='LeagueIndex')
lines(density(LeagueIndex_data$AssignToHotkeys[LeagueIndex_data$LeagueIndex_cat==0]), col = "red", lty = 2, lwd=2)
legend("topright", c('High', 'Low'),
       col = c("black","red"),
       lty = c(1,2), lwd = 1, bty ="n", cex = 1.5)


library(lawstat)
leven_test = levene.test(LeagueIndex_data$AssignToHotkeys, LeagueIndex_data$LeagueIndex_cat)
leven_test
t.test(LeagueIndex_data$AssignToHotkeys ~ LeagueIndex_data$LeagueIndex_cat, paired = F, var.equal = T)




##########################MinimapAttacks###############
#상자그림
par(mfrow = c(1,1))
MinimapAttacks_box = boxplot(data$MinimapAttacks, main = "MinimapAttacks",
                              col = 'lightblue', horizontal = T, ylim = c(0,0.0008))

#밀도함수 그래프
plot(density(data$MinimapAttacks[data$LeagueIndex==1]), xlab="MinimapAttacks",
     lwd=2, main='LeagueIndex', xlim = c(0,0.0003))
lines(density(data$MinimapAttacks[data$LeagueIndex==2]), col = "red", lty = 2, lwd=2)
lines(density(data$MinimapAttacks[data$LeagueIndex==3]), col = "blue", lty = 3,lwd=2)      
lines(density(data$MinimapAttacks[data$LeagueIndex==4]), col = "green", lty = 4,lwd=2)
lines(density(data$MinimapAttacks[data$LeagueIndex==5]), col = "orange", lty = 5,lwd=2)   
lines(density(data$MinimapAttacks[data$LeagueIndex==6]), col = "brown", lty = 6,lwd=2)
lines(density(data$MinimapAttacks[data$LeagueIndex==7]), col = "maroon1", lty = 7,lwd=2)   
lines(density(data$MinimapAttacks[data$LeagueIndex==8]), col = "pink4", lty = 8,lwd=2)   
legend("topright", c('Bronze','Silver','Gold','Platinum','Diamond','Master','GrandMaster', 'Professional'),
       col = c("black","red", "blue",'green','orange','brown','maroon1', 'pink4'),
       lty = c(1,2,3,4,5,6,7,8), lwd = 1, bty ="n", cex = 1.5)


##########################MinimapRightClicks###############
#상자그림
par(mfrow = c(1,1))
mode(MinimapRightClicks)
MinimapRightClicks_box = boxplot(data$MinimapRightClicks, main = "MinimapRightClicks",
                              col = 'lightblue', horizontal = T, ylim = c(0,0.0025))

#밀도함수 그래프
plot(density(data$MinimapRightClicks[data$LeagueIndex==1]), xlab="MinimapRightClicks",
     lwd=2, main='LeagueIndex', xlim = c(0,0.002))
lines(density(data$MinimapRightClicks[data$LeagueIndex==2]), col = "red", lty = 2, lwd=2)
lines(density(data$MinimapRightClicks[data$LeagueIndex==3]), col = "blue", lty = 3,lwd=2)      
lines(density(data$MinimapRightClicks[data$LeagueIndex==4]), col = "green", lty = 4,lwd=2)
lines(density(data$MinimapRightClicks[data$LeagueIndex==5]), col = "orange", lty = 5,lwd=2)   
lines(density(data$MinimapRightClicks[data$LeagueIndex==6]), col = "brown", lty = 6,lwd=2)
lines(density(data$MinimapRightClicks[data$LeagueIndex==7]), col = "maroon1", lty = 7,lwd=2)   
lines(density(data$MinimapRightClicks[data$LeagueIndex==8]), col = "pink4", lty = 8,lwd=2)   
legend("topright", c('Bronze','Silver','Gold','Platinum','Diamond','Master','GrandMaster', 'Professional'),
       col = c("black","red", "blue",'green','orange','brown','maroon1', 'pink4'),
       lty = c(1,2,3,4,5,6,7,8), lwd = 1, bty ="n", cex = 1.5)

#높은 수준의 등급과 낮은 수준의 등급과의 나이 차이 비교
nrow(data[!is.na(data$MinimapRightClicks),]) #Age에 결측값을 제거한 row의 숫자
LeagueIndex_cat = rep(0, 3395) #Age의 결측값을 제거한 숫자만큼 0값 생성
LeagueIndex_cat[data$LeagueIndex == 6] = 1
LeagueIndex_cat[data$LeagueIndex == 5] = 1
LeagueIndex_cat = as.factor(LeagueIndex_cat)
MinimapRightClicks = data$MinimapRightClicks[!is.na(data$MinimapRightClicks)]
LeagueIndex_data = data.frame(LeagueIndex_cat, MinimapRightClicks) 

plot(density(LeagueIndex_data$MinimapRightClicks[LeagueIndex_data$LeagueIndex_cat==1]), xlab="MinimapRightClicks",
     lwd=2, main='LeagueIndex', ylim = c(0, 2100))
lines(density(LeagueIndex_data$MinimapRightClicks[LeagueIndex_data$LeagueIndex_cat==0]), col = "red", lty = 2, lwd=2)
legend("topright", c('High', 'Low'),
       col = c("black","red"),
       lty = c(1,2), lwd = 1, bty ="n", cex = 1.5)


library(lawstat)
leven_test = levene.test(LeagueIndex_data$MinimapRightClicks, LeagueIndex_data$LeagueIndex_cat)
leven_test
t.test(LeagueIndex_data$MinimapRightClicks ~ LeagueIndex_data$LeagueIndex_cat, paired = F, var.equal = F)

########################NumberOfPACs###############
#상자그림
par(mfrow = c(1,1))
NumberOfPACs_box = boxplot(data$NumberOfPACs, main = "NumberOfPACs",
                                 col = 'lightblue', horizontal = T)

#밀도함수 그래프
plot(density(data$NumberOfPACs[data$LeagueIndex==1]), xlab="NumberOfPACs",
     lwd=2, main='LeagueIndex', xlim = c(0, 0.01))
lines(density(data$NumberOfPACs[data$LeagueIndex==2]), col = "red", lty = 2, lwd=2)
lines(density(data$NumberOfPACs[data$LeagueIndex==3]), col = "blue", lty = 3,lwd=2)      
lines(density(data$NumberOfPACs[data$LeagueIndex==4]), col = "green", lty = 4,lwd=2)
lines(density(data$NumberOfPACs[data$LeagueIndex==5]), col = "orange", lty = 5,lwd=2)   
lines(density(data$NumberOfPACs[data$LeagueIndex==6]), col = "brown", lty = 6,lwd=2)
lines(density(data$NumberOfPACs[data$LeagueIndex==7]), col = "maroon1", lty = 7,lwd=2)   
lines(density(data$NumberOfPACs[data$LeagueIndex==8]), col = "pink4", lty = 8,lwd=2)   
legend("topright", c('Bronze','Silver','Gold','Platinum','Diamond','Master','GrandMaster', 'Professional'),
       col = c("black","red", "blue",'green','orange','brown','maroon1', 'pink4'),
       lty = c(1,2,3,4,5,6,7,8), lwd = 1, bty ="n", cex = 1.5)

#높은 수준의 등급과 낮은 수준의 등급과의 나이 차이 비교
nrow(data[!is.na(data$NumberOfPACs),]) #Age에 결측값을 제거한 row의 숫자
LeagueIndex_cat = rep(0, 3395) #Age의 결측값을 제거한 숫자만큼 0값 생성
LeagueIndex_cat[data$LeagueIndex == 6] = 1
LeagueIndex_cat[data$LeagueIndex == 8] = 1
LeagueIndex_cat[data$LeagueIndex == 7] = 1
LeagueIndex_cat = as.factor(LeagueIndex_cat)
NumberOfPACs = data$NumberOfPACs[!is.na(data$NumberOfPACs)]
LeagueIndex_data = data.frame(LeagueIndex_cat, NumberOfPACs) 

plot(density(LeagueIndex_data$NumberOfPACs[LeagueIndex_data$LeagueIndex_cat==1]), xlab="NumberOfPACs",
     lwd=2, main='LeagueIndex', ylim = c(0, 700))
lines(density(LeagueIndex_data$NumberOfPACs[LeagueIndex_data$LeagueIndex_cat==0]), col = "red", lty = 2, lwd=2)
legend("topright", c('High', 'Low'),
       col = c("black","red"),
       lty = c(1,2), lwd = 1, bty ="n", cex = 1.5)


library(lawstat)
leven_test = levene.test(LeagueIndex_data$NumberOfPACs, LeagueIndex_data$LeagueIndex_cat)
leven_test
t.test(LeagueIndex_data$NumberOfPACs ~ LeagueIndex_data$LeagueIndex_cat, paired = F, var.equal = F)


########################GapBetweenPACs###############
#상자그림
par(mfrow = c(1,1))
GapBetweenPACs_box = boxplot(data$GapBetweenPACs, main = "GapBetweenPACs",
                           col = 'lightblue', horizontal = T, ylim = c(0, 150))

#밀도함수 그래프
plot(density(data$GapBetweenPACs[data$LeagueIndex==1]), xlab="GapBetweenPACs",
     lwd=2, main='LeagueIndex', ylim = c(0, 0.08))
lines(density(data$GapBetweenPACs[data$LeagueIndex==2]), col = "red", lty = 2, lwd=2)
lines(density(data$GapBetweenPACs[data$LeagueIndex==3]), col = "blue", lty = 3,lwd=2)      
lines(density(data$GapBetweenPACs[data$LeagueIndex==4]), col = "green", lty = 4,lwd=2)
lines(density(data$GapBetweenPACs[data$LeagueIndex==5]), col = "orange", lty = 5,lwd=2)   
lines(density(data$GapBetweenPACs[data$LeagueIndex==6]), col = "brown", lty = 6,lwd=2)
lines(density(data$GapBetweenPACs[data$LeagueIndex==7]), col = "maroon1", lty = 7,lwd=2)   
lines(density(data$GapBetweenPACs[data$LeagueIndex==8]), col = "pink4", lty = 8,lwd=2)   
legend("topright", c('Bronze','Silver','Gold','Platinum','Diamond','Master','GrandMaster', 'Professional'),
       col = c("black","red", "blue",'green','orange','brown','maroon1', 'pink4'),
       lty = c(1,2,3,4,5,6,7,8), lwd = 1, bty ="n", cex = 1.5)

#높은 수준의 등급과 낮은 수준의 등급과의 나이 차이 비교
nrow(data[!is.na(data$GapBetweenPACs),]) #Age에 결측값을 제거한 row의 숫자
LeagueIndex_cat = rep(0, 3395) #Age의 결측값을 제거한 숫자만큼 0값 생성
LeagueIndex_cat[data$LeagueIndex == 6] = 1
LeagueIndex_cat[data$LeagueIndex == 8] = 1
LeagueIndex_cat[data$LeagueIndex == 7] = 1
LeagueIndex_cat = as.factor(LeagueIndex_cat)
GapBetweenPACs = data$GapBetweenPACs[!is.na(data$GapBetweenPACs)]
LeagueIndex_data = data.frame(LeagueIndex_cat, GapBetweenPACs) 

plot(density(LeagueIndex_data$GapBetweenPACs[LeagueIndex_data$LeagueIndex_cat==1]), xlab="GapBetweenPACs",
     lwd=2, main='LeagueIndex', ylim = c(0, 0.055))
lines(density(LeagueIndex_data$GapBetweenPACs[LeagueIndex_data$LeagueIndex_cat==0]), col = "red", lty = 2, lwd=2)
legend("topright", c('High', 'Low'),
       col = c("black","red"),
       lty = c(1,2), lwd = 1, bty ="n", cex = 1.5)


library(lawstat)
leven_test = levene.test(LeagueIndex_data$GapBetweenPACs, LeagueIndex_data$LeagueIndex_cat)
leven_test
t.test(LeagueIndex_data$GapBetweenPACs ~ LeagueIndex_data$LeagueIndex_cat, paired = F, var.equal = F)

########################ActionLatency###############
#상자그림
par(mfrow = c(1,1))
ActionLatency_box = boxplot(data$ActionLatency, main = "ActionLatency",
                             col = 'lightblue', horizontal = T)

#밀도함수 그래프
plot(density(data$ActionLatency[data$LeagueIndex==1]), xlab="ActionLatency",
     lwd=2, main='LeagueIndex', ylim = c(0, 0.08))
lines(density(data$ActionLatency[data$LeagueIndex==2]), col = "red", lty = 2, lwd=2)
lines(density(data$ActionLatency[data$LeagueIndex==3]), col = "blue", lty = 3,lwd=2)      
lines(density(data$ActionLatency[data$LeagueIndex==4]), col = "green", lty = 4,lwd=2)
lines(density(data$ActionLatency[data$LeagueIndex==5]), col = "orange", lty = 5,lwd=2)   
lines(density(data$ActionLatency[data$LeagueIndex==6]), col = "brown", lty = 6,lwd=2)
lines(density(data$ActionLatency[data$LeagueIndex==7]), col = "maroon1", lty = 7,lwd=2)   
lines(density(data$ActionLatency[data$LeagueIndex==8]), col = "pink4", lty = 8,lwd=2)   
legend("topright", c('Bronze','Silver','Gold','Platinum','Diamond','Master','GrandMaster', 'Professional'),
       col = c("black","red", "blue",'green','orange','brown','maroon1', 'pink4'),
       lty = c(1,2,3,4,5,6,7,8), lwd = 1, bty ="n", cex = 1.5)

#높은 수준의 등급과 낮은 수준의 등급과의 나이 차이 비교
nrow(data[!is.na(data$ActionLatency),]) #Age에 결측값을 제거한 row의 숫자
LeagueIndex_cat = rep(0, 3395) #Age의 결측값을 제거한 숫자만큼 0값 생성
LeagueIndex_cat[data$LeagueIndex == 6] = 1
LeagueIndex_cat[data$LeagueIndex == 8] = 1
LeagueIndex_cat[data$LeagueIndex == 7] = 1
LeagueIndex_cat = as.factor(LeagueIndex_cat)
ActionLatency = data$ActionLatency[!is.na(data$ActionLatency)]
LeagueIndex_data = data.frame(LeagueIndex_cat, ActionLatency) 

plot(density(LeagueIndex_data$ActionLatency[LeagueIndex_data$LeagueIndex_cat==1]), xlab="ActionLatency",
     lwd=2, main='LeagueIndex')
lines(density(LeagueIndex_data$ActionLatency[LeagueIndex_data$LeagueIndex_cat==0]), col = "red", lty = 2, lwd=2)
legend("topright", c('High', 'Low'),
       col = c("black","red"),
       lty = c(1,2), lwd = 1, bty ="n", cex = 1.5)


library(lawstat)
leven_test = levene.test(LeagueIndex_data$ActionLatency, LeagueIndex_data$LeagueIndex_cat)
leven_test
t.test(LeagueIndex_data$ActionLatency ~ LeagueIndex_data$LeagueIndex_cat, paired = F, var.equal = F)

########################ActionsInPAC###############
#상자그림
par(mfrow = c(1,1))
ActionsInPAC_box = boxplot(data$ActionsInPAC, main = "ActionsInPAC",
                            col = 'lightblue', horizontal = T, ylim = c(0,15))

#밀도함수 그래프
plot(density(data$ActionsInPAC[data$LeagueIndex==1]), xlab="ActionsInPAC",
     lwd=2, main='LeagueIndex', ylim = c(0, 0.4))
lines(density(data$ActionsInPAC[data$LeagueIndex==2]), col = "red", lty = 2, lwd=2)
lines(density(data$ActionsInPAC[data$LeagueIndex==3]), col = "blue", lty = 3,lwd=2)      
lines(density(data$ActionsInPAC[data$LeagueIndex==4]), col = "green", lty = 4,lwd=2)
lines(density(data$ActionsInPAC[data$LeagueIndex==5]), col = "orange", lty = 5,lwd=2)   
lines(density(data$ActionsInPAC[data$LeagueIndex==6]), col = "brown", lty = 6,lwd=2)
lines(density(data$ActionsInPAC[data$LeagueIndex==7]), col = "maroon1", lty = 7,lwd=2)   
lines(density(data$ActionsInPAC[data$LeagueIndex==8]), col = "pink4", lty = 8,lwd=2)   
legend("topright", c('Bronze','Silver','Gold','Platinum','Diamond','Master','GrandMaster', 'Professional'),
       col = c("black","red", "blue",'green','orange','brown','maroon1', 'pink4'),
       lty = c(1,2,3,4,5,6,7,8), lwd = 1, bty ="n", cex = 1.5)

#높은 수준의 등급과 낮은 수준의 등급과의 나이 차이 비교
nrow(data[!is.na(data$ActionsInPAC),]) #Age에 결측값을 제거한 row의 숫자
LeagueIndex_cat = rep(0, 3395) #Age의 결측값을 제거한 숫자만큼 0값 생성
LeagueIndex_cat[data$LeagueIndex == 6] = 1
LeagueIndex_cat[data$LeagueIndex == 8] = 1
LeagueIndex_cat[data$LeagueIndex == 7] = 1
LeagueIndex_cat = as.factor(LeagueIndex_cat)
ActionsInPAC = data$ActionsInPAC[!is.na(data$ActionsInPAC)]
LeagueIndex_data = data.frame(LeagueIndex_cat, ActionsInPAC) 

plot(density(LeagueIndex_data$ActionsInPAC[LeagueIndex_data$LeagueIndex_cat==1]), xlab="ActionsInPAC",
     lwd=2, main='LeagueIndex')
lines(density(LeagueIndex_data$ActionsInPAC[LeagueIndex_data$LeagueIndex_cat==0]), col = "red", lty = 2, lwd=2)
legend("topright", c('High', 'Low'),
       col = c("black","red"),
       lty = c(1,2), lwd = 1, bty ="n", cex = 1.5)


library(lawstat)
leven_test = levene.test(LeagueIndex_data$ActionsInPAC, LeagueIndex_data$LeagueIndex_cat)
leven_test
t.test(LeagueIndex_data$ActionsInPAC ~ LeagueIndex_data$LeagueIndex_cat, paired = F, var.equal = F)


########################TotalMapExplored###############
#상자그림
par(mfrow = c(1,1))
TotalMapExplored_box = boxplot(data$TotalMapExplored, main = "TotalMapExplored",
                           col = 'lightblue', horizontal = T)
text(TotalMapExplored_box$out, rep(1, NROW(TotalMapExplored_box$out)), labels = TotalMapExplored_box$out, pos = c(1,3))


#밀도함수 그래프
plot(density(data$TotalMapExplored[data$LeagueIndex==1]), xlab="TotalMapExplored",
     lwd=2, main='LeagueIndex', ylim = c(0, 0.08))
lines(density(data$TotalMapExplored[data$LeagueIndex==2]), col = "red", lty = 2, lwd=2)
lines(density(data$TotalMapExplored[data$LeagueIndex==3]), col = "blue", lty = 3,lwd=2)      
lines(density(data$TotalMapExplored[data$LeagueIndex==4]), col = "green", lty = 4,lwd=2)
lines(density(data$TotalMapExplored[data$LeagueIndex==5]), col = "orange", lty = 5,lwd=2)   
lines(density(data$TotalMapExplored[data$LeagueIndex==6]), col = "brown", lty = 6,lwd=2)
lines(density(data$TotalMapExplored[data$LeagueIndex==7]), col = "maroon1", lty = 7,lwd=2)   
lines(density(data$TotalMapExplored[data$LeagueIndex==8]), col = "pink4", lty = 8,lwd=2)   
legend("topright", c('Bronze','Silver','Gold','Platinum','Diamond','Master','GrandMaster', 'Professional'),
       col = c("black","red", "blue",'green','orange','brown','maroon1', 'pink4'),
       lty = c(1,2,3,4,5,6,7,8), lwd = 1, bty ="n", cex = 1.5)

#높은 수준의 등급과 낮은 수준의 등급과의 나이 차이 비교
nrow(data[!is.na(data$TotalMapExplored),]) #Age에 결측값을 제거한 row의 숫자
LeagueIndex_cat = rep(0, 3395) #Age의 결측값을 제거한 숫자만큼 0값 생성
LeagueIndex_cat[data$LeagueIndex == 6] = 1
LeagueIndex_cat[data$LeagueIndex == 8] = 1
LeagueIndex_cat[data$LeagueIndex == 7] = 1
LeagueIndex_cat = as.factor(LeagueIndex_cat)
TotalMapExplored = data$TotalMapExplored[!is.na(data$TotalMapExplored)]
LeagueIndex_data = data.frame(LeagueIndex_cat, TotalMapExplored) 

plot(density(LeagueIndex_data$TotalMapExplored[LeagueIndex_data$LeagueIndex_cat==1]), xlab="TotalMapExplored",
     lwd=2, main='LeagueIndex')
lines(density(LeagueIndex_data$TotalMapExplored[LeagueIndex_data$LeagueIndex_cat==0]), col = "red", lty = 2, lwd=2)
legend("topright", c('High', 'Low'),
       col = c("black","red"),
       lty = c(1,2), lwd = 1, bty ="n", cex = 1.5)


library(lawstat)
leven_test = levene.test(LeagueIndex_data$TotalMapExplored, LeagueIndex_data$LeagueIndex_cat)
leven_test
t.test(LeagueIndex_data$TotalMapExplored ~ LeagueIndex_data$LeagueIndex_cat, paired = F, var.equal = T)


########################WorkersMade###############
#상자그림
par(mfrow = c(1,1))
WorkersMade_box = boxplot(data$WorkersMade, main = "WorkersMade",
                               col = 'lightblue', horizontal = T)
text(WorkersMade_box$out, rep(1, NROW(WorkersMade_box$out)), labels = WorkersMade_box$out, pos = c(1,3))


#밀도함수 그래프
plot(density(data$WorkersMade[data$LeagueIndex==1]), xlab="WorkersMade",
     lwd=2, main='LeagueIndex', ylim = c(0, 2000))
lines(density(data$WorkersMade[data$LeagueIndex==2]), col = "red", lty = 2, lwd=2)
lines(density(data$WorkersMade[data$LeagueIndex==3]), col = "blue", lty = 3,lwd=2)      
lines(density(data$WorkersMade[data$LeagueIndex==4]), col = "green", lty = 4,lwd=2)
lines(density(data$WorkersMade[data$LeagueIndex==5]), col = "orange", lty = 5,lwd=2)   
lines(density(data$WorkersMade[data$LeagueIndex==6]), col = "brown", lty = 6,lwd=2)
lines(density(data$WorkersMade[data$LeagueIndex==7]), col = "maroon1", lty = 7,lwd=2)   
lines(density(data$WorkersMade[data$LeagueIndex==8]), col = "pink4", lty = 8,lwd=2)   
legend("topright", c('Bronze','Silver','Gold','Platinum','Diamond','Master','GrandMaster', 'Professional'),
       col = c("black","red", "blue",'green','orange','brown','maroon1', 'pink4'),
       lty = c(1,2,3,4,5,6,7,8), lwd = 1, bty ="n", cex = 1.5)

#높은 수준의 등급과 낮은 수준의 등급과의 나이 차이 비교
nrow(data[!is.na(data$WorkersMade),]) #Age에 결측값을 제거한 row의 숫자
LeagueIndex_cat = rep(0, 3395) #Age의 결측값을 제거한 숫자만큼 0값 생성
LeagueIndex_cat[data$LeagueIndex == 6] = 1
LeagueIndex_cat[data$LeagueIndex == 8] = 1
LeagueIndex_cat[data$LeagueIndex == 7] = 1
LeagueIndex_cat = as.factor(LeagueIndex_cat)
WorkersMade = data$WorkersMade[!is.na(data$WorkersMade)]
LeagueIndex_data = data.frame(LeagueIndex_cat, WorkersMade) 

plot(density(LeagueIndex_data$WorkersMade[LeagueIndex_data$LeagueIndex_cat==1]), xlab="WorkersMade",
     lwd=2, main='LeagueIndex', ylim = c(0, 1200))
lines(density(LeagueIndex_data$WorkersMade[LeagueIndex_data$LeagueIndex_cat==0]), col = "red", lty = 2, lwd=2)
legend("topright", c('High', 'Low'),
       col = c("black","red"),
       lty = c(1,2), lwd = 1, bty ="n", cex = 1.5)


library(lawstat)
leven_test = levene.test(LeagueIndex_data$WorkersMade, LeagueIndex_data$LeagueIndex_cat)
leven_test
t.test(LeagueIndex_data$WorkersMade ~ LeagueIndex_data$LeagueIndex_cat, paired = F, var.equal = T)


########################UniqueUnitsMade###############
#상자그림
par(mfrow = c(1,1))
UniqueUnitsMade_box = boxplot(data$UniqueUnitsMade, main = "UniqueUnitsMade",
                          col = 'lightblue', horizontal = T)
text(UniqueUnitsMade_box$out, rep(1, NROW(UniqueUnitsMade_box$out)), labels = UniqueUnitsMade_box$out, pos = c(1,3))


#밀도함수 그래프
plot(density(data$UniqueUnitsMade[data$LeagueIndex==1]), xlab="UniqueUnitsMade",
     lwd=2, main='LeagueIndex', ylim = c(0, 0.35))
lines(density(data$UniqueUnitsMade[data$LeagueIndex==2]), col = "red", lty = 2, lwd=2)
lines(density(data$UniqueUnitsMade[data$LeagueIndex==3]), col = "blue", lty = 3,lwd=2)      
lines(density(data$UniqueUnitsMade[data$LeagueIndex==4]), col = "green", lty = 4,lwd=2)
lines(density(data$UniqueUnitsMade[data$LeagueIndex==5]), col = "orange", lty = 5,lwd=2)   
lines(density(data$UniqueUnitsMade[data$LeagueIndex==6]), col = "brown", lty = 6,lwd=2)
lines(density(data$UniqueUnitsMade[data$LeagueIndex==7]), col = "maroon1", lty = 7,lwd=2)   
lines(density(data$UniqueUnitsMade[data$LeagueIndex==8]), col = "pink4", lty = 8,lwd=2)   
legend("topright", c('Bronze','Silver','Gold','Platinum','Diamond','Master','GrandMaster', 'Professional'),
       col = c("black","red", "blue",'green','orange','brown','maroon1', 'pink4'),
       lty = c(1,2,3,4,5,6,7,8), lwd = 1, bty ="n", cex = 1.5)

#######################ComplexUnitsMade###############
#상자그림
par(mfrow = c(1,1))
ComplexUnitsMade_box = boxplot(data$ComplexUnitsMade, main = "ComplexUnitsMade",
                          col = 'lightblue', horizontal = T)


#밀도함수 그래프
plot(density(data$ComplexUnitsMade[data$LeagueIndex==1]), xlab="ComplexUnitsMade",
     lwd=2, main='LeagueIndex', ylim = c(0, 50000))
lines(density(data$ComplexUnitsMade[data$LeagueIndex==2]), col = "red", lty = 2, lwd=2)
lines(density(data$ComplexUnitsMade[data$LeagueIndex==3]), col = "blue", lty = 3,lwd=2)      
lines(density(data$ComplexUnitsMade[data$LeagueIndex==4]), col = "green", lty = 4,lwd=2)
lines(density(data$ComplexUnitsMade[data$LeagueIndex==5]), col = "orange", lty = 5,lwd=2)   
lines(density(data$ComplexUnitsMade[data$LeagueIndex==6]), col = "brown", lty = 6,lwd=2)
lines(density(data$ComplexUnitsMade[data$LeagueIndex==7]), col = "maroon1", lty = 7,lwd=2)   
lines(density(data$ComplexUnitsMade[data$LeagueIndex==8]), col = "pink4", lty = 8,lwd=2)   
legend("topright", c('Bronze','Silver','Gold','Platinum','Diamond','Master','GrandMaster', 'Professional'),
       col = c("black","red", "blue",'green','orange','brown','maroon1', 'pink4'),
       lty = c(1,2,3,4,5,6,7,8), lwd = 1, bty ="n", cex = 1.5)



########################ComplexAbilitiesUsed###############
#상자그림
par(mfrow = c(1,1))
ComplexAbilitiesUsed_box = boxplot(data$ComplexAbilitiesUsed, main = "ComplexAbilitiesUsed",
                          col = 'lightblue', horizontal = T, ylim = c(0, 0.0017))


#밀도함수 그래프
plot(density(data$ComplexAbilitiesUsed[data$LeagueIndex==1]), xlab="ComplexAbilitiesUsed",
     lwd=2, main='LeagueIndex')
lines(density(data$ComplexAbilitiesUsed[data$LeagueIndex==2]), col = "red", lty = 2, lwd=2)
lines(density(data$ComplexAbilitiesUsed[data$LeagueIndex==3]), col = "blue", lty = 3,lwd=2)      
lines(density(data$ComplexAbilitiesUsed[data$LeagueIndex==4]), col = "green", lty = 4,lwd=2)
lines(density(data$ComplexAbilitiesUsed[data$LeagueIndex==5]), col = "orange", lty = 5,lwd=2)   
lines(density(data$ComplexAbilitiesUsed[data$LeagueIndex==6]), col = "brown", lty = 6,lwd=2)
lines(density(data$ComplexAbilitiesUsed[data$LeagueIndex==7]), col = "maroon1", lty = 7,lwd=2)   
lines(density(data$ComplexAbilitiesUsed[data$LeagueIndex==8]), col = "pink4", lty = 8,lwd=2)   
legend("topright", c('Bronze','Silver','Gold','Platinum','Diamond','Master','GrandMaster', 'Professional'),
       col = c("black","red", "blue",'green','orange','brown','maroon1', 'pink4'),
       lty = c(1,2,3,4,5,6,7,8), lwd = 1, bty ="n", cex = 1.5)


###############결측값 제거######################
data_na = data[!is.na(data$Age) & !is.na(data$HoursPerWeek)  & ! is.na(data$TotalHours),]
View(colSums(is.na(data)))

#############LeagueIndex와 관련있는 변수###############

par(mfrow = c(1,1), cex = 2)
pairs(~LeagueIndex+APM+AssignToHotkeys+MinimapRightClicks+NumberOfPACs+GapBetweenPACs+ActionLatency+ActionsInPAC
      +TotalMapExplored+WorkersMade,data=data, 
      main="Simple Scatterplot Matrix", cex = 0.1)


data_na2 = data.frame(data_na$LeagueIndex,data_na$APM,data_na$AssignToHotkeys, data_na$NumberOfPACs, 
                      data_na$GapBetweenPACs, data_na$ActionLatency, data_na$WorkersMade)

names(data_na2)= c('LeagueIndex','APM','AssignToHotkeys','NumberOfPACs','GapBetweenPACs','ActionLatency',
                   'WorkersMade')
data_na2$LeagueIndex = as.numeric(data_na2$LeagueIndex)

View(cor(data_na2)

data_na2 =
data_na2 = data$
cor()

#APM, SelectByHotkeys, AssignToHotkeys
pairs(~LeagueIndex+APM+SelectByHotkeys+AssignToHotkeys+UniqueHotkeys,data=data, 
      main="Simple Scatterplot Matrix")

#NumberOfPACs,ActionLatency
pairs(~LeagueIndex +MinimapAttacks+MinimapRightClicks+NumberOfPACs+GapBetweenPACs+ActionLatency,data=data, 
      main="Simple Scatterplot Matrix")

pairs(~LeagueIndex+ActionsInPAC+TotalMapExplored+WorkersMade+UniqueUnitsMade+ComplexUnitsMade+ComplexAbilitiesUsed ,data=data, 
      main="Simple Scatterplot Matrix")

#

pairs(~LeagueIndex+APM+SelectByHotkeys+AssignToHotkeys+NumberOfPACs+ActionLatency+GapBetweenPACs,data=data, 
      main="Simple Scatterplot Matrix")
attach(data)

View(cor(data_na2))
cor_matrix = cor(subset(data_na2, select = -c(GameID,LeagueIndex)), data_na2$LeagueIndex,method = 'pearson')
colnames(cor_matrix)[1] = 'LeagueIndex'
cor_matrix = cor_matrix[abs(cor_matrix) >= 0.5,]
View(t(cor_matrix))
cor(data_na$Age, data_na$HoursPerWeek, method = 'pearson')
cor(data_na$Age, data_na)

cor_matrix2 = cor(subset(data_na2, select = c(APM, SelectByHotkeys, AssignToHotkeys, NumberOfPACs, ActionLatency)), data_na2$LeagueIndex,method = 'pearson')
colnames(cor_matrix2)[1] = 'LeagueIndex'
View(cor_matrix2)


#library(dplyr)
subset(data_na2, select = -c(Age, HoursPerWeek, TotalHours)) %>%
  group_by(LeagueIndex) %>%
  summarize_all(funs(mean, median, max, min))->k
View(k)


data$LeagueIndex = ordered(data$LeagueIndex)
data$Age = as.numeric(data$Age)
data$HoursPerWeek = as.numeric(data$HoursPerWeek)
data$TotalHours = as.numeric(data$TotalHours)

