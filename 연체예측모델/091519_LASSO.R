setwd('C:/Users/User/Desktop/bigcontest/data') #디렉토리 설정
data_set <- read.table('data_set_3.csv', header = T, sep = ',', stringsAsFactors= T) #데이터 불러오기 


####################자료형에 적절한 변수처리######################
data_set = data_set[,-1] #CUST_ID 제거 
data_set$X1 = as.factor(data_set$X1) #Target factor처리


###################Data분리###################
#Train data에서 타겟을 같은 비율로 샘플링하는 oversampling 적용. (oversampling 미적용시, 해당 방법으로는 타겟 분류 불가했음)
set.seed(1)
data_set_1 = data_set[data_set$X1 == 1,] #Target 1 추출
data_set_0 = data_set[data_set$X1 == 0,] #Target 2 추출

train_1 = sample(length(data_set_1[,1]), 3001, replace = F) #전체 Target 1중 70% 해당하는 3001개 추출을 위해 관측치 넘버 랜덤 추출
train_0 = sample(length(data_set_0[,1]), 70163, replace = F) 

data_test_1 = data_set_1[-train_1,]
data_test_0 = data_set_0[-train_0,]
data_test = rbind(data_test_0, data_test_1)

data_set_1 = data_set_1[train_1,]
data_set_0 = data_set_0[train_0,]
data_train = rbind(data_set_0, data_set_1)


###################SMOTE###########################
library(DMwR)

data_SMOTE = SMOTE(X1~., data_train, perc.over = 800, perc.under = 100)
table(data_SMOTE$X1)


###################LASSO 적합##################################
#lambda 랜덤 추출 범위와, 모델 평가 척도를 다르게 하여 lasso 실행

#1번 gird(5, -5), mae, alpha(1), 5번과 다르지 않으므로 생략
x = model.matrix(X1~., data_SMOTE)
y = data_SMOTE$X1
grid = 10^seq(5, -5, length = 100)
library(glmnet)
cv.out = cv.glmnet(x, y, alpha = 1, lambda = grid, family = 'binomial', type.measure = 'mae')
plot(cv.out)
bestlambda = cv.out$lambda.min
bestlambda


fit = glmnet(x, y, alpha = 1, lambda = bestlambda, family = 'binomial')
x_test = model.matrix(X1~., data_test)
y_test = data_test$X1
pred = predict(fit, newx = x_test, type = "response", s = bestlambda ) #s는 lambda에 따른 fitting 결과


#5번 grid(5,-5), auc, alpha(1)
x = model.matrix(X1~., data_SMOTE)
y = data_SMOTE$X1
grid5 = 10^seq(0, -10, length = 100)
library(glmnet)
cv.out5 = cv.glmnet(x, y, alpha = 1, lambda = grid5, family = 'binomial', type.measure = 'auc')
plot(cv.out5)
bestlambda5 = cv.out5$lambda.min
bestlambda5

fit5 = glmnet(x, y, alpha = 1, lambda = bestlambda5, family = 'binomial')

x_test = model.matrix(X1~., data_test)
y_test = data_test$X1
pred5 = predict(fit5, newx = x_test, type = "response", s = bestlambda5 ) #s는 lambda에 따른 fitting 결과




#평가
target = c()
target[pred>0.5] = 1
target[pred<=0.5] = 0
result <- table(y_test, target)
Precision <- result[4] / (result[3] + result[4])
Recall <- result[4] / (result[2] + result[4])
f.value <- 2 * (Precision * Recall) / (Precision + Recall)

target5 = c()
target5[pred5>0.823] = 1
target5[pred5<=0.823] = 0
result <- table(target5, y_test)
Precision <- result[4] / (result[2] + result[4])
Recall <- result[4] / (result[3] + result[4])
f.value5 <- 2 * (Precision * Recall) / (Precision + Recall)


#F값 비교
f.value
f.value5

fit5$beta

#Beta값 비교
beta_table =  cbind(fit$beta, fit2$beta, fit3$beta, fit4$beta, fit5$beta)


#모집단의 비율만큼 random Selection한 경우의 정확
tar = rep(0, 30056)
n = sample(30056, 14652, replace = F)
tar[n] = 1
result = table(y_test, tar)
result

