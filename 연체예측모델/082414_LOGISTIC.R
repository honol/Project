############################## data_set 원본 데이터 ##############################
setwd('C:/Users/User/Desktop/bigcontest/data')

data_set <- read.table('Data_set.csv', header = T, sep = ',', stringsAsFactors= F)

data_na = data_set

# OCCP_NAME_G : '*' -> NA
data_na$OCCP_NAME_G[data_na$OCCP_NAME_G == '*'] <- NA

# MATE_OCCP_NAME_G : '*' -> NA
data_na$MATE_OCCP_NAME_G[data_na$MATE_OCCP_NAME_G == '*'] <- NA

# AGE : '*' -> NA
data_na$AGE[data_na$AGE == '*'] <- NA

# SEX : '*' -> NA
data_na$SEX[data_na$SEX == '*'] <- NA

# LAST_CHLD_AGE : 'NULL' -> NA
data_na$LAST_CHLD_AGE[which(data_na$LAST_CHLD_AGE == 'NULL')] <- NA

data_narm = na.omit(data_na)


############################## data_factor : 범주화 변수 변환 ##############################
data_factor <- data_narm


#클러스터링 변수 선택(범주형 중 수준 축소가 필요한 경우)
SCI <- cbind(data_factor$TOT_LNIF_AMT, data_factor$TOT_CLIF_AMT,      
             data_factor$BNK_LNIF_AMT, data_factor$CPT_LNIF_AMT, 
             data_factor$CB_GUIF_AMT)

HAN <- cbind(data_factor$CUST_JOB_INCM, data_factor$HSHD_INFR_INCM,
             data_factor$LAST_CHLD_AGE, data_factor$MATE_JOB_INCM, data_factor$TOT_CRLN_AMT,
             data_factor$TOT_REPY_AMT, data_factor$STLN_REMN_AMT, data_factor$LT1Y_STLN_AMT,
             data_factor$GDINS_MON_PREM, data_factor$SVINS_MON_PREM, data_factor$FMLY_GDINS_MNPREM,
             data_factor$FMLY_SVINS_MNPREM, data_factor$MAX_MON_PREM, data_factor$TOT_PREM,
             data_factor$FMLY_TOT_PREM, data_factor$FYCM_PAID_AMT, data_factor$FMLY_CLAM_CNT, data_factor$AGE)
SKT <- cbind(data_factor$AVG_CALL_TIME,
             data_factor$AVG_CALL_FREQ, data_factor$ARPU,
             data_factor$MON_TLFE_AMT, data_factor$MOBL_FATY_PRC,
             data_factor$NUM_DAY_SUSP, data_factor$CRMM_OVDU_AMT,
             data_factor$LT1Y_MXOD_AMT, data_factor$MOBL_PRIN)

#cluster값으로 변환한 변수들을 data_Train에 삽입
#k$cluster 값을 할당하여 변수를 10개 범주로 변환

#SCI에 속하는 변수 중, cluster가 필요한 변수를 변환함. 변환한 각 변수를 SCI[,i]에 저장
for (i in 1:5)
{
  SCI[,i] = kmeans(SCI[,i], 10)$cluster
}

#HAN에 속하는 변수 중, cluster가 필요한 변수를 변환함. 변환한 각 변수를 HAN[,i]에 저장
for (i in 1:18)
{
  HAN[,i] = kmeans(HAN[,i], 10)$cluster
}

#SKT에 속하는 변수 중, cluster가 필요한 변수를 변환함. 변환한 각 변수를 SKT[,i]에 저장
for (i in 1:9)
{
  SKT[,i] = kmeans(SKT[,i], 10)$cluster
}

#data_Train에서 각 변수의 인덱스 파악
names(data_factor)
SCI_num = c(7, 8, 9, 10, 16)
HAN_num = c(18, 19, 22, 24, 27, 28, 37, 38, 40, 41, 42, 43, 44, 45, 46, 50, 51, 53)
SKT_num = c(55, 56, 58, 59, 61, 63, 64, 66, 69)

j = 1
for (i in SCI_num)
{
  data_factor[,i] = SCI[,j]
  j = j + 1
  print(j)
}

j = 1
for (i in HAN_num)
{
  data_factor[,i] = HAN[,j]
  j = j + 1
  print(j)
}

j = 1
for (i in SKT_num)
{
  data_factor[,i] = SKT[,j]
  j = j + 1
  print(j)
}


#범주화
data_factor$TOT_LNIF_AMT <- as.factor(data_factor$TOT_LNIF_AMT)
data_factor$TOT_CLIF_AMT <- as.factor(data_factor$TOT_CLIF_AMT)
data_factor$BNK_LNIF_AMT <- as.factor(data_factor$BNK_LNIF_AMT)
data_factor$CPT_LNIF_AMT <- as.factor(data_factor$CPT_LNIF_AMT)
data_factor$CRDT_OCCR_MDIF <- as.factor(data_factor$CRDT_OCCR_MDIF)
data_factor$SPTCT_OCCR_MDIF <- as.factor(data_factor$SPTCT_OCCR_MDIF)
data_factor$CTCD_OCCR_MDIF <- as.factor(data_factor$CTCD_OCCR_MDIF)
data_factor$CB_GUIF_AMT <- as.factor(data_factor$CB_GUIF_AMT)
data_factor$CUST_JOB_INCM <- as.factor(data_factor$CUST_JOB_INCM)
data_factor$HSHD_INFR_INCM <- as.factor(data_factor$HSHD_INFR_INCM)
data_factor$LAST_CHLD_AGE <- as.factor(data_factor$LAST_CHLD_AGE)
data_factor$MATE_JOB_INCM <- as.factor(data_factor$MATE_JOB_INCM)
data_factor$TOT_CRLN_AMT <- as.factor(data_factor$TOT_CRLN_AMT)
data_factor$TOT_REPY_AMT <- as.factor(data_factor$TOT_REPY_AMT)
data_factor$STRT_CRDT_GRAD <- as.factor(data_factor$STRT_CRDT_GRAD)
data_factor$LTST_CRDT_GRAD <- as.factor(data_factor$LTST_CRDT_GRAD)
data_factor$LT1Y_PEOD_RATE <- as.factor(data_factor$LT1Y_PEOD_RATE)
data_factor$STLN_REMN_AMT <- as.factor(data_factor$STLN_REMN_AMT)
data_factor$LT1Y_STLN_AMT <- as.factor(data_factor$LT1Y_STLN_AMT)
data_factor$LT1Y_SLOD_RATE <- as.factor(data_factor$LT1Y_SLOD_RATE)
data_factor$GDINS_MON_PREM <- as.factor(data_factor$GDINS_MON_PREM)
data_factor$SVINS_MON_PREM <- as.factor(data_factor$SVINS_MON_PREM)
data_factor$FMLY_GDINS_MNPREM <- as.factor(data_factor$FMLY_GDINS_MNPREM)
data_factor$FMLY_SVINS_MNPREM <- as.factor(data_factor$FMLY_SVINS_MNPREM)
data_factor$MAX_MON_PREM <- as.factor(data_factor$MAX_MON_PREM)
data_factor$TOT_PREM <- as.factor(data_factor$TOT_PREM)
data_factor$FMLY_TOT_PREM <- as.factor(data_factor$FMLY_TOT_PREM)
data_factor$FYCM_PAID_AMT <- as.factor(data_factor$FYCM_PAID_AMT)
data_factor$ARPU <- as.factor(data_factor$ARPU)
data_factor$MON_TLFE_AMT <- as.factor(data_factor$MON_TLFE_AMT)
data_factor$MOBL_FATY_PRC <- as.factor(data_factor$MOBL_FATY_PRC)
data_factor$CRMM_OVDU_AMT <- as.factor(data_factor$CRMM_OVDU_AMT)
data_factor$LT1Y_MXOD_AMT <- as.factor(data_factor$LT1Y_MXOD_AMT)
data_factor$MOBL_PRIN <- as.factor(data_factor$MOBL_PRIN)

# 범주화(순서존재NO)
data_factor$TARGET <- as.factor(data_factor$TARGET)
data_factor$OCCP_NAME_G <- as.factor(data_factor$OCCP_NAME_G)
data_factor$MATE_OCCP_NAME_G <- as.factor(data_factor$MATE_OCCP_NAME_G)
data_factor$SEX <- as.factor(data_factor$SEX)
data_factor$TEL_MBSP_GRAD <- as.factor(data_factor$TEL_MBSP_GRAD)
data_factor$CBPT_MBSP_YN <- as.factor(data_factor$CBPT_MBSP_YN)
data_factor$PAYM_METD <- as.factor(data_factor$PAYM_METD)
data_factor$LINE_STUS <- as.factor(data_factor$LINE_STUS)


# 범주화(날짜)
data_factor$MIN_CNTT_DATE <- as.factor(data_factor$MIN_CNTT_DATE)
data_factor$TEL_CNTT_QTR <- as.factor(data_factor$TEL_CNTT_QTR)


#동일한 비율로 타겟 변수 추출
data_od = data_factor[which(data_factor$TARGET == 1),]
data_rp = data_factor[which(data_factor$TARGET == 0),]

set.seed(123)
randomNumber_od = sample(1:4243, size = 3395, replace = F)
randomNumber_rp = sample(1:94801, size = 3395, replace = F)


data_od_train = data_od[randomNumber_od,]
data_rp_train = data_rp[randomNumber_rp,]

data_od_test = data_od[-randomNumber_od,]
data_rp_test = data_rp[-randomNumber_rp,]

data_Test = rbind(data_od_test, data_rp_test)
data_Train = rbind(data_od_train, data_rp_train)



#모델 생성 및 예측
data_Train2 = subset(data_Train, select = -c(CUST_ID, MIN_CNTT_DATE, ARPU, TEL_CNTT_QTR))
data_Test2 = subset(data_Test, select = -c(CUST_ID, MIN_CNTT_DATE, ARPU, TEL_CNTT_QTR))
model = glm(TARGET ~.,data = data_Train2, family = binomial)
a = summary(model)$coefficients[,4]
b=c()
for (i in 1:385){
  if (a[i] <= 0.1)
  {
    b = c(b, a[i])
  }
  
}
a[4]
str(bb)
bb = unclass(b)
names(b)
names(a)

names(model$coefficients)
names(model)
summary(model)
model$effects

prob = predict(model, data_Test2, type = 'response')
prob
#test set과 비교
idx = 1:92254
for (i in idx)
{
  if (prob[i] >= 0.9)
  {
    prob[i] = 1
  }
  else
  {
    prob[i] = 0
  }
}

data_Test2$TARGET
table(prob)
table(data_Test2$TARGET)
result <- table(data_Test2$TARGET, prob)
Precision <- result[4] / (result[3] + result[4])
Recall <- result[4] / (result[2] + result[4])
f.value <- 2 * (Precision * Recall) / (Precision + Recall)
f.value

cc <- c("BNK_LNIF_CNT" ,"SPART_LNIF_CNT","ECT_LNIF_CNT"
        ,"TOT_LNIF_AMT"
        ,"BNK_LNIF_AMT","CRDT_OCCR_MDIF"       
        ,"SPTCT_OCCR_MDIF","CRDT_CARD_CNT", "CTCD_OCCR_MDIF"
        ,"OCCP_NAME_G", "HSHD_INFR_INCM", "LAST_CHLD_AGE" ,"MATE_JOB_INCM"      
        ,"TOT_CRLN_AMT","TOT_CRLN_AMT"        
        ,"TOT_REPY_AMT"          
        ,"LT1Y_CLOD_RATE", "STRT_CRDT_GRAD"          
        ,"LT1Y_PEOD_RATE", "STLN_REMN_AMT" ,"LT1Y_STLN_AMT"       
        ,"GDINS_MON_PREM", "TOT_PREM" , "FMLY_TOT_PREM"           
        ,"CNTT_LAMT_CNT" ,"FMLY_CLAM_CNT" , "AGE" , "SEX" , "AVG_CALL_FREQ"            
        ,"TEL_MBSP_GRAD" , "MON_TLFE_AMT"            
        ,"CBPT_MBSP_YN" , "MOBL_FATY_PRC" , "CRMM_OVDU_AMT"           
        ,"TLFE_UNPD_CNT" ,"LT1Y_MXOD_AMT" ,"PAYM_METD" ,"PAYM_METD"               
        ,"LINE_STUS"   )
length(cc)
fol <- paste('TARGET ~', cc[1], sep='' )
for (i in 2:39){fol = paste(fol, cc[i], sep='+' )}
fol
model = glm(fol,data = data_Train2, family = binomial)