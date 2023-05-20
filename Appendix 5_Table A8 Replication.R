# Appendix 5; Table A8 Replication (Updated on 23.05.08).
rm(list = ls())
options(scipen = 999)   
# Lines #10-123: Load the datasets and do preprocessing
# Lines #124-143: Figure 3 (W is education)
# Lines #144-163: Figure 3 (W is marital status)
# LInes #164-190: Figure 3 (W is education and marital stauts)
# Lines #191-241: Figure 3 (W is education, marital status, and gender)

#####################################################################################
# 1) Load the necessary datasets
setwd('enter your working directory')
ObsData <- read.csv("./Observational Data_Main.csv")
ExpData <- read.csv("./Experimental Data.csv")
LeaveModule <- read.csv("./Observational Data_Alternative.csv")

# 2) Combine ObsData and LeaveModule datasets based on the identifier 'TUCASEID'
TeleChoice <- merge(ObsData, LeaveModule, by = "TUCASEID")

# 3) Create X (whether subjects prefer to do telework or not) in the observational dataset
Temp <- list()
for(i in 1:nrow(TeleChoice)){
  if (TeleChoice$LEJF_11[i]==1 & TeleChoice$LEJF_12[i]==1 & TeleChoice$LEJF_14[i]==1 & TeleChoice$LEJF_13[i]==3){
    Temp[i] <- 1
  }
  else if (TeleChoice$LEJF_11[i]==1 & TeleChoice$LEJF_12[i]==1 & TeleChoice$LEJF_14[i]==1 & TeleChoice$LEJF_13[i]==4) {
    Temp[i] <- 1
  }
  else if (TeleChoice$LEJF_11[i]==1 & TeleChoice$LEJF_12[i]==1 & TeleChoice$LEJF_14[i]==1 & TeleChoice$LEJF_13[i]==5) {
    Temp[i] <- 1
  }
  else if ( TeleChoice$LEJF_11[i]==1 & TeleChoice$LEJF_12[i]==1 & TeleChoice$LEJF_14[i]==1 & TeleChoice$LEJF_13[i]==2) {
    Temp[i] <- 0
  }
  else if (TeleChoice$LEJF_11[i]==2) {
    Temp[i] <- 0
  }
  else{
    Temp[i] <- 2
        }
}

TeleChoice$X_selfSelected <- unlist(Temp)

# 4) Create Z (Whether they actually work remote or not) in the observational dataset
Temp <- list()
for(i in 1:nrow(TeleChoice)){
  if (TeleChoice$LEJF_11[i]==1 & TeleChoice$LEJF_12[i]==1 & TeleChoice$LEJF_14[i]==1 & TeleChoice$LEJF_13[i]==3){
    Temp[i] <- 1
  }
  else if (TeleChoice$LEJF_11[i]==1 & TeleChoice$LEJF_12[i]==1 & TeleChoice$LEJF_14[i]==1 & TeleChoice$LEJF_13[i]==4) {
    Temp[i] <- 1
  }
  else if (TeleChoice$LEJF_11[i]==1 & TeleChoice$LEJF_12[i]==1 & TeleChoice$LEJF_14[i]==1 & TeleChoice$LEJF_13[i]==5) {
    Temp[i] <- 1
  }
  else if ( TeleChoice$LEJF_11[i]==1 & TeleChoice$LEJF_12[i]==1 & TeleChoice$LEJF_14[i]==1 & TeleChoice$LEJF_13[i]==2) {
    Temp[i] <- 1
  }
  else if (TeleChoice$LEJF_11[i]==2) {
    Temp[i] <- 0
  }
  else{
    Temp[i] <- 2
  }
}
TeleChoice$Z_Actual <- unlist(Temp)

# 4) Keep neccessary columns and rows only
TeleChoice_sub <- TeleChoice[TeleChoice$X_selfSelected != 2, ] 
keeps <- c("TUCASEID","TUACTDUR", "TEAGE","Education","Race","NumChild","Married","Gender","Fulltime","X_selfSelected","Z_Actual")
TeleChoice_sub <- TeleChoice_sub[keeps]

# 5) Create different aggregates of Y variable
# Note: We can create two types of aggregates of Y: 1) DID estimate (Avg_Y_post - Avg_Y_pre) and 2) Y during the experimental period
# 5-1) Individuals' preformance change (Y when the experiment ends - Y when the experiment starts) 
library(dplyr) 
AvgY_PreTreatment <- ExpData %>%
  group_by(personid) %>%
  filter(year_week < 201049)  %>%
  summarise(Avg_Y = mean(perform1, na.rm = T))              

AvgY_PreTreatment <- rename(AvgY_PreTreatment,Avg_Y_pre=Avg_Y )

AvgY_PostTreatment <- ExpData %>%
  group_by(personid) %>%
  filter(year_week > 201049)  %>%
  summarise(Avg_Y = mean(perform1, na.rm = T))              

AvgY_PostTreatment<-rename(AvgY_PostTreatment,Avg_Y_post=Avg_Y )

# 5-2) Average of Y over the entire time period (at the person level)
Y_LastRecord <- ExpData %>%
  group_by(personid) %>%
  filter(row_number() == n())   

AvgY_EntirePeriod <- ExpData %>%
  group_by(personid) %>%
  summarise(Avg_Y = mean(perform1, na.rm = T))         

ExpData_AvgY <- inner_join(Y_LastRecord, AvgY_EntirePeriod,  by='personid')
ExpData_AvgY <- inner_join(ExpData_AvgY, AvgY_PreTreatment,  by='personid')
ExpData_AvgY <- inner_join(ExpData_AvgY, AvgY_PostTreatment,  by='personid')

ExpData_AvgY$Y_change <- ExpData_AvgY$Avg_Y_post -  ExpData_AvgY$Avg_Y_pre

# 5-3) Replace the existing ExpData and Y with the condensed dataset and the differenced Y
# Note: in our paper, there are three aggregates of Y used. 
#       Readers can select one of the aggregates to replicate the results that you would like to obtain
# 5-3-1) DID estimates; change in workers’ productivity from the pre-experimental period to the experimental period.
ExpData <- ExpData_AvgY
ExpData$perform1<- ExpData$Y_change  
# 5-3-2) Workers’ productivity during the experimental period
ExpData <- ExpData_AvgY
ExpData$perform1<- ExpData$Avg_Y_post
# 5-3-3) Do not replace anything if you are interested in the values in the section "the workers’ productivity during the study"

# 6) Recategorize the "education" variable in the observational data
TeleChoice_sub$Educ_bin <- as.integer(TeleChoice_sub$Education==2|TeleChoice_sub$Education==3|TeleChoice_sub$Education==4)

# 7) Create the variable Z in the experimental dataset
ExpData$telecommuting <- as.integer(ExpData$expgroup==1)

#####################################################################################
# Transportability results; Column (2) of Table A8

# 1) Calculate P*(W)
P_w0 <- table(TeleChoice_sub$Educ_bin)[1]/nrow(TeleChoice_sub)
P_w1 <- table(TeleChoice_sub$Educ_bin)[2]/nrow(TeleChoice_sub)

# 2) Calculate E(Y|do(X), W)
E_Y_X0_W0 <- aggregate(perform1 ~ telecommuting + high_educ, data=ExpData, mean)[1,3]
E_Y_X0_W1 <- aggregate(perform1 ~ telecommuting + high_educ, data=ExpData, mean)[3,3]
E_Y_X1_W0 <- aggregate(perform1 ~ telecommuting + high_educ, data=ExpData, mean)[2,3]
E_Y_X1_W1 <- aggregate(perform1 ~ telecommuting + high_educ, data=ExpData, mean)[4,3]

# 3) Calculate E(Y|do(X=0), S) and E(Y|do(X=1), S) 
E_Y_X0_S <- E_Y_X0_W0*P_w0 + E_Y_X0_W1*P_w1
E_Y_X1_S <- E_Y_X1_W0*P_w0 + E_Y_X1_W1*P_w1

# 4) ATE = E(Y|do(X=1), S) - E(Y|do(X=0), S)
E_Y_X1_S-E_Y_X0_S

#####################################################################################
# Transportability results; Column (3) of Table A8

# 1) Calculate P(W)
P_W0 <- table(TeleChoice_sub$Married)[1]/nrow(TeleChoice_sub)
P_W1 <- table(TeleChoice_sub$Married)[2]/nrow(TeleChoice_sub)

# 2) Calculate E(Y|do(X), W)
E_Y_X0_W0 <- aggregate(perform1 ~ telecommuting + married, data=ExpData, mean)[1,3]
E_Y_X0_W1 <- aggregate(perform1 ~ telecommuting + married, data=ExpData, mean)[3,3]
E_Y_X1_W0 <- aggregate(perform1 ~ telecommuting + married, data=ExpData, mean)[2,3]
E_Y_X1_W1 <- aggregate(perform1 ~ telecommuting + married, data=ExpData, mean)[4,3]

# 3) Calculate E(Y|do(X=0), S) and E(Y|do(X=1), S) 
E_Y_X0_S <- E_Y_X0_W0*P_w0 + E_Y_X0_W1*P_w1
E_Y_X1_S <- E_Y_X1_W0*P_w0 + E_Y_X1_W1*P_w1

# 4) ATE = E(Y|do(X=1), S) - E(Y|do(X=0), S)
E_Y_X1_S-E_Y_X0_S

#####################################################################################
# Transportability results; Column (4) of Table A8

# 1) Calculate P(W)
P_E0_M0 <- table(TeleChoice_sub$Educ_bin, TeleChoice_sub$Married)[1,1]/nrow(TeleChoice_sub)
P_E0_M1 <- table(TeleChoice_sub$Educ_bin, TeleChoice_sub$Married)[1,2]/nrow(TeleChoice_sub)
P_E1_M0 <- table(TeleChoice_sub$Educ_bin, TeleChoice_sub$Married)[2,1]/nrow(TeleChoice_sub)
P_E1_M1 <- table(TeleChoice_sub$Educ_bin, TeleChoice_sub$Married)[2,2]/nrow(TeleChoice_sub)

# 2) Calculate E(Y|do(X), W(=Edu and Married))
E_Y_X0_E0_M0 <- aggregate(perform1 ~ telecommuting + high_educ + married, data=ExpData, mean)[1,4]
E_Y_X0_E0_M1 <- aggregate(perform1 ~ telecommuting + high_educ + married, data=ExpData, mean)[5,4]
E_Y_X0_E1_M0 <- aggregate(perform1 ~ telecommuting + high_educ + married, data=ExpData, mean)[3,4]
E_Y_X0_E1_M1 <- aggregate(perform1 ~ telecommuting + high_educ + married, data=ExpData, mean)[7,4]

E_Y_X1_E0_M0 <- aggregate(perform1 ~ telecommuting + high_educ + married, data=ExpData, mean)[2,4]
E_Y_X1_E0_M1 <- aggregate(perform1 ~ telecommuting + high_educ + married, data=ExpData, mean)[6,4]
E_Y_X1_E1_M0 <- aggregate(perform1 ~ telecommuting + high_educ + married, data=ExpData, mean)[4,4]
E_Y_X1_E1_M1 <- aggregate(perform1 ~ telecommuting + high_educ + married, data=ExpData, mean)[8,4]

# 3) Calculate E(Y|do(X=0), S) and E(Y|do(X=1), S) 
E_Y_X0_S <- E_Y_X0_E0_M0*P_E0_M0 + E_Y_X0_E0_M1*P_E0_M1 + E_Y_X0_E1_M0*P_E1_M0 + E_Y_X0_E1_M1*P_E1_M1
E_Y_X1_S <- E_Y_X1_E0_M0*P_E0_M0 + E_Y_X1_E0_M1*P_E0_M1 + E_Y_X1_E1_M0*P_E1_M0 + E_Y_X1_E1_M1*P_E1_M1

# 4) ATE = E(Y|do(X=1), S) - E(Y|do(X=0), S)
E_Y_X1_S-E_Y_X0_S

#####################################################################################
# Transportability results; Column (5) of Table A8

# 1) Using the experimental data, develop the predictive model (k-means)
training_data <- ExpData[, c('married','high_educ','men')]

# 1-1) K-means
set.seed(5)
ClusteringModel <- kmeans(training_data , centers = 4, iter.max = 100000)  
ClusteringModel
ExpData$cluster <- as.factor(ClusteringModel$cluster)

# 2) Using observational data, obtain the predicted values
test_data <- TeleChoice_sub[,c('Married','Educ_bin','Gender')]
colnames(test_data)<- c('married','high_educ','men')

centers <- ClusteringModel$centers
n_centers <- nrow(centers)
temp <- test_data
for(i in 1:nrow(test_data)){
  dist_mat <- as.matrix(dist(rbind(centers, test_data[i,])))
  dist_mat <- as.matrix(dist_mat[-seq(n_centers), seq(n_centers)])
  temp$category[i] <- which.min(dist_mat)
}

# 3) Calculate ATE
# Note that W is observable, and it has 4-level (due to the clustering)

# 3-1) Calculate P(W)
P_W1 <- table(temp$category)[1]/nrow(temp)
P_W2 <- table(temp$category)[2]/nrow(temp)
P_W3 <- table(temp$category)[3]/nrow(temp)
P_W4 <- table(temp$category)[4]/nrow(temp)

# 3-2) Calculate E(Y|do(X), W)
E_Y_X0_W1 <- aggregate(perform1 ~ telecommuting + cluster, data=ExpData, mean)[1,3]
E_Y_X0_W2 <- aggregate(perform1 ~ telecommuting + cluster, data=ExpData, mean)[3,3]
E_Y_X0_W3 <- aggregate(perform1 ~ telecommuting + cluster, data=ExpData, mean)[5,3]
E_Y_X0_W4 <- aggregate(perform1 ~ telecommuting + cluster, data=ExpData, mean)[7,3]

E_Y_X1_W1 <- aggregate(perform1 ~ telecommuting + cluster, data=ExpData, mean)[2,3]
E_Y_X1_W2 <- aggregate(perform1 ~ telecommuting + cluster, data=ExpData, mean)[4,3]
E_Y_X1_W3 <- aggregate(perform1 ~ telecommuting + cluster, data=ExpData, mean)[6,3]
E_Y_X1_W4 <- aggregate(perform1 ~ telecommuting + cluster, data=ExpData, mean)[8,3]

# 3-3) Calculate E(Y|do(X=0), S) and E(Y|do(X=1), S) 
E_Y_X0_S <- E_Y_X0_W1*P_W1 + E_Y_X0_W2*P_W2 + E_Y_X0_W3*P_W3 + E_Y_X0_W4*P_W4
E_Y_X1_S <- E_Y_X1_W1*P_W1 + E_Y_X1_W2*P_W2 + E_Y_X1_W3*P_W3 + E_Y_X1_W4*P_W4

# 3-4) ATE = E(Y|do(X=1), S) - E(Y|do(X=0), S)
E_Y_X1_S-E_Y_X0_S

