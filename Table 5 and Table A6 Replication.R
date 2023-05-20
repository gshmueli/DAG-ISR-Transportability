# Table 5 and Table A6 Replication (Updated on 23.05.08)
rm(list = ls())
# Lines #11-82: Load the datasets and do preprocessing
# Lines #83-124: Figure 4 & Figure 5E (W is education)
# Lines #125-165: Figure 4 and Figure 5E (W is marital status)
# Lines #166-238: Figure 4 & Figure 5E (W is education and marital status)
# Lines #239-337: Figure 4 & Figure 5E (W is education, marital status, and gender)
# Lines #338-362: Figure 5A, 5B, & 5C
# Lines #363-391: Figure 5D

################################################################################
# 1) Load the necessary datasets
setwd('enter your working directory')
ObsData <- read.csv("./Observational Data_Main.csv")
ExpData <- read.csv("./Experimental Data.csv")

# 2) Create different aggregates of Y variable
# Note: We can create two types of aggregates of Y: 1) DID estimate (Avg_Y_post - Avg_Y_pre) and 2) Y during the experimental period

# 2-1) Individuals' preformance change (Y when the experiment ends - Y when the experiment starts) 
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

# 2-2) Average of Y over the entire time period (at the person level)
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

# 2-3) Replace the existing ExpData and Y with the condensed dataset and the differenced Y
# Note: in our paper, there are three aggregates of Y used. 
#       Readers can select one of the aggregates to replicate the results that you would like to obtain
# 2-3-1) DID estimates; change in workers’ productivity from the pre-experimental period to the experimental period.
# Note: this is to replicate the results in Table 5 in the manuscript
ExpData <- ExpData_AvgY
ExpData$perform1<- ExpData$Y_change  
# 2-3-2) Workers’ productivity during the experimental period
# Note: this is to replicate the results in Table A6 in Appendix 3
ExpData <- ExpData_AvgY
ExpData$perform1<- ExpData$Avg_Y_post
# 2-3-3) Do not replace anything if you are interested in the values in the section "the workers’ productivity during the study"
# Note: this is to replicate the results in Table A6 in Appendix 3

# 3) Create the Z (WFHSelected) in the observational dataset 
# Note: the name of the variables X and Z in the observational data is X=Telecommuting, Z=Randomized_Z, repectively
set.seed(5)
n <- nrow(ObsData)
s <- rnorm(n,0,1)                              
xb <- 6.5*(ObsData$Telecommuting-0.25) +3.5*s 
p <- 1/(1 + exp(-xb))
Randomized_Z <- rbinom(n, 1, prob = p)
ObsData <- cbind(ObsData, Randomized_Z)
summary(ObsData)

# 4) Recategorize the "Education" variable in the observational data
ObsData$Educ_bin <- as.integer(ObsData$Education==2|ObsData$Education==3|ObsData$Education==4)

# 5) Create the variable Z in the experimental dataset
# Note: the name of the variables X and Z in the experimental data is X=Selected, Z=telecommuting, respectively
ExpData$telecommuting <- as.integer(ExpData$expgroup==1)

################################################################################
# Transportability results; Column of (3) of Table 5 and Table A6

# 1) Calculate P(Z|X,W,S)
N_X0_W0 <- table(ObsData$Telecommuting, ObsData$Educ_bin)[1,1]
N_X0_W1 <- table(ObsData$Telecommuting, ObsData$Educ_bin)[1,2]
N_X1_W0 <- table(ObsData$Telecommuting, ObsData$Educ_bin)[2,1]
N_X1_W1 <- table(ObsData$Telecommuting, ObsData$Educ_bin)[2,2]

P_Z0_X0_W0 <- table(ObsData$Telecommuting, ObsData$Educ_bin, ObsData$Randomized_Z)[1,1,1]/N_X0_W0
P_Z0_X0_W1 <- table(ObsData$Telecommuting, ObsData$Educ_bin, ObsData$Randomized_Z)[1,2,1]/N_X0_W1
P_Z0_X1_W0 <- table(ObsData$Telecommuting, ObsData$Educ_bin, ObsData$Randomized_Z)[2,1,1]/N_X1_W0
P_Z0_X1_W1 <- table(ObsData$Telecommuting, ObsData$Educ_bin, ObsData$Randomized_Z)[2,2,1]/N_X1_W1
P_Z1_X0_W0 <- table(ObsData$Telecommuting, ObsData$Educ_bin, ObsData$Randomized_Z)[1,1,2]/N_X0_W0
P_Z1_X0_W1 <- table(ObsData$Telecommuting, ObsData$Educ_bin, ObsData$Randomized_Z)[1,2,2]/N_X0_W1
P_Z1_X1_W0 <- table(ObsData$Telecommuting, ObsData$Educ_bin, ObsData$Randomized_Z)[2,1,2]/N_X1_W0
P_Z1_X1_W1 <- table(ObsData$Telecommuting, ObsData$Educ_bin, ObsData$Randomized_Z)[2,2,2]/N_X1_W1


# 2) Calculate E(Y|do(X), Z, W)
E_Y_X0_Z0_W0 <- aggregate(perform1 ~ Selected + telecommuting + high_educ, data=ExpData, mean)[1,4]
E_Y_X0_Z0_W1 <- aggregate(perform1 ~ Selected + telecommuting + high_educ, data=ExpData, mean)[4,4]
E_Y_X0_Z1_W0 <- 0
E_Y_X0_Z1_W1 <- 0

E_Y_X1_Z0_W0 <- aggregate(perform1 ~ Selected + telecommuting + high_educ, data=ExpData, mean)[2,4]
E_Y_X1_Z0_W1 <- aggregate(perform1 ~ Selected + telecommuting + high_educ, data=ExpData, mean)[5,4]
E_Y_X1_Z1_W0 <- aggregate(perform1 ~ Selected + telecommuting + high_educ, data=ExpData, mean)[3,4]
E_Y_X1_Z1_W1 <- aggregate(perform1 ~ Selected + telecommuting + high_educ, data=ExpData, mean)[6,4]

# 3) Calculate P(W)
P_W0 <- table(ObsData$Educ_bin)[1]/nrow(ObsData)
P_W1 <- table(ObsData$Educ_bin)[2]/nrow(ObsData)

# 4) Calculate E(Y|do(X=0), S) and E(Y|do(X=1), S) 
E_Y_X0_S <- E_Y_X0_Z0_W0*P_Z0_X0_W0*P_W0 + E_Y_X0_Z1_W0*P_Z1_X0_W0*P_W0 + E_Y_X0_Z0_W1*P_Z0_X0_W1*P_W1 + E_Y_X0_Z1_W1*P_Z1_X0_W1*P_W1
E_Y_X1_S <- E_Y_X1_Z0_W0*P_Z0_X1_W0*P_W0 + E_Y_X1_Z1_W0*P_Z1_X1_W0*P_W0 + E_Y_X1_Z0_W1*P_Z0_X1_W1*P_W1 + E_Y_X1_Z1_W1*P_Z1_X1_W1*P_W1

# 5) ATE = E(Y|do(X=1), S) - E(Y|do(X=0), S)
E_Y_X1_S-E_Y_X0_S


################################################################################
# Transportability results; Column of (4) of Table 5 and Table A6

# 1) Calculate P(Z|X,W,S)
N_X0_W0 <- table(ObsData$Telecommuting, ObsData$Married)[1,1]
N_X0_W1 <- table(ObsData$Telecommuting, ObsData$Married)[1,2]
N_X1_W0 <- table(ObsData$Telecommuting, ObsData$Married)[2,1]
N_X1_W1 <- table(ObsData$Telecommuting, ObsData$Married)[2,2]

P_Z0_X0_W0 <- table(ObsData$Telecommuting, ObsData$Married, ObsData$Randomized_Z)[1,1,1]/N_X0_W0
P_Z0_X0_W1 <- table(ObsData$Telecommuting, ObsData$Married, ObsData$Randomized_Z)[1,2,1]/N_X0_W1
P_Z0_X1_W0 <- table(ObsData$Telecommuting, ObsData$Married, ObsData$Randomized_Z)[2,1,1]/N_X1_W0
P_Z0_X1_W1 <- table(ObsData$Telecommuting, ObsData$Married, ObsData$Randomized_Z)[2,2,1]/N_X1_W1
P_Z1_X0_W0 <- table(ObsData$Telecommuting, ObsData$Married, ObsData$Randomized_Z)[1,1,2]/N_X0_W0
P_Z1_X0_W1 <- table(ObsData$Telecommuting, ObsData$Married, ObsData$Randomized_Z)[1,2,2]/N_X0_W1
P_Z1_X1_W0 <- table(ObsData$Telecommuting, ObsData$Married, ObsData$Randomized_Z)[2,1,2]/N_X1_W0
P_Z1_X1_W1 <- table(ObsData$Telecommuting, ObsData$Married, ObsData$Randomized_Z)[2,2,2]/N_X1_W1

# 2) Calculate E(Y|do(X), Z, W)
E_Y_X0_Z0_W0 <- aggregate(perform1 ~ Selected + telecommuting + married, data=ExpData, mean)[1,4]
E_Y_X0_Z0_W1 <- aggregate(perform1 ~ Selected + telecommuting + married, data=ExpData, mean)[4,4]
E_Y_X0_Z1_W0 <- 0
E_Y_X0_Z1_W1 <- 0

E_Y_X1_Z0_W0 <- aggregate(perform1 ~ Selected + telecommuting + married, data=ExpData, mean)[2,4]
E_Y_X1_Z0_W1 <- aggregate(perform1 ~ Selected + telecommuting + married, data=ExpData, mean)[5,4]
E_Y_X1_Z1_W0 <- aggregate(perform1 ~ Selected + telecommuting + married, data=ExpData, mean)[3,4]
E_Y_X1_Z1_W1 <- aggregate(perform1 ~ Selected + telecommuting + married, data=ExpData, mean)[6,4]

# 3) Calculate P(W)
P_W0 <- table(ObsData$Married)[1]/nrow(ObsData)
P_W1 <- table(ObsData$Married)[2]/nrow(ObsData)

# 4) Calculate E(Y|do(X=0), S) and E(Y|do(X=1), S) 
E_Y_X0_S <- E_Y_X0_Z0_W0*P_Z0_X0_W0*P_W0 + E_Y_X0_Z1_W0*P_Z1_X0_W0*P_W0 + E_Y_X0_Z0_W1*P_Z0_X0_W1*P_W1 + E_Y_X0_Z1_W1*P_Z1_X0_W1*P_W1
E_Y_X1_S <- E_Y_X1_Z0_W0*P_Z0_X1_W0*P_W0 + E_Y_X1_Z1_W0*P_Z1_X1_W0*P_W0 + E_Y_X1_Z0_W1*P_Z0_X1_W1*P_W1 + E_Y_X1_Z1_W1*P_Z1_X1_W1*P_W1

# 5) ATE = E(Y|do(X=1), S) - E(Y|do(X=0), S)
E_Y_X1_S-E_Y_X0_S


################################################################################
# Transportability results; Column of (5) of Table 5 and Table A6

# 1) Calculate P(Z|X, W(=Edu and Married), S)
# Denominator part
N_X0_E0_M0 <- table(ObsData$Telecommuting, ObsData$Educ_bin, ObsData$Married)[1,1,1]
N_X0_E0_M1 <- table(ObsData$Telecommuting, ObsData$Educ_bin, ObsData$Married)[1,1,2]
N_X0_E1_M0 <- table(ObsData$Telecommuting, ObsData$Educ_bin, ObsData$Married)[1,2,1]
N_X0_E1_M1 <- table(ObsData$Telecommuting, ObsData$Educ_bin, ObsData$Married)[1,2,2]
N_X1_E1_M0 <- table(ObsData$Telecommuting, ObsData$Educ_bin, ObsData$Married)[2,2,1]
N_X1_E1_M1 <- table(ObsData$Telecommuting, ObsData$Educ_bin, ObsData$Married)[2,2,2]
N_X1_E0_M0 <- table(ObsData$Telecommuting, ObsData$Educ_bin, ObsData$Married)[2,1,1]
N_X1_E0_M1 <- table(ObsData$Telecommuting, ObsData$Educ_bin, ObsData$Married)[2,1,2]

# Numerator part
Randomized_Z_0 <- ObsData[ObsData$Randomized_Z==0,]
Randomized_Z_1 <- ObsData[ObsData$Randomized_Z==1,]

P_Z0_X0_E0_M0 <- table(Randomized_Z_0$Telecommuting, Randomized_Z_0$Educ_bin, Randomized_Z_0$Married)[1,1,1]/N_X0_E0_M0
P_Z0_X0_E0_M1 <- table(Randomized_Z_0$Telecommuting, Randomized_Z_0$Educ_bin, Randomized_Z_0$Married)[1,1,2]/N_X0_E0_M1
P_Z0_X0_E1_M0 <- table(Randomized_Z_0$Telecommuting, Randomized_Z_0$Educ_bin, Randomized_Z_0$Married)[1,2,1]/N_X0_E1_M0
P_Z0_X0_E1_M1 <- table(Randomized_Z_0$Telecommuting, Randomized_Z_0$Educ_bin, Randomized_Z_0$Married)[1,2,2]/N_X0_E1_M1
P_Z0_X1_E0_M0 <- table(Randomized_Z_0$Telecommuting, Randomized_Z_0$Educ_bin, Randomized_Z_0$Married)[2,1,1]/N_X1_E0_M0
P_Z0_X1_E0_M1 <- table(Randomized_Z_0$Telecommuting, Randomized_Z_0$Educ_bin, Randomized_Z_0$Married)[2,1,2]/N_X1_E0_M1
P_Z0_X1_E1_M0 <- table(Randomized_Z_0$Telecommuting, Randomized_Z_0$Educ_bin, Randomized_Z_0$Married)[2,2,1]/N_X1_E1_M0
P_Z0_X1_E1_M1 <- table(Randomized_Z_0$Telecommuting, Randomized_Z_0$Educ_bin, Randomized_Z_0$Married)[2,2,2]/N_X1_E1_M1

P_Z1_X0_E0_M0 <- table(Randomized_Z_1$Telecommuting, Randomized_Z_1$Educ_bin, Randomized_Z_1$Married)[1,1,1]/N_X0_E0_M0
P_Z1_X0_E0_M1 <- table(Randomized_Z_1$Telecommuting, Randomized_Z_1$Educ_bin, Randomized_Z_1$Married)[1,1,2]/N_X0_E0_M1
P_Z1_X0_E1_M0 <- table(Randomized_Z_1$Telecommuting, Randomized_Z_1$Educ_bin, Randomized_Z_1$Married)[1,2,1]/N_X0_E1_M0
P_Z1_X0_E1_M1 <- table(Randomized_Z_1$Telecommuting, Randomized_Z_1$Educ_bin, Randomized_Z_1$Married)[1,2,2]/N_X0_E1_M1
P_Z1_X1_E0_M0 <- table(Randomized_Z_1$Telecommuting, Randomized_Z_1$Educ_bin, Randomized_Z_1$Married)[2,1,1]/N_X1_E0_M0
P_Z1_X1_E0_M1 <- table(Randomized_Z_1$Telecommuting, Randomized_Z_1$Educ_bin, Randomized_Z_1$Married)[2,1,2]/N_X1_E0_M1
P_Z1_X1_E1_M0 <- table(Randomized_Z_1$Telecommuting, Randomized_Z_1$Educ_bin, Randomized_Z_1$Married)[2,2,1]/N_X1_E1_M0
P_Z1_X1_E1_M1 <- table(Randomized_Z_1$Telecommuting, Randomized_Z_1$Educ_bin, Randomized_Z_1$Married)[2,2,2]/N_X1_E1_M1

# 2) Calculate E(Y|do(X), Z, W(Edu and Married))
E_Y_X0_Z0_E0_M0 <- aggregate(perform1 ~ Selected + telecommuting + high_educ + married, data=ExpData, mean)[1,5]
E_Y_X0_Z0_E0_M1 <- aggregate(perform1 ~ Selected + telecommuting + high_educ + married, data=ExpData, mean)[7,5]
E_Y_X0_Z0_E1_M0 <- aggregate(perform1 ~ Selected + telecommuting + high_educ + married, data=ExpData, mean)[4,5]
E_Y_X0_Z0_E1_M1 <- aggregate(perform1 ~ Selected + telecommuting + high_educ + married, data=ExpData, mean)[10,5]
E_Y_X0_Z1_E0_M0 <- 0
E_Y_X0_Z1_E0_M1 <- 0
E_Y_X0_Z1_E1_M0 <- 0
E_Y_X0_Z1_E1_M1 <- 0

E_Y_X1_Z0_E0_M0 <- aggregate(perform1 ~ Selected + telecommuting + high_educ + married, data=ExpData, mean)[2,5]
E_Y_X1_Z0_E0_M1 <- aggregate(perform1 ~ Selected + telecommuting + high_educ + married, data=ExpData, mean)[8,5]
E_Y_X1_Z0_E1_M0 <- aggregate(perform1 ~ Selected + telecommuting + high_educ + married, data=ExpData, mean)[5,5]
E_Y_X1_Z0_E1_M1 <- aggregate(perform1 ~ Selected + telecommuting + high_educ + married, data=ExpData, mean)[11,5]
E_Y_X1_Z1_E0_M0 <- aggregate(perform1 ~ Selected + telecommuting + high_educ + married, data=ExpData, mean)[3,5]
E_Y_X1_Z1_E0_M1 <- aggregate(perform1 ~ Selected + telecommuting + high_educ + married, data=ExpData, mean)[9,5]
E_Y_X1_Z1_E1_M0 <- aggregate(perform1 ~ Selected + telecommuting + high_educ + married, data=ExpData, mean)[6,5]
E_Y_X1_Z1_E1_M1 <- aggregate(perform1 ~ Selected + telecommuting + high_educ + married, data=ExpData, mean)[12,5]

# 3) Calculate P(W)
P_E0_M0 <- table(ObsData$Educ_bin, ObsData$Married)[1,1]/nrow(ObsData)
P_E0_M1 <- table(ObsData$Educ_bin, ObsData$Married)[1,2]/nrow(ObsData)
P_E1_M0 <- table(ObsData$Educ_bin, ObsData$Married)[2,1]/nrow(ObsData)
P_E1_M1 <- table(ObsData$Educ_bin, ObsData$Married)[2,2]/nrow(ObsData)

# 4) Calculate E(Y|do(X=0), S) and E(Y|do(X=1), S) 
E_Y_X0_S <- E_Y_X0_Z0_E0_M0*P_Z0_X0_E0_M0*P_E0_M0 + E_Y_X0_Z0_E0_M1*P_Z0_X0_E0_M1*P_E0_M1 + E_Y_X0_Z0_E1_M0*P_Z0_X0_E1_M0*P_E1_M0 + E_Y_X0_Z0_E1_M1*P_Z0_X0_E1_M1*P_E1_M1+
            E_Y_X0_Z1_E0_M0*P_Z1_X0_E0_M0*P_E0_M0 + E_Y_X0_Z1_E0_M1*P_Z1_X0_E0_M1*P_E0_M1 + E_Y_X0_Z1_E1_M0*P_Z1_X0_E1_M0*P_E1_M0 + E_Y_X0_Z1_E1_M1*P_Z1_X0_E1_M1*P_E1_M1

E_Y_X1_S <- E_Y_X1_Z0_E0_M0*P_Z0_X1_E0_M0*P_E0_M0 + E_Y_X1_Z0_E0_M1*P_Z0_X1_E0_M1*P_E0_M1 + E_Y_X1_Z0_E1_M0*P_Z0_X1_E1_M0*P_E1_M0 + E_Y_X1_Z0_E1_M1*P_Z0_X1_E1_M1*P_E1_M1+
            E_Y_X1_Z1_E0_M0*P_Z1_X1_E0_M0*P_E0_M0 + E_Y_X1_Z1_E0_M1*P_Z1_X1_E0_M1*P_E0_M1 + E_Y_X1_Z1_E1_M0*P_Z1_X1_E1_M0*P_E1_M0 + E_Y_X1_Z1_E1_M1*P_Z1_X1_E1_M1*P_E1_M1


# 5) ATE = P(Y|do(X=1), S) - P(Y|do(X=0), S)
E_Y_X1_S-E_Y_X0_S


################################################################################
# Transportability results; Column of (6) of Table 5 and Table A6
# Note: we used a basic clustering algorithm, K-means, to generate subgroups having similar characteristics

# 1) Using the experimental data, develop the predictive model (k-mean)
training_data <- ExpData[, c('married','high_educ','men')]

# 1-1) K-means
set.seed(5)
ClusteringModel <- kmeans(training_data , centers = 4, iter.max = 100000)   #centers = k
ClusteringModel
ExpData$cluster <- as.factor(ClusteringModel$cluster)

# 2) Using observational data, obtain the predicted values
test_data <- ObsData[,c('Married','Educ_bin','Gender')]
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
# 3-1) Calculate P(Z|X,W,S)
N_X0_W1 <- table(ObsData$Telecommuting, temp$category)[1,1]
N_X0_W2 <- table(ObsData$Telecommuting, temp$category)[1,2]
N_X0_W3 <- table(ObsData$Telecommuting, temp$category)[1,3]
N_X0_W4 <- table(ObsData$Telecommuting, temp$category)[1,4]

N_X1_W1 <- table(ObsData$Telecommuting, temp$category)[2,1]
N_X1_W2 <- table(ObsData$Telecommuting, temp$category)[2,2]
N_X1_W3 <- table(ObsData$Telecommuting, temp$category)[2,3]
N_X1_W4 <- table(ObsData$Telecommuting, temp$category)[2,4]

P_Z0_X0_W1 <- table(ObsData$Telecommuting, temp$category, ObsData$Randomized_Z)[1,1,1]/N_X0_W1
P_Z0_X0_W2 <- table(ObsData$Telecommuting, temp$category, ObsData$Randomized_Z)[1,2,1]/N_X0_W2
P_Z0_X0_W3 <- table(ObsData$Telecommuting, temp$category, ObsData$Randomized_Z)[1,3,1]/N_X0_W3
P_Z0_X0_W4 <- table(ObsData$Telecommuting, temp$category, ObsData$Randomized_Z)[1,4,1]/N_X0_W4

P_Z0_X1_W1 <- table(ObsData$Telecommuting, temp$category, ObsData$Randomized_Z)[2,1,1]/N_X1_W1
P_Z0_X1_W2 <- table(ObsData$Telecommuting, temp$category, ObsData$Randomized_Z)[2,2,1]/N_X1_W2
P_Z0_X1_W3 <- table(ObsData$Telecommuting, temp$category, ObsData$Randomized_Z)[2,3,1]/N_X1_W3
P_Z0_X1_W4 <- table(ObsData$Telecommuting, temp$category, ObsData$Randomized_Z)[2,4,1]/N_X1_W4

P_Z1_X0_W1 <- table(ObsData$Telecommuting, temp$category, ObsData$Randomized_Z)[1,1,2]/N_X0_W1
P_Z1_X0_W2 <- table(ObsData$Telecommuting, temp$category, ObsData$Randomized_Z)[1,2,2]/N_X0_W2
P_Z1_X0_W3 <- table(ObsData$Telecommuting, temp$category, ObsData$Randomized_Z)[1,3,2]/N_X0_W3
P_Z1_X0_W4 <- table(ObsData$Telecommuting, temp$category, ObsData$Randomized_Z)[1,4,2]/N_X0_W4

P_Z1_X1_W1 <- table(ObsData$Telecommuting, temp$category, ObsData$Randomized_Z)[2,1,2]/N_X1_W1
P_Z1_X1_W2 <- table(ObsData$Telecommuting, temp$category, ObsData$Randomized_Z)[2,2,2]/N_X1_W2
P_Z1_X1_W3 <- table(ObsData$Telecommuting, temp$category, ObsData$Randomized_Z)[2,3,2]/N_X1_W3
P_Z1_X1_W4 <- table(ObsData$Telecommuting, temp$category, ObsData$Randomized_Z)[2,4,2]/N_X1_W4

# 3-2) Calculate E(Y|do(X), Z, W)
E_Y_X0_Z0_W1 <- aggregate(perform1 ~ Selected + telecommuting + cluster, data=ExpData, mean)[1,4]
E_Y_X0_Z0_W2 <- aggregate(perform1 ~ Selected + telecommuting + cluster, data=ExpData, mean)[4,4]
E_Y_X0_Z0_W3 <- aggregate(perform1 ~ Selected + telecommuting + cluster, data=ExpData, mean)[7,4]
E_Y_X0_Z0_W4 <- aggregate(perform1 ~ Selected + telecommuting + cluster, data=ExpData, mean)[10,4]

E_Y_X0_Z1_W1 <- 0
E_Y_X0_Z1_W2 <- 0
E_Y_X0_Z1_W3 <- 0
E_Y_X0_Z1_W4 <- 0

E_Y_X1_Z0_W1 <- aggregate(perform1 ~ Selected + telecommuting + cluster, data=ExpData, mean)[2,4]
E_Y_X1_Z0_W2 <- aggregate(perform1 ~ Selected + telecommuting + cluster, data=ExpData, mean)[5,4]
E_Y_X1_Z0_W3 <- aggregate(perform1 ~ Selected + telecommuting + cluster, data=ExpData, mean)[8,4]
E_Y_X1_Z0_W4 <- aggregate(perform1 ~ Selected + telecommuting + cluster, data=ExpData, mean)[11,4]

E_Y_X1_Z1_W1 <- aggregate(perform1 ~ Selected + telecommuting + cluster, data=ExpData, mean)[3,4]
E_Y_X1_Z1_W2 <- aggregate(perform1 ~ Selected + telecommuting + cluster, data=ExpData, mean)[6,4]
E_Y_X1_Z1_W3 <- aggregate(perform1 ~ Selected + telecommuting + cluster, data=ExpData, mean)[9,4]
E_Y_X1_Z1_W4 <- aggregate(perform1 ~ Selected + telecommuting + cluster, data=ExpData, mean)[12,4]

# 3-3) Calculate P(W)
P_W1 <- table(temp$category)[1]/nrow(temp)
P_W2 <- table(temp$category)[2]/nrow(temp)
P_W3 <- table(temp$category)[3]/nrow(temp)
P_W4 <- table(temp$category)[4]/nrow(temp)

# 3-4) Calculate P(Y|do(X=0), S) and P(Y|do(X=1), S) 

E_Y_X0_S <- E_Y_X0_Z0_W1*P_Z0_X0_W1*P_W1 + E_Y_X0_Z0_W2*P_Z0_X0_W2*P_W2 + E_Y_X0_Z0_W3*P_Z0_X0_W3*P_W3 + E_Y_X0_Z0_W4*P_Z0_X0_W4*P_W4 +
            E_Y_X0_Z1_W1*P_Z1_X0_W1*P_W1 + E_Y_X0_Z1_W2*P_Z1_X0_W2*P_W2 + E_Y_X0_Z1_W3*P_Z1_X0_W3*P_W3 + E_Y_X0_Z1_W4*P_Z1_X0_W4*P_W4 

E_Y_X1_S <- E_Y_X1_Z0_W1*P_Z0_X1_W1*P_W1 + E_Y_X1_Z0_W2*P_Z0_X1_W2*P_W2 + E_Y_X1_Z0_W3*P_Z0_X1_W3*P_W3 + E_Y_X1_Z0_W4*P_Z0_X1_W4*P_W4 +
            E_Y_X1_Z1_W1*P_Z1_X1_W1*P_W1 + E_Y_X1_Z1_W2*P_Z1_X1_W2*P_W2 + E_Y_X1_Z1_W3*P_Z1_X1_W3*P_W3 + E_Y_X1_Z1_W4*P_Z1_X1_W4*P_W4 


# 3-5) ATE = P(Y|do(X=1), S) - P(Y|do(X=0), S)
E_Y_X1_S-E_Y_X0_S



################################################################################
# Transportability results; Column of (2) of Table 5 and Table A6

# 1) Calculate P(Y|do(X), Z)
E_Y_X0_Z0 <- aggregate(perform1 ~ Selected + telecommuting, data=ExpData, mean)[1,3]
E_Y_X0_Z1 <- 0
E_Y_X1_Z0 <- aggregate(perform1 ~ Selected + telecommuting, data=ExpData, mean)[2,3]
E_Y_X1_Z1 <- aggregate(perform1 ~ Selected + telecommuting, data=ExpData, mean)[3,3]

# 2) Calculate P(Z|X,S)
X0 <- table(ObsData$Telecommuting)[1]
X1 <- table(ObsData$Telecommuting)[2]

P_Z0_X0 <- table(ObsData$Telecommuting, ObsData$Randomized_Z)[1,1]/X0
P_Z1_X0 <- table(ObsData$Telecommuting, ObsData$Randomized_Z)[1,2]/X0
P_Z0_X1 <- table(ObsData$Telecommuting, ObsData$Randomized_Z)[2,1]/X1
P_Z1_X1 <- table(ObsData$Telecommuting, ObsData$Randomized_Z)[2,2]/X1

# 3) Calculate P(Y|do(X=0), S) and P(Y|do(X=1), S) 
E_Y_X0_S <- E_Y_X0_Z0*P_Z0_X0 + E_Y_X0_Z1 * P_Z1_X0
E_Y_X1_S <- E_Y_X1_Z0*P_Z0_X1 + E_Y_X1_Z1*P_Z1_X1

# 4) ATE = P(Y|do(X=1), S) - P(Y|do(X=0), S)
E_Y_X1_S-E_Y_X0_S

################################################################################
# Transportability results; Column of (2) of Table 5 and Table A6

# 1) Calculate P(Z|X,S)
X0 <- table(ObsData$Telecommuting)[1]
X1 <- table(ObsData$Telecommuting)[2]

P_X0 <- X0/sum(X0,X1)
P_X1 <- X1/sum(X0,X1)

P_Z0_X0 <- table(ObsData$Telecommuting, ObsData$Randomized_Z)[1,1]/X0
P_Z1_X0 <- table(ObsData$Telecommuting, ObsData$Randomized_Z)[1,2]/X0
P_Z0_X1 <- table(ObsData$Telecommuting, ObsData$Randomized_Z)[2,1]/X1
P_Z1_X1 <- table(ObsData$Telecommuting, ObsData$Randomized_Z)[2,2]/X1

# 2) Calculate E(Y|X', Z, S)

E_Y_X0_Z0 <- aggregate(TUACTDUR ~ Telecommuting + Randomized_Z, data=ObsData, mean)[1,3]
E_Y_X1_Z0 <- aggregate(TUACTDUR ~ Telecommuting + Randomized_Z, data=ObsData, mean)[2,3]
E_Y_X0_Z1 <- aggregate(TUACTDUR ~ Telecommuting + Randomized_Z, data=ObsData, mean)[3,3]
E_Y_X1_Z1 <- aggregate(TUACTDUR ~ Telecommuting + Randomized_Z, data=ObsData, mean)[4,3]
# if prefer to use the standardized values, use: scale(TUACTDUR, center = TRUE, scale = TRUE)

# 3) Calculate E(Y|do(X=0), S) and E(Y|do(X=1), S) 
E_Y_X0_S <- P_Z0_X0*(E_Y_X0_Z0*P_X0+E_Y_X1_Z0*P_X1) + P_Z1_X0*(E_Y_X0_Z1*P_X0+E_Y_X1_Z1*P_X1)
E_Y_X1_S <- P_Z0_X1*(E_Y_X0_Z0*P_X0+E_Y_X1_Z0*P_X1) + P_Z1_X1*(E_Y_X0_Z1*P_X0+E_Y_X1_Z1*P_X1)

# 4) ATE = P(Y|do(X=1), S) - P(Y|do(X=0), S)
E_Y_X1_S-E_Y_X0_S
