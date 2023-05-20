# Simulation (Updated on 23.05.08)
# The causal diagram assumed here is Fig 5C in Park, Tafti, and Shmueli (2023).
# Lines #6-62: Table A2
# Lines #63-205: Table A4

################################################################################
# 1) Simulate the source data from Figure 5C

rm(list=ls())
n <- 1000000
set.seed(5)

u1 <- rnorm(n, 0, 1)
M <- rbinom(n, 1, 0.9)

X_source <- rbinom(n, 1, 0.5)     
Z_source <- rbinom(n, 1, 1/(1+exp(-(M*X_source))))                             
Y_source <- 0.5*Z_source + 0.5*X_source + u1

SimulatedData_source <- data.frame(X_source,Z_source,Y_source)  


# 2) Simulate the target data from Figure 5C
n <- 1000000
set.seed(5)

u1 <- rnorm(n, 0, 1)
M <- rbinom(n, 1, 0.9)
S <- rbinom(n, 1, 0.2)

X_target <- rbinom(n, 1, 1/(1+exp(-(u1))))     
Z_target <- rbinom(n, 1, 1/(1+exp(-(M*X_target))))*S               
Y_target <- 0.5*Z_target + 0.5*X_target + u1

SimulatedData_target <- data.frame(X_target,Z_target,Y_target, S)  

# 3) Ground truth OLS result; Row #3 of Table A2 in the Source part.
# 3-1) P(Y|do(X))= P(Y|X) by the 2nd rule of do-calculus.
E_Y_x1 <- aggregate(Y_source~ X_source, data=SimulatedData_source, mean)[2,2]
E_Y_x0 <- aggregate(Y_source~ X_source, data=SimulatedData_source, mean)[1,2]
ATE <- E_Y_x1-E_Y_x0
print(c(E_Y_x0, E_Y_x1, ATE))

# 4) E(Y|do(X), Z), Conditional expectation; Row #1 and #2 of Table A2 in the source part.
E_Y_x0_z0 <- aggregate(Y_source~ X_source + Z_source, data=SimulatedData_source, mean)[1,3]
E_Y_x1_z0 <- aggregate(Y_source~ X_source + Z_source, data=SimulatedData_source, mean)[2,3]
E_Y_x0_z1 <- aggregate(Y_source~ X_source + Z_source, data=SimulatedData_source, mean)[3,3]
E_Y_x1_z1 <- aggregate(Y_source~ X_source + Z_source, data=SimulatedData_source, mean)[4,3]

# 5) Transportability results; Row #3 of Table A2 in the target part.
# 5-1) P*(Z|X)
P_z0_x0_S <- table(Z_target, X_target)[1,1]/table(X_target)[1]
P_z0_x1_S <- table(Z_target, X_target)[1,2]/table(X_target)[2]
P_z1_x0_S <- table(Z_target, X_target)[2,1]/table(X_target)[1]
P_z1_x1_S <- table(Z_target, X_target)[2,2]/table(X_target)[2]

# 5-2) E*(Y|do(X))
E_Y_X0_S <- E_Y_x0_z0*P_z0_x0_S + E_Y_x0_z1*P_z1_x0_S
E_Y_X1_S <- E_Y_x1_z0*P_z0_x1_S + E_Y_x1_z1*P_z1_x1_S
ATE_transported <- E_Y_X1_S - E_Y_X0_S
print(c(E_Y_X0_S, E_Y_X1_S, ATE_transported))

################################################################################
# 1) Simulate the source data when confounders are observable

rm(list=ls())
n <- 1000000
set.seed(5)

u1 <- rnorm(n, 0, 1)
M <- rbinom(n, 1, 0.9)

W1_source <- rbinom(n, 1, 0.2)
W2_source <- rbinom(n, 1, 0.4)
X_source <- rbinom(n, 1, 0.5)     
Z_source <- rbinom(n, 1, 1/(1+exp(-(M*X_source))))                         
Y_source <- 0.5*Z_source + 0.5*X_source + 0.5*W1_source + 0.5*W2_source + u1

SimulatedData_source <- data.frame(X_source,Z_source,Y_source, W1_source, W2_source)  

# 2) Simulate the source data when confounders are observable

n <- 1000000
set.seed(5)

u1 <- rnorm(n, 0, 1)
M <- rbinom(n, 1, 0.9)
S <- rbinom(n, 1, 0.2)

W1_target <- rbinom(n, 1, 0.2)*S
W2_target <- rbinom(n, 1, 0.4)*S
X_target <- rbinom(n, 1, 1/(1+exp(-(u1+W1_target+W2_target))))     
Z_target <- rbinom(n, 1, 1/(1+exp(-(M*X_target))))*S               
Y_target <- 0.5*Z_target + 0.5*X_target + 0.5*W1_target + 0.5*W2_target + u1

SimulatedData_target <- data.frame(X_target,Z_target,Y_target, W1_target, W2_target)  

# 3) Causal effect of X on Y in the source; Column (3) of Table A4
E_Y_x1 <- aggregate(Y_source~ X_source, data=SimulatedData_source, mean)[2,2]
E_Y_x0 <- aggregate(Y_source~ X_source, data=SimulatedData_source, mean)[1,2]
ATE <- E_Y_x1-E_Y_x0
print(c(E_Y_x0, E_Y_x1, ATE))

# 4) Non causal effect of X and Y in the target; Column (5) of Table A4 
E_Y_x0_w10_w20_S <- aggregate(Y_target~ X_target + W1_target + W2_target, data=SimulatedData_target, mean)[1,4]
E_Y_x1_w10_w20_S <- aggregate(Y_target~ X_target + W1_target + W2_target, data=SimulatedData_target, mean)[2,4]
E_Y_x0_w11_w20_S <- aggregate(Y_target~ X_target + W1_target + W2_target, data=SimulatedData_target, mean)[3,4]
E_Y_x1_w11_w20_S <- aggregate(Y_target~ X_target + W1_target + W2_target, data=SimulatedData_target, mean)[4,4]
E_Y_x0_w10_w21_S <- aggregate(Y_target~ X_target + W1_target + W2_target, data=SimulatedData_target, mean)[5,4]
E_Y_x1_w10_w21_S <- aggregate(Y_target~ X_target + W1_target + W2_target, data=SimulatedData_target, mean)[6,4]
E_Y_x0_w11_w21_S <- aggregate(Y_target~ X_target + W1_target + W2_target, data=SimulatedData_target, mean)[7,4]
E_Y_x1_w11_w21_S <- aggregate(Y_target~ X_target + W1_target + W2_target, data=SimulatedData_target, mean)[8,4]

E_Y_x0_S_biased <- E_Y_x0_w10_w20_S*P_w10_w20_S + E_Y_x0_w11_w20_S*P_w11_w20_S + E_Y_x0_w10_w21_S*P_w10_w21_S + E_Y_x0_w11_w21_S*P_w11_w21_S
E_Y_x1_S_biased <- E_Y_x1_w10_w20_S*P_w10_w20_S + E_Y_x1_w11_w20_S*P_w11_w20_S + E_Y_x1_w10_w21_S*P_w10_w21_S + E_Y_x1_w11_w21_S*P_w11_w21_S
ATE_S_biased <- E_Y_x1_S_biased-E_Y_x0_S_biased
print(c(E_Y_x0_S_biased, E_Y_x1_S_biased, ATE_S_biased))

# 5) Transportability results; Column (6) of Table A4
# 5-1) E(Y|do(X), Z, W)
E_Y_x0_z0_W10_W20 <- aggregate(Y_source~ X_source + Z_source + W1_source + W2_source, data=SimulatedData_source, mean)[1,5]
E_Y_x1_z0_W10_W20 <- aggregate(Y_source~ X_source + Z_source + W1_source + W2_source, data=SimulatedData_source, mean)[2,5]
E_Y_x0_z1_W10_W20 <- aggregate(Y_source~ X_source + Z_source + W1_source + W2_source, data=SimulatedData_source, mean)[3,5]
E_Y_x1_z1_W10_W20 <- aggregate(Y_source~ X_source + Z_source + W1_source + W2_source, data=SimulatedData_source, mean)[4,5]
E_Y_x0_z0_W11_W20 <- aggregate(Y_source~ X_source + Z_source + W1_source + W2_source, data=SimulatedData_source, mean)[5,5]
E_Y_x1_z0_W11_W20 <- aggregate(Y_source~ X_source + Z_source + W1_source + W2_source, data=SimulatedData_source, mean)[6,5]
E_Y_x0_z1_W11_W20 <- aggregate(Y_source~ X_source + Z_source + W1_source + W2_source, data=SimulatedData_source, mean)[7,5]
E_Y_x1_z1_W11_W20 <- aggregate(Y_source~ X_source + Z_source + W1_source + W2_source, data=SimulatedData_source, mean)[8,5]

E_Y_x0_z0_W10_W21 <- aggregate(Y_source~ X_source + Z_source + W1_source + W2_source, data=SimulatedData_source, mean)[9,5]
E_Y_x1_z0_W10_W21 <- aggregate(Y_source~ X_source + Z_source + W1_source + W2_source, data=SimulatedData_source, mean)[10,5]
E_Y_x0_z1_W10_W21 <- aggregate(Y_source~ X_source + Z_source + W1_source + W2_source, data=SimulatedData_source, mean)[11,5]
E_Y_x1_z1_W10_W21 <- aggregate(Y_source~ X_source + Z_source + W1_source + W2_source, data=SimulatedData_source, mean)[12,5]
E_Y_x0_z0_W11_W21 <- aggregate(Y_source~ X_source + Z_source + W1_source + W2_source, data=SimulatedData_source, mean)[13,5]
E_Y_x1_z0_W11_W21 <- aggregate(Y_source~ X_source + Z_source + W1_source + W2_source, data=SimulatedData_source, mean)[14,5]
E_Y_x0_z1_W11_W21 <- aggregate(Y_source~ X_source + Z_source + W1_source + W2_source, data=SimulatedData_source, mean)[15,5]
E_Y_x1_z1_W11_W21 <- aggregate(Y_source~ X_source + Z_source + W1_source + W2_source, data=SimulatedData_source, mean)[16,5]

# 5-2) P*(W)
P_w10_w20_S <- table(W1_target, W2_target)[1,1]/n
P_w10_w21_S <- table(W1_target, W2_target)[1,2]/n
P_w11_w20_S <- table(W1_target, W2_target)[2,1]/n
P_w11_w21_S <- table(W1_target, W2_target)[2,2]/n

# 5-3) P*(Z|X, W)
P_z0_x0_w10_w20_S <- table(Z_target, X_target, W1_target, W2_target)[1,1,1,1]/table(X_target, W1_target, W2_target)[1,1,1]
P_z0_x1_w10_w20_S <- table(Z_target, X_target, W1_target, W2_target)[1,2,1,1]/table(X_target, W1_target, W2_target)[2,1,1]
P_z1_x0_w10_w20_S <- table(Z_target, X_target, W1_target, W2_target)[2,1,1,1]/table(X_target, W1_target, W2_target)[1,1,1]
P_z1_x1_w10_w20_S <- table(Z_target, X_target, W1_target, W2_target)[2,2,1,1]/table(X_target, W1_target, W2_target)[2,1,1]
P_z0_x0_w11_w20_S <- table(Z_target, X_target, W1_target, W2_target)[1,1,2,1]/table(X_target, W1_target, W2_target)[1,2,1]
P_z0_x1_w11_w20_S <- table(Z_target, X_target, W1_target, W2_target)[1,2,2,1]/table(X_target, W1_target, W2_target)[2,2,1]
P_z1_x0_w11_w20_S <- table(Z_target, X_target, W1_target, W2_target)[2,1,2,1]/table(X_target, W1_target, W2_target)[1,2,1]
P_z1_x1_w11_w20_S <- table(Z_target, X_target, W1_target, W2_target)[2,2,2,1]/table(X_target, W1_target, W2_target)[2,2,1]

P_z0_x0_w10_w21_S <- table(Z_target, X_target, W1_target, W2_target)[1,1,1,2]/table(X_target, W1_target, W2_target)[1,1,2]
P_z0_x1_w10_w21_S <- table(Z_target, X_target, W1_target, W2_target)[1,2,1,2]/table(X_target, W1_target, W2_target)[2,1,2]
P_z1_x0_w10_w21_S <- table(Z_target, X_target, W1_target, W2_target)[2,1,1,2]/table(X_target, W1_target, W2_target)[1,1,2]
P_z1_x1_w10_w21_S <- table(Z_target, X_target, W1_target, W2_target)[2,2,1,2]/table(X_target, W1_target, W2_target)[2,1,2]
P_z0_x0_w11_w21_S <- table(Z_target, X_target, W1_target, W2_target)[1,1,2,2]/table(X_target, W1_target, W2_target)[1,2,2]
P_z0_x1_w11_w21_S <- table(Z_target, X_target, W1_target, W2_target)[1,2,2,2]/table(X_target, W1_target, W2_target)[2,2,2]
P_z1_x0_w11_w21_S <- table(Z_target, X_target, W1_target, W2_target)[2,1,2,2]/table(X_target, W1_target, W2_target)[1,2,2]
P_z1_x1_w11_w21_S <- table(Z_target, X_target, W1_target, W2_target)[2,2,2,2]/table(X_target, W1_target, W2_target)[2,2,2]

# 5-4) E*(Y|do(X))
E_Y_X0_S <- E_Y_x0_z0_W10_W20*P_w10_w20_S*P_z0_x0_w10_w20_S + E_Y_x0_z1_W10_W20*P_w10_w20_S*P_z1_x0_w10_w20_S + E_Y_x0_z0_W11_W20*P_w11_w20_S*P_z0_x0_w11_w20_S + E_Y_x0_z1_W11_W20*P_w11_w20_S*P_z1_x0_w11_w20_S +
  E_Y_x0_z0_W10_W21*P_w10_w21_S*P_z0_x0_w10_w21_S + E_Y_x0_z1_W10_W21*P_w10_w21_S*P_z1_x0_w10_w21_S + E_Y_x0_z0_W11_W21*P_w11_w21_S*P_z0_x0_w11_w21_S + E_Y_x0_z1_W11_W21*P_w11_w21_S*P_z1_x0_w11_w21_S

E_Y_X1_S <- E_Y_x1_z0_W10_W20*P_w10_w20_S*P_z0_x1_w10_w20_S + E_Y_x1_z1_W10_W20*P_w10_w20_S*P_z1_x1_w10_w20_S + E_Y_x1_z0_W11_W20*P_w11_w20_S*P_z0_x1_w11_w20_S + E_Y_x1_z1_W11_W20*P_w11_w20_S*P_z1_x1_w11_w20_S +
  E_Y_x1_z0_W10_W21*P_w10_w21_S*P_z0_x1_w10_w21_S + E_Y_x1_z1_W10_W21*P_w10_w21_S*P_z1_x1_w10_w21_S + E_Y_x1_z0_W11_W21*P_w11_w21_S*P_z0_x1_w11_w21_S + E_Y_x1_z1_W11_W21*P_w11_w21_S*P_z1_x1_w11_w21_S

ATE_transported <- E_Y_X1_S - E_Y_X0_S
print(c(E_Y_X0_S, E_Y_X1_S, ATE_transported))

# 6) IPSW estimator; Column (6) of Table A5
# Note: assumption here is that cohort is a random sample of the target population.
#        Thus, we are going to sample 100,000 observations randomly. 
cohortData <- SimulatedData_target[sample(nrow(SimulatedData_target), 100000),]
trialData <- SimulatedData_source

colnames(cohortData) <- c('X','Z','Y','W1','W2')
colnames(trialData) <- c('X','Z','Y','W1','W2')

cohortData$S <- 0
trialData$S <- 1

N <- n   
n <- nrow(trialData)
m <- nrow(cohortData)
cohortData$pw <- m/N
trialData$pw <- 1

# 6-1) Combine trial and cohort (S,Z) to estimate propensity scores
Combined <- rbind(cohortData,trialData)
b <- nrow(Combined)

# 6-2) Estimate selection propensity scores using logistic regression
mylogit <- glm(S ~ W1 + W2 + Z, data = Combined, family = "binomial", weights=(Combined$pw)^(-1))
Combined$p <- predict(mylogit,type = "response")
trialData$p<-Combined$p[which(Combined$S==1)]

# 6-3) Compute the IPSW estimator when S=1
IPSW_mu1 <- (sum(trialData$X*trialData$Y/trialData$p))/(sum(trialData$X/trialData$p))
IPSW_mu0 <- (sum((1-trialData$X)*trialData$Y/trialData$p))/(sum((1-trialData$X)/trialData$p)) 
IPSW_ATE <- IPSW_mu1-IPSW_mu0
print(IPSW_ATE)

