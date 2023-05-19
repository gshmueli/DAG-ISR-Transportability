# Simulation; Replicating the results in Table A5 in Appendix
# Line 9-119: Figure 4
# Line 120-201:Figure 5A
# Line 202-283: Figure 5B
# Line 284-367: Figure 5C
# Line 368-458: Figure 5D
# Line 459-566: Figure 5E

#################################################################################
# 1) Simulate the source ("experimental") data from Figure 4; as such, influences to X are cut off by randomization. 

rm(list=ls())
n <- 1000000
set.seed(5)
u1 <- rnorm(n, 0, 1)
u2 <- rnorm(n, 0, 1)

W_source <- rbinom(n, 1, 1/(1+exp(-u1)))
X_source <- rbinom(n, 1, 0.5)     
Z_source <- rbinom(n, 1, 1/(1+exp(-(-1*X_source))))                             
Y_source <- 0.5*Z_source + 0.5*X_source + 0.5*W_source + u2

SimulatedData_source <- data.frame(X_source,Z_source,Y_source,W_source, u1, u2)  

# 2) Simulate the target data from Figure 4
set.seed(5)
u1 <- rnorm(n, 0, 1)
u2 <- rnorm(n, 0, 1)
S <- rnorm(n,0,2)

W_target <- rbinom(n, 1, 1/(1+exp(-u1+S)))
X_target <- rbinom(n, 1, 1/(1+exp(-(W_target + u1 + u2 + S))))     
Z_target <- rbinom(n, 1, 1/(1+exp(-(-1*X_target + S))))                             
Y_target <- 0.5*Z_target + 0.5*X_target + 0.5*W_target + u2    

SimulatedData_target <- data.frame(X_target,Z_target,Y_target,W_target, u1, u2)

# 3) Ground truth OLS result; column (3) of Table A5 
model_ref <- lm(Y_target ~ X_target + W_target + u1 + u2, data = SimulatedData_target)
summary(model_ref)

# 4) Transportability result; column (4) of Table A5 
# 4-1) P*(W)
P_w0 <- table(W_target)[1]/n
P_w1 <- table(W_target)[2]/n

# 4-2) P*(Z|W, X)
P_z0_w0_x0 <- table(Z_target, W_target, X_target)[1,1,1]/table(W_target, X_target)[1,1]
P_z0_w1_x0 <- table(Z_target, W_target, X_target)[1,2,1]/table(W_target, X_target)[2,1]
P_z1_w0_x0 <- table(Z_target, W_target, X_target)[2,1,1]/table(W_target, X_target)[1,1]
P_z1_w1_x0 <- table(Z_target, W_target, X_target)[2,2,1]/table(W_target, X_target)[2,1]

P_z0_w0_x1 <- table(Z_target, W_target, X_target)[1,1,2]/table(W_target, X_target)[1,2]
P_z0_w1_x1 <- table(Z_target, W_target, X_target)[1,2,2]/table(W_target, X_target)[2,2]
P_z1_w0_x1 <- table(Z_target, W_target, X_target)[2,1,2]/table(W_target, X_target)[1,2]
P_z1_w1_x1 <- table(Z_target, W_target, X_target)[2,2,2]/table(W_target, X_target)[2,2]

# 4-3) P(Y|do(X), W, Z)
P_y_x0_w0_z0 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[1,4]
P_y_x0_w1_z0 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[3,4]
P_y_x0_w0_z1 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[5,4]
P_y_x0_w1_z1 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[7,4]

P_y_x1_w0_z0 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[2,4]
P_y_x1_w1_z0 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[4,4]
P_y_x1_w0_z1 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[6,4]
P_y_x1_w1_z1 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[8,4]

# 4-4) Calculate the transported causal estimates. 
E_Y_x0 <- P_y_x0_w0_z0*P_z0_w0_x0*P_w0 + P_y_x0_w1_z0*P_z0_w1_x0*P_w1 + P_y_x0_w0_z1*P_z1_w0_x0*P_w0 + P_y_x0_w1_z1*P_z1_w1_x0*P_w1
E_Y_x1 <- P_y_x1_w0_z0*P_z0_w0_x1*P_w0 + P_y_x1_w1_z0*P_z0_w1_x1*P_w1 + P_y_x1_w0_z1*P_z1_w0_x1*P_w0 + P_y_x1_w1_z1*P_z1_w1_x1*P_w1
ATE_transported <- E_Y_x1-E_Y_x0
print(c(E_Y_x0, E_Y_x1, ATE_transported))

# 5) When the placebo intervention is given.
Y_source <- 0*Z_source + 0*X_source + 0.5*W_source + u2
SimulatedData_source <- data.frame(X_source,Z_source,Y_source,W_source, u1, u2)  

Y_target <- 0*Z_target + 0*X_target + 0.5*W_target + u2    
SimulatedData_target <- data.frame(X_target,Z_target,Y_target,W_target, u1, u2)

# 5-1) Ground truth OLS result; column (1) of Table A5 
model_placebo_ref <- lm(Y_target ~ X_target + W_target + u1 + u2, data = SimulatedData_target)
summary(model_placebo_ref)

# 5-2) Transportability result; column (2) of Table A5 
# 5-2-1) P*(W)
P_w0 <- table(W_target)[1]/n
P_w1 <- table(W_target)[2]/n

# 5-2-2) P*(Z|W, X)
P_z0_w0_x0 <- table(Z_target, W_target, X_target)[1,1,1]/table(W_target, X_target)[1,1]
P_z0_w1_x0 <- table(Z_target, W_target, X_target)[1,2,1]/table(W_target, X_target)[2,1]
P_z1_w0_x0 <- table(Z_target, W_target, X_target)[2,1,1]/table(W_target, X_target)[1,1]
P_z1_w1_x0 <- table(Z_target, W_target, X_target)[2,2,1]/table(W_target, X_target)[2,1]

P_z0_w0_x1 <- table(Z_target, W_target, X_target)[1,1,2]/table(W_target, X_target)[1,2]
P_z0_w1_x1 <- table(Z_target, W_target, X_target)[1,2,2]/table(W_target, X_target)[2,2]
P_z1_w0_x1 <- table(Z_target, W_target, X_target)[2,1,2]/table(W_target, X_target)[1,2]
P_z1_w1_x1 <- table(Z_target, W_target, X_target)[2,2,2]/table(W_target, X_target)[2,2]

# 5-2-3) P(Y|do(X), W, Z)
P_y_x0_w0_z0 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[1,4]
P_y_x0_w1_z0 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[3,4]
P_y_x0_w0_z1 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[5,4]
P_y_x0_w1_z1 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[7,4]

P_y_x1_w0_z0 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[2,4]
P_y_x1_w1_z0 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[4,4]
P_y_x1_w0_z1 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[6,4]
P_y_x1_w1_z1 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[8,4]

# 5-2-4) Calculate the transported causal estimates. 
E_Y_x0 <- P_y_x0_w0_z0*P_z0_w0_x0*P_w0 + P_y_x0_w1_z0*P_z0_w1_x0*P_w1 + P_y_x0_w0_z1*P_z1_w0_x0*P_w0 + P_y_x0_w1_z1*P_z1_w1_x0*P_w1
E_Y_x1 <- P_y_x1_w0_z0*P_z0_w0_x1*P_w0 + P_y_x1_w1_z0*P_z0_w1_x1*P_w1 + P_y_x1_w0_z1*P_z1_w0_x1*P_w0 + P_y_x1_w1_z1*P_z1_w1_x1*P_w1
ATE_transported_placebo <- E_Y_x1-E_Y_x0
print(c(E_Y_x0, E_Y_x1, ATE_transported_placebo))

#################################################################################
# 1)  Simulate the source data from Figure 5A

rm(list=ls())
n <- 1000000
set.seed(5)
u1 <- rnorm(n, 0, 1)
u2 <- rnorm(n, 0, 1)

U_source <- rbinom(n, 1, 1/(1+exp(-u1)))
X_source <- rbinom(n, 1, 0.5)     
Z_source <- rbinom(n, 1, 1/(1+exp(-(-1*X_source))))                             
Y_source <- 0.5*Z_source + 0.5*X_source + 0.5*U_source + u2

SimulatedData_source <- data.frame(X_source,Z_source,Y_source,U_source, u1, u2)  

# 2) Simulate the target data from Figure 5A
set.seed(5)
u1 <- rnorm(n, 0, 1)
u2 <- rnorm(n, 0, 1)
S <- rnorm(n,0,2)

U_target <- rbinom(n, 1, 1/(1+exp(-u1)))
X_target <- rbinom(n, 1, 1/(1+exp(-(U_target + u1 + u2 + S))))     
Z_target <- rbinom(n, 1, 1/(1+exp(-(-1*X_target))))                             
Y_target <- 0.5*Z_target + 0.5*X_target + 0.5*U_target + u2    

SimulatedData_target <- data.frame(X_target,Z_target,Y_target,U_target, u1, u2)  

# 3) Ground truth OLS result; column (3) of Table A5 
model_ref <- lm(Y_target ~ X_target + U_target + u1 + u2, data = SimulatedData_target)
summary(model_ref)

# 4) Transportability result; column (4) of Table A5
# 4-1) P*(Z|X)
P_z0_x0 <- table(Z_target, X_target)[1,1]/table(X_target)[1]
P_z0_x1 <- table(Z_target, X_target)[1,2]/table(X_target)[2]
P_z1_x0 <- table(Z_target, X_target)[2,1]/table(X_target)[1]
P_z1_x1 <- table(Z_target, X_target)[2,2]/table(X_target)[2]

# 4-2) P(Y|do(X), Z)
P_y_x0_z0 <- aggregate(Y_source ~ X_source + Z_source, data=SimulatedData_source, mean)[1,3]
P_y_x1_z0 <- aggregate(Y_source ~ X_source + Z_source, data=SimulatedData_source, mean)[2,3]
P_y_x0_z1 <- aggregate(Y_source ~ X_source + Z_source, data=SimulatedData_source, mean)[3,3]
P_y_x1_z1 <- aggregate(Y_source ~ X_source + Z_source, data=SimulatedData_source, mean)[4,3]

# 4-3) Calculate the transported causal estimates. 
E_Y_x0 <- P_y_x0_z0*P_z0_x0 + P_y_x0_z1*P_z1_x0
E_Y_x1 <- P_y_x1_z0*P_z0_x1 + P_y_x1_z1*P_z1_x1
ATE_transported <- E_Y_x1-E_Y_x0
print(c(E_Y_x0, E_Y_x1, ATE_transported))

# 5) When the placebo intervention is given.
Y_source <- 0*Z_source + 0*X_source + 0.5*U_source + u2
SimulatedData_source <- data.frame(X_source,Z_source,Y_source,U_source, u1, u2)  

Y_target <- 0*Z_target + 0*X_target + 0.5*U_target + u2    
SimulatedData_target <- data.frame(X_target,Z_target,Y_target,U_target, u1, u2)

# 5-1) Ground truth OLS result; column (1) of Table A5 
model_placebo_ref <- lm(Y_target ~ X_target + U_target + u1 + u2, data = SimulatedData_target)
summary(model_placebo_ref)

# 5-2) Transportability result; column (2) of Table A5 
# 5-2-1) P*(Z|X)
P_z0_x0 <- table(Z_target, X_target)[1,1]/table(X_target)[1]
P_z0_x1 <- table(Z_target, X_target)[1,2]/table(X_target)[2]
P_z1_x0 <- table(Z_target, X_target)[2,1]/table(X_target)[1]
P_z1_x1 <- table(Z_target, X_target)[2,2]/table(X_target)[2]

# 5-2-2) P(Y|do(X), Z)
P_y_x0_z0 <- aggregate(Y_source ~ X_source + Z_source, data=SimulatedData_source, mean)[1,3]
P_y_x1_z0 <- aggregate(Y_source ~ X_source + Z_source, data=SimulatedData_source, mean)[2,3]
P_y_x0_z1 <- aggregate(Y_source ~ X_source + Z_source, data=SimulatedData_source, mean)[3,3]
P_y_x1_z1 <- aggregate(Y_source ~ X_source + Z_source, data=SimulatedData_source, mean)[4,3]

# 5-2-3) Calculate the transported causal estimates. 
E_Y_x0 <- P_y_x0_z0*P_z0_x0 + P_y_x0_z1*P_z1_x0
E_Y_x1 <- P_y_x1_z0*P_z0_x1 + P_y_x1_z1*P_z1_x1
ATE_transported_placebo <- E_Y_x1-E_Y_x0
print(c(E_Y_x0, E_Y_x1, ATE_transported_placebo))

#################################################################################
# 1) Simulate the source data from Figure 5B

rm(list=ls())
n <- 1000000
set.seed(5)
u1 <- rnorm(n, 0, 1)
u2 <- rnorm(n, 0, 1)

U_source <- rbinom(n, 1, 1/(1+exp(-u1)))
X_source <- rbinom(n, 1, 0.5)     
Z_source <- rbinom(n, 1, 1/(1+exp(-(-1*X_source))))                             
Y_source <- 0.5*Z_source + 0.5*X_source + 0.5*U_source + u2

SimulatedData_source <- data.frame(X_source,Z_source,Y_source,U_source, u1, u2)  

# 2) Simulate the target data from Figure 5B
set.seed(5)
u1 <- rnorm(n, 0, 1)
u2 <- rnorm(n, 0, 1)
S <- rnorm(n,0,2)

U_target <- rbinom(n, 1, 1/(1+exp(-u1)))
X_target <- rbinom(n, 1, 1/(1+exp(-(U_target + u1 + u2 + S))))     
Z_target <- rbinom(n, 1, 1/(1+exp(-(-1*X_target + S))))                             
Y_target <- 0.5*Z_target + 0.5*X_target + 0.5*U_target + u2    

SimulatedData_target <- data.frame(X_target,Z_target,Y_target,U_target, u1, u2)  

# 3) Ground truth OLS result; column (3) of Table A5 
model_ref <- lm(Y_target ~ X_target + U_target + u1 + u2, data = SimulatedData_target)
summary(model_ref)

# 4) Transportability result; column (4) of Table A5
# 4-1) P*(Z|X)
P_z0_x0 <- table(Z_target, X_target)[1,1]/table(X_target)[1]
P_z0_x1 <- table(Z_target, X_target)[1,2]/table(X_target)[2]
P_z1_x0 <- table(Z_target, X_target)[2,1]/table(X_target)[1]
P_z1_x1 <- table(Z_target, X_target)[2,2]/table(X_target)[2]

# 4-2) P(Y|do(X), Z)
P_y_x0_z0 <- aggregate(Y_source ~ X_source + Z_source, data=SimulatedData_source, mean)[1,3]
P_y_x1_z0 <- aggregate(Y_source ~ X_source + Z_source, data=SimulatedData_source, mean)[2,3]
P_y_x0_z1 <- aggregate(Y_source ~ X_source + Z_source, data=SimulatedData_source, mean)[3,3]
P_y_x1_z1 <- aggregate(Y_source ~ X_source + Z_source, data=SimulatedData_source, mean)[4,3]

# 4-3) Calculate the transported causal estimates. 
E_Y_x0 <- P_y_x0_z0*P_z0_x0 + P_y_x0_z1*P_z1_x0
E_Y_x1 <- P_y_x1_z0*P_z0_x1 + P_y_x1_z1*P_z1_x1
ATE_transported <- E_Y_x1-E_Y_x0
print(c(E_Y_x0, E_Y_x1, ATE_transported))

# 5) When the placebo intervention is given.
Y_source <- 0*Z_source + 0*X_source + 0.5*U_source + u2
SimulatedData_source <- data.frame(X_source,Z_source,Y_source,U_source, u1, u2)  

Y_target <- 0*Z_target + 0*X_target + 0.5*U_target + u2    
SimulatedData_target <- data.frame(X_target,Z_target,Y_target,U_target, u1, u2)  

# 5-1) Ground truth OLS result; column (1) of Table A5 
model_placebo_ref <- lm(Y_target ~ X_target + U_target + u1 + u2, data = SimulatedData_target)
summary(model_placebo_ref)

# 5-2) Transportability result; column (2) of Table A5 
# 5-2-1) P*(Z|X)
P_z0_x0 <- table(Z_target, X_target)[1,1]/table(X_target)[1]
P_z0_x1 <- table(Z_target, X_target)[1,2]/table(X_target)[2]
P_z1_x0 <- table(Z_target, X_target)[2,1]/table(X_target)[1]
P_z1_x1 <- table(Z_target, X_target)[2,2]/table(X_target)[2]

# 5-2-2) P(Y|do(X), Z)
P_y_x0_z0 <- aggregate(Y_source ~ X_source + Z_source, data=SimulatedData_source, mean)[1,3]
P_y_x1_z0 <- aggregate(Y_source ~ X_source + Z_source, data=SimulatedData_source, mean)[2,3]
P_y_x0_z1 <- aggregate(Y_source ~ X_source + Z_source, data=SimulatedData_source, mean)[3,3]
P_y_x1_z1 <- aggregate(Y_source ~ X_source + Z_source, data=SimulatedData_source, mean)[4,3]

# 5-2-3) Calculate the transported causal estimates. 
E_Y_x0 <- P_y_x0_z0*P_z0_x0 + P_y_x0_z1*P_z1_x0
E_Y_x1 <- P_y_x1_z0*P_z0_x1 + P_y_x1_z1*P_z1_x1
ATE_transported_placebo <- E_Y_x1-E_Y_x0
print(c(E_Y_x0, E_Y_x1, ATE_transported_placebo))

#################################################################################
# 1) Simulate the source data from Figure 5C

rm(list=ls())
n <- 1000000
set.seed(5)
u1 <- rnorm(n, 0, 1)
u2 <- rnorm(n, 0, 1)
u3 <- rnorm(n, 0, 1)

U_source <- rbinom(n, 1, 1/(1+exp(-u1+u3)))
X_source <- rbinom(n, 1, 0.5)     
Z_source <- rbinom(n, 1, 1/(1+exp(-(-1*X_source))))                             
Y_source <- 0.5*Z_source + 0.5*X_source + 0.5*U_source + u2 +u3

SimulatedData_source <- data.frame(X_source,Z_source,Y_source,U_source, u1, u2, u3)  

# 2) Simulate the target data from Figure 5C
set.seed(5)
u1 <- rnorm(n, 0, 1)
u2 <- rnorm(n, 0, 1)
u3 <- rnorm(n, 0, 1)
S <- rnorm(n,0,2)

U_target <- rbinom(n, 1, 1/(1+exp(-u1+u3)))
X_target <- rbinom(n, 1, 1/(1+exp(-(U_target + u1 + u2))))     
Z_target <- rbinom(n, 1, 1/(1+exp(-(-1*X_target + S))))                             
Y_target <- 0.5*Z_target + 0.5*X_target + 0.5*U_target + u2 + u3   

SimulatedData_target <- data.frame(X_target,Z_target,Y_target,U_target, u1, u2, u3)  

# 3) Ground truth OLS result; column (3) of Table A5
model_ref <- lm(Y_target ~ X_target + U_target + u1 + u2 + u3, data = SimulatedData_target)
summary(model_ref)

# 4) Transportability result; column (4) of Table A5
# 4-1) P*(Z|X)
P_z0_x0 <- table(Z_target, X_target)[1,1]/table(X_target)[1]
P_z0_x1 <- table(Z_target, X_target)[1,2]/table(X_target)[2]
P_z1_x0 <- table(Z_target, X_target)[2,1]/table(X_target)[1]
P_z1_x1 <- table(Z_target, X_target)[2,2]/table(X_target)[2]

# 4-2) P(Y|do(X), Z)
P_y_x0_z0 <- aggregate(Y_source ~ X_source + Z_source, data=SimulatedData_source, mean)[1,3]
P_y_x1_z0 <- aggregate(Y_source ~ X_source + Z_source, data=SimulatedData_source, mean)[2,3]
P_y_x0_z1 <- aggregate(Y_source ~ X_source + Z_source, data=SimulatedData_source, mean)[3,3]
P_y_x1_z1 <- aggregate(Y_source ~ X_source + Z_source, data=SimulatedData_source, mean)[4,3]

# 4-3) Calculate the transported causal estimates. 
E_Y_x0 <- P_y_x0_z0*P_z0_x0 + P_y_x0_z1*P_z1_x0
E_Y_x1 <- P_y_x1_z0*P_z0_x1 + P_y_x1_z1*P_z1_x1
ATE_transported <- E_Y_x1-E_Y_x0
print(c(E_Y_x0, E_Y_x1, ATE_transported))

# 5) When the placebo intervention is given.
Y_source <- 0*Z_source + 0*X_source + 0.5*U_source + u2 +u3
SimulatedData_source <- data.frame(X_source,Z_source,Y_source,U_source, u1, u2, u3)  

Y_target <- 0*Z_target + 0*X_target + 0.5*U_target + u2 + u3   
SimulatedData_target <- data.frame(X_target,Z_target,Y_target,U_target, u1, u2, u3)  

# 5-1) Ground truth OLS result; column (1) of Table A5 
model_placebo_ref <- lm(Y_target ~ X_target + U_target + u1 + u2 + u3, data = SimulatedData_target)
summary(model_placebo_ref)

# 5-2) Transportability result; column (2) of Table A5 
# 5-2-1) P*(Z|X)
P_z0_x0 <- table(Z_target, X_target)[1,1]/table(X_target)[1]
P_z0_x1 <- table(Z_target, X_target)[1,2]/table(X_target)[2]
P_z1_x0 <- table(Z_target, X_target)[2,1]/table(X_target)[1]
P_z1_x1 <- table(Z_target, X_target)[2,2]/table(X_target)[2]

# 5-2-2) P(Y|do(X), Z)
P_y_x0_z0 <- aggregate(Y_source ~ X_source + Z_source, data=SimulatedData_source, mean)[1,3]
P_y_x1_z0 <- aggregate(Y_source ~ X_source + Z_source, data=SimulatedData_source, mean)[2,3]
P_y_x0_z1 <- aggregate(Y_source ~ X_source + Z_source, data=SimulatedData_source, mean)[3,3]
P_y_x1_z1 <- aggregate(Y_source ~ X_source + Z_source, data=SimulatedData_source, mean)[4,3]

# 5-2-3) Calculate the transported causal estimates. 
E_Y_x0 <- P_y_x0_z0*P_z0_x0 + P_y_x0_z1*P_z1_x0
E_Y_x1 <- P_y_x1_z0*P_z0_x1 + P_y_x1_z1*P_z1_x1
ATE_transported_placebo <- E_Y_x1-E_Y_x0
print(c(E_Y_x0, E_Y_x1, ATE_transported_placebo))

#################################################################################
# 1) Simulate the source data from Figure 5D

rm(list=ls())
n <- 1000000
set.seed(5)
u1 <- rnorm(n, 0, 1)
u2 <- rnorm(n, 0, 1)

U_source <- rbinom(n, 1, 1/(1+exp(-u1)))
X_source <- rbinom(n, 1, 0.5)     
Z_source <- rbinom(n, 1, 1/(1+exp(-(-1*X_source))))                             
Y_source <- 0.5*Z_source + 0.5*U_source + u2

SimulatedData_source <- data.frame(X_source,Z_source,Y_source,U_source, u1, u2)  

# 2) Simulate the target data from Figure 5D
set.seed(5)
u1 <- rnorm(n, 0, 1)
u2 <- rnorm(n, 0, 1)
S <- rnorm(n,0,2)

U_target <- rbinom(n, 1, 1/(1+exp(-u1)))
X_target <- rbinom(n, 1, 1/(1+exp(-(U_target + u1 + u2))))     
Z_target <- rbinom(n, 1, 1/(1+exp(-(-1*X_target+ S))))                             
Y_target <- 0.5*Z_target + 0.5*U_target + u2    

SimulatedData_target <- data.frame(X_target,Z_target,Y_target,U_target, u1, u2)  

# 3) Ground truth OLS result; column (3) of Table A5 
model_ref <- lm(Y_target ~ X_target + U_target + u1 + u2, data = SimulatedData_target)
summary(model_ref)

# 4) Transportability result; column (4) of Table A5
# 4-1) P*(Z|X)
P_z0_x0 <- table(Z_target, X_target)[1,1]/table(X_target)[1]
P_z0_x1 <- table(Z_target, X_target)[1,2]/table(X_target)[2]
P_z1_x0 <- table(Z_target, X_target)[2,1]/table(X_target)[1]
P_z1_x1 <- table(Z_target, X_target)[2,2]/table(X_target)[2]

# 4-2) P*(Y|X, Z)
P_y_x0_z0 <- aggregate(Y_target ~ X_target + Z_target, data=SimulatedData_target, mean)[1,3]
P_y_x1_z0 <- aggregate(Y_target ~ X_target + Z_target, data=SimulatedData_target, mean)[2,3]
P_y_x0_z1 <- aggregate(Y_target ~ X_target + Z_target, data=SimulatedData_target, mean)[3,3]
P_y_x1_z1 <- aggregate(Y_target ~ X_target + Z_target, data=SimulatedData_target, mean)[4,3]

# 4-3) P*(X)
P_x0 <- table(X_target)[1]/n
P_x1 <- table(X_target)[2]/n

# 4-4) Calculate the transported causal estimates. 
E_Y_x0 <- P_z0_x0*(P_y_x0_z0*P_x0 + P_y_x1_z0*P_x1) + P_z1_x0*(P_y_x0_z1*P_x0 + P_y_x1_z1*P_x1)
E_Y_x1 <- P_z0_x1*(P_y_x0_z0*P_x0 + P_y_x1_z0*P_x1) + P_z1_x1*(P_y_x0_z1*P_x0 + P_y_x1_z1*P_x1)
ATE_transported <- E_Y_x1-E_Y_x0
print(c(E_Y_x0, E_Y_x1, ATE_transported))

# 5) When the placebo intervention is given.
Y_source <- 0*Z_source + 0.5*U_source + u2
SimulatedData_source <- data.frame(X_source,Z_source,Y_source,U_source, u1, u2)  

Y_target <- 0*Z_target + 0.5*U_target + u2    
SimulatedData_target <- data.frame(X_target,Z_target,Y_target,U_target, u1, u2)  

# 5-1) Ground truth OLS result; column (1) of Table A5 
model_placebo_ref <- lm(Y_target ~ X_target + U_target + u1 + u2, data = SimulatedData_target)
summary(model_placebo_ref)

# 5-2) Transportability result; column (2) of Table A5 
# 5-2-1) P*(Z|X)
P_z0_x0 <- table(Z_target, X_target)[1,1]/table(X_target)[1]
P_z0_x1 <- table(Z_target, X_target)[1,2]/table(X_target)[2]
P_z1_x0 <- table(Z_target, X_target)[2,1]/table(X_target)[1]
P_z1_x1 <- table(Z_target, X_target)[2,2]/table(X_target)[2]

# 5-2-2) P*(Y|X, Z)
P_y_x0_z0 <- aggregate(Y_target ~ X_target + Z_target, data=SimulatedData_target, mean)[1,3]
P_y_x1_z0 <- aggregate(Y_target ~ X_target + Z_target, data=SimulatedData_target, mean)[2,3]
P_y_x0_z1 <- aggregate(Y_target ~ X_target + Z_target, data=SimulatedData_target, mean)[3,3]
P_y_x1_z1 <- aggregate(Y_target ~ X_target + Z_target, data=SimulatedData_target, mean)[4,3]

# 5-2-3) P*(X)
P_x0 <- table(X_target)[1]/n
P_x1 <- table(X_target)[2]/n

# 5-2-4) Calculate the transported causal estimates. 
E_Y_x0 <- P_z0_x0*(P_y_x0_z0*P_x0 + P_y_x1_z0*P_x1) + P_z1_x0*(P_y_x0_z1*P_x0 + P_y_x1_z1*P_x1)
E_Y_x1 <- P_z0_x1*(P_y_x0_z0*P_x0 + P_y_x1_z0*P_x1) + P_z1_x1*(P_y_x0_z1*P_x0 + P_y_x1_z1*P_x1)
ATE_transported_placebo <- E_Y_x1-E_Y_x0
print(c(E_Y_x0, E_Y_x1, ATE_transported_placebo))


#################################################################################
# 1) Simulate the source data from Figure 5E

rm(list=ls())
n <- 1000000
set.seed(5)
u1 <- rnorm(n, 0, 1)
u2 <- rnorm(n, 0, 1)

W_source <- rbinom(n, 1, 1/(1+exp(-u1)))
X_source <- rbinom(n, 1, 0.5)     
Z_source <- rbinom(n, 1, 1/(1+exp(-(-1*X_source))))                             
Y_source <- 0.5*Z_source + 0.5*X_source + 0.5*W_source + u2

SimulatedData_source <- data.frame(X_source,Z_source,Y_source,W_source, u1, u2)  

# 2) # Simulate the target data from Figure 5E
set.seed(5)
u1 <- rnorm(n, 0, 1)
u2 <- rnorm(n, 0, 1)
S <- rnorm(n,0,2)

W_target <- rbinom(n, 1, 1/(1+exp(-u1 + S)))
X_target <- rbinom(n, 1, 1/(1+exp(-(W_target + u1 + u2))))     
Z_target <- rbinom(n, 1, 1/(1+exp(-(-1*X_target + S))))                             
Y_target <- 0.5*Z_target + 0.5*X_target + 0.5*W_target + u2    

SimulatedData_target <- data.frame(X_target,Z_target,Y_target,W_target, u1, u2)  

# 3) Ground truth OLS result; column (3) of Table A5 
model_ref <- lm(Y_target ~ X_target + W_target + u1 + u2, data = SimulatedData_target)
summary(model_ref)

# 4) Transportability result; column (4) of Table A5 
# 4-1) P*(W)
P_w0 <- table(W_target)[1]/n
P_w1 <- table(W_target)[2]/n

# 4-2) P*(Z|W, X)
P_z0_w0_x0 <- table(Z_target, W_target, X_target)[1,1,1]/table(W_target, X_target)[1,1]
P_z0_w1_x0 <- table(Z_target, W_target, X_target)[1,2,1]/table(W_target, X_target)[2,1]
P_z1_w0_x0 <- table(Z_target, W_target, X_target)[2,1,1]/table(W_target, X_target)[1,1]
P_z1_w1_x0 <- table(Z_target, W_target, X_target)[2,2,1]/table(W_target, X_target)[2,1]

P_z0_w0_x1 <- table(Z_target, W_target, X_target)[1,1,2]/table(W_target, X_target)[1,2]
P_z0_w1_x1 <- table(Z_target, W_target, X_target)[1,2,2]/table(W_target, X_target)[2,2]
P_z1_w0_x1 <- table(Z_target, W_target, X_target)[2,1,2]/table(W_target, X_target)[1,2]
P_z1_w1_x1 <- table(Z_target, W_target, X_target)[2,2,2]/table(W_target, X_target)[2,2]

# 4-3) P(Y|do(X), W, Z)
P_y_x0_w0_z0 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[1,4]
P_y_x0_w1_z0 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[3,4]
P_y_x0_w0_z1 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[5,4]
P_y_x0_w1_z1 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[7,4]

P_y_x1_w0_z0 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[2,4]
P_y_x1_w1_z0 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[4,4]
P_y_x1_w0_z1 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[6,4]
P_y_x1_w1_z1 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[8,4]

# 4-4) Calculate the transported causal estimates. 
E_Y_x0 <- P_y_x0_w0_z0*P_z0_w0_x0*P_w0 + P_y_x0_w1_z0*P_z0_w1_x0*P_w1 + P_y_x0_w0_z1*P_z1_w0_x0*P_w0 + P_y_x0_w1_z1*P_z1_w1_x0*P_w1
E_Y_x1 <- P_y_x1_w0_z0*P_z0_w0_x1*P_w0 + P_y_x1_w1_z0*P_z0_w1_x1*P_w1 + P_y_x1_w0_z1*P_z1_w0_x1*P_w0 + P_y_x1_w1_z1*P_z1_w1_x1*P_w1
ATE_transported <- E_Y_x1-E_Y_x0
print(c(E_Y_x0, E_Y_x1, ATE_transported))

# 5) When the placebo intervention is given.
Y_source <- 0*Z_source + 0*X_source + 0.5*W_source + u2
SimulatedData_source <- data.frame(X_source,Z_source,Y_source,W_source, u1, u2)  

Y_target <- 0*Z_target + 0*X_target + 0.5*W_target + u2    
SimulatedData_target <- data.frame(X_target,Z_target,Y_target,W_target, u1, u2)  

# 5-1) Ground truth OLS result; column (1) of Table A5 
model_placebo_ref <- lm(Y_target ~ X_target + W_target + u1 + u2, data = SimulatedData_target)
summary(model_placebo_ref)

# 5-2) Transportability result; column (2) of Table A5 
# 5-2-1) P*(W)
P_w0 <- table(W_target)[1]/n
P_w1 <- table(W_target)[2]/n

# 5-2-2) P*(Z|W, X)
P_z0_w0_x0 <- table(Z_target, W_target, X_target)[1,1,1]/table(W_target, X_target)[1,1]
P_z0_w1_x0 <- table(Z_target, W_target, X_target)[1,2,1]/table(W_target, X_target)[2,1]
P_z1_w0_x0 <- table(Z_target, W_target, X_target)[2,1,1]/table(W_target, X_target)[1,1]
P_z1_w1_x0 <- table(Z_target, W_target, X_target)[2,2,1]/table(W_target, X_target)[2,1]

P_z0_w0_x1 <- table(Z_target, W_target, X_target)[1,1,2]/table(W_target, X_target)[1,2]
P_z0_w1_x1 <- table(Z_target, W_target, X_target)[1,2,2]/table(W_target, X_target)[2,2]
P_z1_w0_x1 <- table(Z_target, W_target, X_target)[2,1,2]/table(W_target, X_target)[1,2]
P_z1_w1_x1 <- table(Z_target, W_target, X_target)[2,2,2]/table(W_target, X_target)[2,2]

# 5-2-3) P(Y|do(X), W, Z)
P_y_x0_w0_z0 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[1,4]
P_y_x0_w1_z0 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[3,4]
P_y_x0_w0_z1 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[5,4]
P_y_x0_w1_z1 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[7,4]

P_y_x1_w0_z0 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[2,4]
P_y_x1_w1_z0 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[4,4]
P_y_x1_w0_z1 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[6,4]
P_y_x1_w1_z1 <- aggregate(Y_source ~ X_source + W_source + Z_source, data=SimulatedData_source, mean)[8,4]

# 5-2-4) Calculate the transported causal estimates. 
E_Y_x0 <- P_y_x0_w0_z0*P_z0_w0_x0*P_w0 + P_y_x0_w1_z0*P_z0_w1_x0*P_w1 + P_y_x0_w0_z1*P_z1_w0_x0*P_w0 + P_y_x0_w1_z1*P_z1_w1_x0*P_w1
E_Y_x1 <- P_y_x1_w0_z0*P_z0_w0_x1*P_w0 + P_y_x1_w1_z0*P_z0_w1_x1*P_w1 + P_y_x1_w0_z1*P_z1_w0_x1*P_w0 + P_y_x1_w1_z1*P_z1_w1_x1*P_w1
ATE_transported_placebo <- E_Y_x1-E_Y_x0
print(c(E_Y_x0, E_Y_x1, ATE_transported_placebo))


