install.packages("MASS")
library(MASS)
install.packages("car")
library(car)
install.packages("dplyr")
library(dplyr)

#situation A
#"Results for Situation A were obtained by conducting three t tests, 
#one on each of two dependent variables and a third on the average of these two variables." (Simmons et al., 2011, p.1361)

set.seed(32)
n <- 20
nsim <- 15000
p_v1a <- numeric(nsim)
p_v2a <- numeric(nsim)
p_v1_v2a <- numeric(nsim)

for (i in 1:nsim) {
  sigma <- rbind(c(1, 0.5), c(0.5,1))
  mu <- c(0, 0)
  two_cor_vars <- as.data.frame(mvrnorm(n = n, mu = mu, Sigma = sigma))
  p_v1a[i] <- t.test(two_cor_vars$V1)$p.value
  p_v2a[i] <- t.test(two_cor_vars$V2)$p.value
  p_v1_v2a[i] <- t.test(rowMeans(two_cor_vars))$p.value
}

alphalevels <- c(0.1, 0.05, 0.01)
for (i in alphalevels) {
  false_positive_a <- c((p_v1a < i)| (p_v2a < i)| (p_v1_v2a < i))
 print(prop.table(table(false_positive_a))) 
}

#situation B
#"Results for Situation B were obtained by conducting one t test after collecting 20 observations per cell 
#and another after collecting an additional 10 observations per cell." (Simmons et al., 2011, p.1361)

set.seed(32)
nsim <- 15000
p_b <- numeric(nsim)
p <- 0.1
#repeat for other values of p
#p <- 0.05
#p <- 0.01
for (i in 0:nsim) {
  var1b <- rnorm(n = 20, mean = 0, sd = 1)
  var2b <- rnorm(n = 20, mean = 0, sd = 1)
  if (t.test(var1b, var2b)$p.value < p) {
    p_b[i] <- t.test(var1b, var2b)$p.value
  } else {
    var_extra1b <- rnorm(n = 10, mean = 0, sd = 1)
    var_extra2b <- rnorm(n = 10, mean = 0, sd = 1)
    var1b <- append(var1b, var_extra1b)
    var2b <- append(var2b, var_extra2b)
    p_b[i] <- t.test(var1b, var2b)$p.value
  }
}

false_positive_b <- (p_b < p)
print(prop.table(table(false_positive_b))) 


#situation C
#"Results for Situation C were obtained by conducting a t test, 
#an analysis of covariance with a gender main effect, 
#and an analysis of covariance with a gender interaction 
#(each observation was assigned a 50% probability of being female). 
#We report a significant effect if the effect of condition was significant in any of these analyses 
#or if the Gender × Condition interaction was significant." (Simmons et al., 2011, p.1361)

set.seed(32)
n <- 20
nsim <- 15000
p_maineffect <- NULL
p_interaction <- NULL
p_t <- NULL

for (i in 1:nsim) {
  var1 <- rnorm(n = 20, mean = 0, sd = 1)
  var2 <- rnorm(n = 20, mean = 0, sd = 1)
  gender <- rbinom(n = n, size = 1, prob = 0.5)
  ancova_model <- aov(var1 ~ var2 * gender)
  p_t [i] <- t.test(var1, var2)$p.value
  p_maineffect[i] <- Anova(ancova_model, type="III", singular.ok = T)$"Pr(>F)"[3]
  p_interaction[i] <- Anova(ancova_model, type="III", singular.ok = T)$"Pr(>F)"[4]
}

alphalevels <- c(0.1, 0.05, 0.01)
for (i in alphalevels) {
  false_positive_c <- c((p_maineffect < i) | (p_interaction < i) | (p_t < i))
  print(prop.table(table(false_positive_c))) 
}

#Situation D 
#"Results for Situation D were obtained by conducting t tests for each of the three possible pairings of conditions 
#and an ordinary least squares regression for the linear trend of all three conditions 
#(coding: low = –1, medium = 0, high = 1)
#reporting the results for any two or all three 
#(e.g., low vs. medium, low vs. high, medium vs. high, low vs. medium vs. high)." (Simmons et al., 2011, p.1361)

set.seed(32)
n <- 20
nsim <- 15000
p_low_med <- NULL 
p_low_high <- NULL
p_med_high <- NULL
p_conditions <- NULL
overall_p <- function(my_model) {
  f <- summary(my_model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

for (i in 1:nsim) {
  condition_d <- data.frame(var1 = rnorm(n = 20, mean = 0, sd = 1), var2 = rnorm(n = 20, mean = 0, sd = 1), condition = sample(rep(c(-1, 0, 1), times = c(6, 7, 7))))
  p_low_med[i] <- t.test(condition_d$var1[condition_d$condition == "-1"], condition_d$var1[condition_d$condition == "0"])$p.value
  p_low_high[i] <- t.test(condition_d$var1[condition_d$condition == "-1"], condition_d$var1[condition_d$condition == "1"])$p.value
  p_med_high[i] <- t.test(condition_d$var1[condition_d$condition == "0"], condition_d$var1[condition_d$condition == "1"])$p.value
  p_conditions[i] <- overall_p(lm(condition_d$var1 ~ condition_d$condition))
}

alphalevels <- c(0.1, 0.05, 0.01)
for (i in alphalevels) {
  false_positive_d <- c((p_low_med < i) | (p_low_high < i) | (p_med_high < i) | (p_conditions < i))
  print(prop.table(table(false_positive_d))) 
}

#A and B combination

set.seed(32)
n <- 20
nsim <- 15000
p_v1ab <- numeric(nsim)
p_v2ab <- numeric(nsim)
p_v1_v2ab <- numeric(nsim)
p <- 0.1
#repeat for other p values
#p <- 0.05
#p <- 0.01

for (i in 1:nsim) {
  sigma <- rbind(c(1, 0.5), c(0.5,1))
  mu <- c(0, 0)
  two_cor_vars <- as.data.frame(mvrnorm(n = n, mu = mu, Sigma = sigma))
  
  if (t.test(two_cor_vars$V1)$p.value < p) {
    p_v1ab[i] <- t.test(two_cor_vars$V1)$p.value  
  } else {
    v1extra <- mvrnorm(n = 10, mu = mu, Sigma = sigma)
    v1ab <- rbind(two_cor_vars, v1extra)
    p_v1ab[i] <- t.test(v1ab$V1)$p.value
  }
  
  if (t.test(two_cor_vars$V2)$p.value < p) {
    p_v2ab[i] <- t.test(two_cor_vars$V2)$p.value 
  } else {
    v2extra <- mvrnorm(n = 10, mu = mu, Sigma = sigma)
    v2ab <- rbind(two_cor_vars, v2extra)
    p_v2ab[i] <- t.test(v2ab$V2)$p.value
  }
  
  if (t.test(rowMeans(two_cor_vars))$p.value < p) {
    p_v1_v2ab[i] <- t.test(rowMeans(two_cor_vars))$p.value
  } else {
    extra <- mvrnorm(n = 10, mu = mu, Sigma = sigma)
    vab <- rbind(two_cor_vars, extra)
    p_v1_v2ab[i] <- t.test(rowMeans(vab))$p.value
  }
}

false_positive_ab <- c((p_v1ab < p) | (p_v2ab < p)| (p_v1_v2ab < p))
print(prop.table(table(false_positive_ab))) 

#A, B, and C combination

set.seed(32)
n <- 20
nsim <- 15000
p_v1abc <- NULL
p_v2abc <- NULL
p_v1_v2abc <- NULL
p_maineffect_abc <- NULL
p_interaction_abc <- NULL
p <- 0.1
#run this section with other values of p again to get the false positive rates
#p <- 0.05
#p <- 0.01

for (i in 1:nsim) {
  sigma <- rbind(c(1, 0.5), c(0.5,1))
  mu <- c(0, 0)
  two_cor_vars <- as.data.frame(mvrnorm(n = n, mu = mu, Sigma = sigma))
  gender <- rbinom(n = n, size = 1, prob = 0.5)
  two_cor_vars_gender <- cbind(two_cor_vars, gender)
  ancova_model_abc <- aov(two_cor_vars_gender$V1 ~ two_cor_vars_gender$V2 * two_cor_vars_gender$gender, singular.ok = T)
  
  if (t.test(two_cor_vars_gender$V1)$p.value < p) {
    p_v1abc[i] <- t.test(two_cor_vars_gender$V1)$p.value  
  } else {
    v1extra <- mvrnorm(n = 10, mu = mu, Sigma = sigma)
    v1abc <- rbind(two_cor_vars_gender[1:2], v1extra)
    p_v1abc[i] <- t.test(v1abc$V1)$p.value
  }
  
  if (t.test(two_cor_vars$V2)$p.value < p) {
    p_v2abc[i] <- t.test(two_cor_vars_gender$V2)$p.value 
  } else {
    v2extra <- mvrnorm(n = 10, mu = mu, Sigma = sigma)
    v2abc <- rbind(two_cor_vars_gender[1:2], v2extra)
    p_v2abc[i] <- t.test(v2abc$V2)$p.value
  }
  
  if (t.test(rowMeans(two_cor_vars_gender))$p.value < p) {
    p_v1_v2abc[i] <- t.test(rowMeans(two_cor_vars_gender[1:2]))$p.value
  } else {
    extra <- mvrnorm(n = 10, mu = mu, Sigma = sigma)
    vab <- rbind(two_cor_vars_gender[1:2], extra)
    p_v1_v2abc[i] <- t.test(rowMeans(vab))$p.value
  }
  
  ancova_model_abc <- aov(two_cor_vars_gender$V1 ~ two_cor_vars_gender$V2 * two_cor_vars_gender$gender)
  
  if (Anova(ancova_model_abc, type = "III", data = two_cor_vars_gender, singular.ok = T)$"Pr(>F)"[3] < p) {
    p_maineffect_abc[i] <- Anova(ancova_model_abc, type = "III", singular.ok = T)$"Pr(>F)"[3]
  } else {
    extra_abc <- mvrnorm(n = 10, mu = mu, Sigma = sigma)
    gender_add <- rbinom(n = 10, size = 1, prob = 0.5)
    vabc1_1 <- cbind(extra_abc, gender_add)
    colnames(vabc1_1) <- colnames(two_cor_vars_gender)
    vabc1 <- rbind(two_cor_vars_gender, vabc1_1)
    ancova_model_abc1 <- aov(vabc1$V1 ~ vabc1$V2 * vabc1$gender, data = vabc1)
    p_maineffect_abc[i] <- Anova(ancova_model_abc1, type = "III", singular.ok = T)$"Pr(>F)"[3]
  }
  
  #ancova_model_abc <- aov(two_cor_vars_gender$V1 ~ two_cor_vars_gender$V2 * two_cor_vars_gender$gender, singular.ok = T)
  
  if (Anova(ancova_model_abc, type = "III", singular.ok = T)$"Pr(>F)"[4] < p) {
    p_interaction_abc[i] <- Anova(ancova_model_abc, type = "III", singular.ok = T)$"Pr(>F)"[4]
  } else {
    extra_abc <- mvrnorm(n = 10, mu = mu, Sigma = sigma)
    gender_add <- rbinom(n = 10, size = 1, prob = 0.5)
    vabc2_1 <- cbind(extra_abc, gender_add)
    colnames(vabc2_1) <- colnames(two_cor_vars_gender)
    vabc2 <- rbind(two_cor_vars_gender, vabc2_1)
    ancova_model_abc2 <- aov(vabc2$V1 ~ vabc2$V2 * vabc2$gender, data = vabc2, singular.ok = T)
    p_interaction_abc[i] <- Anova(ancova_model_abc2, type = "III", singular.ok = T)$"Pr(>F)"[4]
  }
}

false_positive_abc <- c((p_v1abc < p)| (p_v2abc < p)| (p_v1_v2abc < p) |  (p_maineffect_abc < p) | (p_interaction_abc < p))
prop.table(table(false_positive_abc))

#paired samples t test to see if the difference between the ratios obtained with this code
#is not significantly different from the paper by Simmons and colleagues (2011)

simmons <- c(17.8, 9.5, 2.2, 14.5, 7.7, 1.6, 21.6, 11.7, 2.7, 23.2, 12.6, 2.8, 26.0, 14.4, 3.3, 50.9, 30.9, 8.4)
replication <- c(17.8, 9.4, 2, 14.4, 7.7, 1.7, 26, 13.7, 2.6, 23.2, 12.7, 2.8, 29.4, 16, 3.9, 48, 28.4, 7)

false_positive_ratios <- data.frame(
                                    group = rep(c("simmons", "replication")),
                                    values = c(simmons, replication)
                                    )
t.test(values ~ group, data = false_positive_ratios, paired = TRUE)
#necessary information to report the t test in APA style
mean(simmons)
sd(simmons)
mean(replication)
sd(replication)