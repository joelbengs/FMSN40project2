library(ggplot2)
library(xtable)
data <- read.delim("Data/hospital.txt", sep = ";")

data$hosp_cat <- factor(data$hosp,
                          levels = c(0, 1),
                          labels = c("0 days", "1+ days"))

ggplot(data, aes(x = age,y = hosp)) + geom_point(size = 1)

#### Hosp and health ####
t1 = table(data$health, data$hosp_cat)
print(xtable(t1, type = "latex"), file = "table1.tex")

t2 = prop.table(table(data$health, data$hosp_cat), margin = 1)
print(xtable(t2, type = "latex"), file = "filename2.tex")

data$health <- factor(data$health,
                        levels = c(1, 2, 3),
                        labels = c("Good", "Bad", "Somewhere in between"))

model.health <- glm(hosp ~ health, family = "binomial", data = data)
# Good is already the reference variable

# beta and confidence intervals
model.health$coefficients
(ci.beta <- confint(model.health))

# odds ratio and confidence intervals
(oddsratio <- exp(model.health$coefficients))
(or.ci <- exp(ci.beta))

#Pseudo R2, AIC, BIC
nullmodel <- glm(hosp ~ 1, family = "binomial", data = data)
(lnL0 <- logLik(nullmodel)[1])

bic <- BIC(nullmodel, model.health)
aic <- AIC(nullmodel, model.health)
(collect.AIC <- data.frame(aic, bic))

collect.AIC$loglik <- 
  c(logLik(nullmodel)[1],
    logLik(model.health)[1])
collect.AIC$R2McF <- 1 - collect.AIC$loglik/lnL0
collect.AIC

print(xtable(collect.AIC, type = "latex"), file = "R2_AIC_BIC.tex")

# Global likelihood ratio test
(anova.null.health <- anova(nullmodel, model.health))
(D_diff <- anova.null.health$Deviance[2])
(df_diff <- anova.null.health$Df[2])
qchisq(1 - 0.05, df_diff)
pchisq(D_diff, df_diff, lower.tail = FALSE)

# Prediction
x0 <- data.frame(health = c("Good", "Bad", "Somewhere in between"))
(hosp.pred <- cbind(x0,
  phat = predict(model.health, x0, type = "response")))

# logit = logodds with s.e. for constructing C.I.
hosp.pred <- cbind(hosp.pred,
  logit = predict(model.health, x0, se.fit = TRUE))
head(hosp.pred)

# An unnecessary variable:
hosp.pred$logit.residual.scale <- NULL

# Calculate confidence intervals for the log odds #
# standard normal quantile:
(lambda <- qnorm(1 - 0.05/2))
hosp.pred$logit.lwr <- hosp.pred$logit.fit - lambda*hosp.pred$logit.se.fit
hosp.pred$logit.upr <- hosp.pred$logit.fit + lambda*hosp.pred$logit.se.fit
head(hosp.pred)

# transform the log-odds intervals into C.I. for odds
hosp.pred$odds.lwr <- exp(hosp.pred$logit.lwr)
hosp.pred$odds.upr <- exp(hosp.pred$logit.upr)
head(hosp.pred)

# transform the odds intervals into C.I. for p
hosp.pred$p.lwr <- hosp.pred$odds.lwr/(1 + hosp.pred$odds.lwr)
hosp.pred$p.upr <- hosp.pred$odds.upr/(1 + hosp.pred$odds.upr)
hosp.pred
hosp.pred$logit.fit <- hosp.pred$logit.se.fit <- hosp.pred$logit.lwr <- hosp.pred$logit.upr <- NULL
hosp.pred$odds.lwr <- hosp.pred$odds.upr <- NULL
hosp.pred
print(xtable(hosp.pred, type = "latex"), file = "predictions_healthcat.tex")

#### Hosp and age ####
ggplot(data, aes(x = age, y = hosp)) + geom_point(size = 0.5) + 
  geom_smooth(size = 0.5) +  
  labs(title = "1+ hospital days (=1) or 0 hospital days (=0) vs age") +
  theme(text = element_text(size = 10))

model.age <- glm(hosp ~ age, family = "binomial", data)
beta <- model.age$coefficients
betas <- data.frame(beta)
betas$expbeta <- c(exp(beta))
betas$ci <- confint(model.age)
betas$expci <- exp(confint(model.age))
betas

#Pseudo R2, AIC, BIC
nullmodel <- glm(hosp ~ 1, family = "binomial", data = data)
(lnL0 <- logLik(nullmodel)[1])

bic <- BIC(nullmodel, model.age)
aic <- AIC(nullmodel, model.age)
(collect.AIC <- data.frame(aic, bic))

collect.AIC$loglik <- 
  c(logLik(nullmodel)[1],
    logLik(model.age)[1])
collect.AIC$R2McF <- 1 - collect.AIC$loglik/lnL0
collect.AIC

print(xtable(collect.AIC, type = "latex"), file = "R2_AIC_BIC_age.tex")

# Wald test
sum.age <- summary(model.age)
print(xtable(sum.age, type = "latex"), file = "waldtest_age.tex")

# odds change 1 year
betas$expbeta[2]
betas$expci[2,]

# odds change 5 years
betas$expbeta[2]^5
betas$expci[2,]^5

# plot predicted probabilities and confidence intervals
hosp.pred <- cbind(data,
                     fit = predict(model.age, type = "response"),
                     conf = predict(model.age, interval = "confidence", type = "response"))

# logit = logodds with s.e. for constructing C.I.
hosp.pred <- cbind(
  hosp.pred,
  logit = predict(model.age, se.fit = TRUE))
head(hosp.pred)
# An unnecessary variable:
hosp.pred$logit.residual.scale <- NULL

# Calculate confidence intervals for the log odds
# standard normal quantile:
(lambda <- qnorm(1 - 0.05/2))
hosp.pred$logit.lwr <- hosp.pred$logit.fit - lambda*hosp.pred$logit.se.fit
hosp.pred$logit.upr <- hosp.pred$logit.fit + lambda*hosp.pred$logit.se.fit
head(hosp.pred)

# transform the log-odds intervals into C.I. for odds
hosp.pred$odds.lwr <- exp(hosp.pred$logit.lwr)
hosp.pred$odds.upr <- exp(hosp.pred$logit.upr)
head(hosp.pred)

# transform the odds intervals into C.I. for p
hosp.pred$p.lwr <- hosp.pred$odds.lwr/(1 + hosp.pred$odds.lwr)
hosp.pred$p.upr <- hosp.pred$odds.upr/(1 + hosp.pred$odds.upr)
head(hosp.pred)

ggplot(data = hosp.pred, aes(x = age, y = hosp)) + 
  geom_point(size = 0.5) + 
  geom_smooth(se = FALSE) + 
  geom_line(aes(y = fit), color = "red", size = 0.5) +
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +
  xlab("age") +
  ylab("hosp") +
  labs(title = "1+ hospital days (=1) or 0 hospital days  (=0) vs age",
       caption = "red = fitted line, with 95% confidence interval, blue = moving average") +
  theme(text = element_text(size = 10))

#### Non-monotonous ####
model.age.sq <- glm(hosp ~ age + I(age^2), family = "binomial", data = data)
beta <- model.age.sq$coefficients
betas.age.sq <- data.frame(beta)
betas.age.sq$expbeta <- c(exp(beta))
betas.age.sq$ci <- confint(model.age.sq)
betas.age.sq$expci <- exp(confint(model.age.sq))
betas.age.sq

# R2, AIC, BIC
nullmodel <- glm(hosp ~ 1, family = "binomial", data = data)
(lnL0 <- logLik(nullmodel)[1])

bic <- BIC(nullmodel, model.age.sq)
aic <- AIC(nullmodel, model.age.sq)
(collect.AIC.age.sq <- data.frame(aic, bic))

collect.AIC.age.sq$loglik <- 
  c(logLik(nullmodel)[1],
    logLik(model.age.sq)[1])
collect.AIC.age.sq$R2McF <- 1 - collect.AIC.age.sq$loglik/lnL0
collect.AIC.age.sq

print(xtable(collect.AIC.age.sq, type = "latex"), file = "R2_AIC_BIC_age_sq.tex")

#Wald test
summary(model.age.sq)

# predict for plotting#
# phat = estimated probabilities p
hosp.pred.sq <- cbind(
  data,
  phat = predict(model.age, type = "response"),
  phat.sq = predict(model.age.sq, type = "response"))

ggplot(hosp.pred.sq, aes(age, hosp)) +
  geom_point() +
  geom_smooth(se = FALSE, linetype = "dashed", size = 0.5) +
  geom_line(aes(y = phat), color = "red", size = 0.5) +
  geom_line(aes(y = phat.sq), color = "green", size = 0.5) +
  xlab("age") +
  ylab("hospital") +
  labs(title = "hospital (=1) or Not hospital (=0) vs age",
       caption = "red = fitted line, blue dashed = moving average") +
  theme(text = element_text(size = 10))

# logit = logodds with s.e. for constructing C.I.
hosp.pred.sq <- cbind(
  hosp.pred.sq,
  logit = predict(model.age.sq, se.fit = TRUE))
head(hosp.pred.sq)
# An unnecessary variable:
hosp.pred.sq$logit.residual.scale <- NULL

# Calculate confidence intervals for the log odds#
# standard normal quantile:
(lambda <- qnorm(1 - 0.05/2))
hosp.pred.sq$logit.lwr <- hosp.pred.sq$logit.fit - lambda*hosp.pred.sq$logit.se.fit
hosp.pred.sq$logit.upr <- hosp.pred.sq$logit.fit + lambda*hosp.pred.sq$logit.se.fit
head(hosp.pred.sq)

# transform the log-odds intervals into C.I. for odds#
hosp.pred.sq$odds.lwr <- exp(hosp.pred.sq$logit.lwr)
hosp.pred.sq$odds.upr <- exp(hosp.pred.sq$logit.upr)
head(hosp.pred.sq)

# transform the odds intervals into C.I. for p#
hosp.pred.sq$p.lwr.sq <- hosp.pred.sq$odds.lwr/(1 + hosp.pred.sq$odds.lwr)
hosp.pred.sq$p.upr.sq <- hosp.pred.sq$odds.upr/(1 + hosp.pred.sq$odds.upr)
hosp.pred.sq$p.upr <- hosp.pred$p.upr
hosp.pred.sq$p.lwr <- hosp.pred$p.lwr
head(hosp.pred.sq)

# plot the intervals:
ggplot(hosp.pred.sq, aes(age, hosp)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = phat), color = "red", size = 0.5) +
  geom_line(aes(y = phat.sq), color = "green", size = 0.5) +
  geom_smooth(se = FALSE, linetype = "dashed", size = 0.5) +
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.1) +
  geom_ribbon(aes(ymin = p.lwr.sq, ymax = p.upr.sq), alpha = 0.3) +
  xlab("Age") +
  ylab("Hospital") +
  labs(title = "Hospital 1+ days (=1) or Hospital 0 days (=0) vs age",
       caption = "red = fitted line with age model, with 95% confidence interval, 
       green = fitted line with squared age model, with 95% confidence interval") +
  theme(text = element_text(size = 10))


#### Calculate the change in odds ratio when ages increases by 1 year ####
coefficients <- model.age.sq$coefficients
# 50 
exp(coefficients[2])*exp(coefficients[3]*(2*50+1))
# 75
exp(coefficients[2])*exp(coefficients[3]*(2*75+1))
#100
exp(coefficients[2])*exp(coefficients[3]*(2*100+1))
# for linear age model
exp(model.age$coefficients[2])

