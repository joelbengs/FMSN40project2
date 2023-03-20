## Variable Selection ##
library(ggplot2)
library(xtable)
data <- read.delim("Data/hospital.txt", sep = ";")

data$hosp_cat <- factor(data$hosp,
                        levels = c(0, 1),
                        labels = c("0 days", "1+ days"))

nullmodel <- glm(hosp ~ 1, family = "binomial", data = data) 
model.age <- glm(hosp ~ age, family = "binomial", data = data) 
model.age.sq <- glm(hosp ~ age + I(age^2), family = "binomial", data = data) 
model.health <- glm(hosp ~ health, family = "binomial", data = data)

data$health <- factor(data$health,
                        levels = c(1,2,3),
                        labels = c("Good", "Bad", "Somewhere in between"))

#### Categorical variables ####
data$sex <- factor(data$sex,
                      levels = c(1,2),
                      labels = c("Male", "Female"))

data$civilst <- factor(data$civilst,
                      levels = c(1,2,3,4),
                      labels = c("Unmarried", "Married", "Divorced", "Widow"))

data$exercise <- factor(data$exercise,
                      levels = c(0,1,2,3,4),
                      labels = c("None", "Sometimes", "1/week", "2/week", "2+/week"))

data$work_norm <- factor(data$work_norm,
                      levels = c(1,2,3,4,5),
                      labels = c("1+ h", "20+ h", "35+ h", "Self-employed", "Other"))

print(xtable(table(data$sex), type = "latex"), file = "catevar.tex")
print(xtable(table(data$civilst), type = "latex"), file = "civilst.tex")
print(xtable(table(data$exercise), type = "latex"), file = "exercise.tex")
print(xtable(table(data$work_norm), type = "latex"), file = "worknorm.tex")

data$sex <- relevel(data$sex, ref = "Female")
data$civilst <- relevel(data$civilst, ref = "Married")
data$exercise <- relevel(data$exercise, ref = "Sometimes")
data$work_norm <- relevel(data$work_norm, ref = "Other")

#### Correlation between x-variables ####
contx <- data[, c("age", "inc_hh", "inc_tot")]
corr <- cor(contx)
pairs(contx)

print(xtable(corr, type = "latex"), file = "corrx.tex")

#### Full Model + R2, AIC, BIC + statistical tests ####
fullmodel <- glm(hosp ~ age + I(age^2) + health + sex + civilst + exercise + work_norm + inc_hh + inc_tot, 
                 family = "binomial", data = data)

#Pseudo R2, AIC, BIC
nullmodel <- glm(hosp ~ 1, family = "binomial", data = data)
(lnL0 <- logLik(nullmodel)[1])

bic <- BIC(nullmodel, fullmodel)
aic <- AIC(nullmodel, fullmodel)
(collect.AIC <- data.frame(aic, bic))

collect.AIC$loglik <- 
  c(logLik(nullmodel)[1],
    logLik(fullmodel)[1])
collect.AIC$R2McF <- 1 - collect.AIC$loglik/lnL0
collect.AIC

print(xtable(collect.AIC, type = "latex"), file = "R2_AIC_BIC_full.tex")

# Significant variables - partial likelihood ratio test

#sex
redmodel <- update(fullmodel, .~. -sex)
(anova.red.full <- anova(redmodel, fullmodel))
(D_diff <- anova.red.full$Deviance[2])
(df_diff <- anova.red.full$Df[2])
qchisq(1 - 0.05, df_diff)
pchisq(D_diff, df_diff, lower.tail = FALSE)

#health
redmodel <- update(fullmodel, .~. -health)
(anova.red.full <- anova(redmodel, fullmodel))
(D_diff <- anova.red.full$Deviance[2])
(df_diff <- anova.red.full$Df[2])
qchisq(1 - 0.05, df_diff)
pchisq(D_diff, df_diff, lower.tail = FALSE)

#civilst
redmodel <- update(fullmodel, .~. -civilst)
(anova.red.full <- anova(redmodel, fullmodel))
(D_diff <- anova.red.full$Deviance[2])
(df_diff <- anova.red.full$Df[2])
qchisq(1 - 0.05, df_diff)
pchisq(D_diff, df_diff, lower.tail = FALSE)

#exercise
redmodel <- update(fullmodel, .~. -exercise)
(anova.red.full <- anova(redmodel, fullmodel))
(D_diff <- anova.red.full$Deviance[2])
(df_diff <- anova.red.full$Df[2])
qchisq(1 - 0.05, df_diff)
pchisq(D_diff, df_diff, lower.tail = FALSE)

#worknorm
redmodel <- update(fullmodel, .~. -work_norm)
(anova.red.full <- anova(redmodel, fullmodel))
(D_diff <- anova.red.full$Deviance[2])
(df_diff <- anova.red.full$Df[2])
qchisq(1 - 0.05, df_diff)
pchisq(D_diff, df_diff, lower.tail = FALSE)

#inchh
redmodel <- update(fullmodel, .~. -inc_hh)
(anova.red.full <- anova(redmodel, fullmodel))
(D_diff <- anova.red.full$Deviance[2])
(df_diff <- anova.red.full$Df[2])
qchisq(1 - 0.05, df_diff)
pchisq(D_diff, df_diff, lower.tail = FALSE)

#inctot
redmodel <- update(fullmodel, .~. -inc_tot)
(anova.red.full <- anova(redmodel, fullmodel))
(D_diff <- anova.red.full$Deviance[2])
(df_diff <- anova.red.full$Df[2])
qchisq(1 - 0.05, df_diff)
pchisq(D_diff, df_diff, lower.tail = FALSE)

#age
redmodel <- update(fullmodel, .~. -age)
(anova.red.full <- anova(redmodel, fullmodel))
(D_diff <- anova.red.full$Deviance[2])
(df_diff <- anova.red.full$Df[2])
qchisq(1 - 0.05, df_diff)
pchisq(D_diff, df_diff, lower.tail = FALSE)

#age^2
redmodel <- update(fullmodel, .~. -I(age^2))
(anova.red.full <- anova(redmodel, fullmodel))
(D_diff <- anova.red.full$Deviance[2])
(df_diff <- anova.red.full$Df[2])
qchisq(1 - 0.05, df_diff)
pchisq(D_diff, df_diff, lower.tail = FALSE)

#age + age^2
redmodel <- update(fullmodel, .~. -age -I(age^2))
(anova.red.full <- anova(redmodel, fullmodel))
(D_diff <- anova.red.full$Deviance[2])
(df_diff <- anova.red.full$Df[2])
qchisq(1 - 0.05, df_diff)
pchisq(D_diff, df_diff, lower.tail = FALSE)

#conclusion: remove exercise, worknorm, inctot


#### Step wise selection using AIC ####
model.stepAIC <- step(nullmodel, 
                      scope = list(lower = nullmodel, upper = fullmodel),
                      direction = "both")


ggplot(data, aes(x = work_norm, y = age)) + 
  geom_boxplot() + labs(title = "Box plot of work_norm vs age")



#### AIC Model ####
AICmodel <- glm(hosp ~ health + sex  + age + I(age^2) + civilst + inc_hh, 
                family = "binomial", data = data )


# Estimates and confidence intervals
beta <- AICmodel$coefficients
betas <- data.frame(beta)
betas$expbeta <- c(exp(beta))
betas$ci <- confint(AICmodel)
betas$expci <- exp(confint(AICmodel))
betas

estimates <- cbind(
  data.frame(beta),
  expbeta = c(exp(beta)),
  ci = confint.default(AICmodel),
  expci = exp(confint.default(AICmodel))
)
estimates
print(xtable(estimates, type = "latex"), file = "optmodel_estimates.tex")

# Pseudo R2, AIC, BIC
(lnL0 <- logLik(nullmodel)[1])

bic <- BIC(nullmodel, AICmodel)
aic <- AIC(nullmodel, AICmodel)
(collect <- data.frame(aic, bic))

collect$loglik <- 
  c(logLik(nullmodel)[1],
    logLik(AICmodel)[1])
collect$R2McF <- 1 - collect$loglik/lnL0
collect

print(xtable(collect, type = "latex"), file = "R2_AIC_BIC_opt.tex")

# Statistical test - full model vs AIC model
(anova.AIC.full <- anova(AICmodel, fullmodel))
(D_diff <- anova.AIC.full$Deviance[2])
(df_diff <- anova.AIC.full$Df[2])
qchisq(1 - 0.05, df_diff)
pchisq(D_diff, df_diff, lower.tail = FALSE)

# Full model is not significantly different from AIC model

#### Step wise selection using BIC ####
model.stepBIC <- step(nullmodel,
                      scope = list(lower = nullmodel, upper = fullmodel),
                      direction = "both", k = log(nrow(data)))
                                                  

#### BIC model ####
BICmodel <- glm(hosp ~ health + sex  + age + I(age^2), 
                family = "binomial", data = data )


# Estimates and confidence intervals
beta <- BICmodel$coefficients
betas <- data.frame(beta)
betas$expbeta <- c(exp(beta))
betas$ci <- confint(BICmodel)
betas$expci <- exp(confint(BICmodel))
betas

estimates <- cbind(
  data.frame(beta),
  expbeta = c(exp(beta)),
  ci = confint.default(BICmodel),
  expci = exp(confint.default(BICmodel))
)
estimates
print(xtable(estimates, type = "latex"), file = "BICmodel_estimates.tex")

# Pseudo R2, AIC, BIC
(lnL0 <- logLik(nullmodel)[1])

bic <- BIC(nullmodel, BICmodel)
aic <- AIC(nullmodel, BICmodel)
(collect <- data.frame(aic, bic))

collect$loglik <- 
  c(logLik(nullmodel)[1],
    logLik(BICmodel)[1])
collect$R2McF <- 1 - collect$loglik/lnL0
collect

print(xtable(collect, type = "latex"), file = "R2_AIC_BIC_BICmodel.tex")

# Statistical test - fullmodel vs BICmodel
(anova.BIC.full <- anova(BICmodel, fullmodel))
(D_diff <- anova.BIC.full$Deviance[2])
(df_diff <- anova.BIC.full$Df[2])
qchisq(1 - 0.05, df_diff)
pchisq(D_diff, df_diff, lower.tail = FALSE)

# BICmodel is significantly different from the full model

#### The model is nested - test BIC against AIC ####
(anova.BIC.AIC <- anova(BICmodel, AICmodel))
(D_diff <- anova.BIC.AIC$Deviance[2])
(df_diff <- anova.BIC.AIC$Df[2])
qchisq(1 - 0.05, df_diff)
pchisq(D_diff, df_diff, lower.tail = FALSE)

# BIC is significally different from AIC

#### Predict probabilies ####
bic.pred <- cbind(
  data,
  phat = predict(BICmodel, type = "response"))

# logit = logodds with s.e. for constructing C.I.
bic.pred <- cbind(
  bic.pred,
  logit = predict(BICmodel, se.fit = TRUE))
head(bic.pred)
# An unnecessary variable:
bic.pred$logit.residual.scale <- NULL

# Calculate confidence intervals for the log odds#
# standard normal quantile:
(lambda <- qnorm(1 - 0.05/2))
bic.pred$logit.lwr <- bic.pred$logit.fit - lambda*bic.pred$logit.se.fit
bic.pred$logit.upr <- bic.pred$logit.fit + lambda*bic.pred$logit.se.fit
head(bic.pred)

# transform the log-odds intervals into C.I. for odds#
bic.pred$odds.lwr <- exp(bic.pred$logit.lwr)
bic.pred$odds.upr <- exp(bic.pred$logit.upr)
head(bic.pred)

# transform the odds intervals into C.I. for p#
bic.pred$p.lwr <- bic.pred$odds.lwr/(1 + bic.pred$odds.lwr)
bic.pred$p.upr <- bic.pred$odds.upr/(1 + bic.pred$odds.upr)
head(bic.pred)

# plot the intervals:
ggplot(bic.pred, aes(x = age, y =hosp, fill = health)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = phat), color = "red", size = 0.5) +
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +
  facet_wrap(~sex) +
  xlab("Age") +
  ylab("Hospital") +
  labs(title = "Hospital 1+ days (=1) or Hospital 0 days (=0) vs age",
       caption = "red = fitted line, with 95% confidence interval") +
  theme(text = element_text(size = 10))

data$health <- relevel(data$health, ref = "Bad")

BICmodel.releveled <- glm(hosp ~ health + sex  + age + I(age^2), 
                family = "binomial", data = data )

summary(BICmodel.releveled)

#### Create a new health variable and replace health with this variable ####
data$health_2 <- data$health

data$health_2[data$health_2 == "Somewhere in between"] <- "Bad"

BIC.modified <- glm(hosp ~ age + I(age^2) + health_2 + sex, family = "binomial", data = data)

data$health_2 <- relevel(data$health_2, ref = "Good")


# Estimates and confidence intervals
beta <- BIC.modified$coefficients
betas <- data.frame(beta)
betas$expbeta <- c(exp(beta))
betas$ci <- confint(BIC.modified)
betas$expci <- exp(confint(BIC.modified))
betas

estimates <- cbind(
  data.frame(beta),
  expbeta = c(exp(beta)),
  ci = confint.default(BIC.modified),
  expci = exp(confint.default(BIC.modified))
)
estimates
print(xtable(estimates, type = "latex"), file = "BIC.modified_estimates.tex")

# Pseudo R2, AIC, BIC
(lnL0 <- logLik(nullmodel)[1])

bic <- BIC(nullmodel, BIC.modified)
aic <- AIC(nullmodel, BIC.modified)
(collect <- data.frame(aic, bic))

collect$loglik <- 
  c(logLik(nullmodel)[1],
    logLik(BIC.modified)[1])
collect$R2McF <- 1 - collect$loglik/lnL0
collect

print(xtable(collect, type = "latex"), file = "R2_AIC_BIC_BIC.modified.tex")




























## Influential observations ##

#### Calculate leverage ####
data2 <- cbind(
  data,
  v = influence(BIC.modified)$hat,
  xb = predict(BIC.modified)
)

#Plot 
(v.strange <- which(data2$v == max(data2$v)))
ggplot(data2, aes(x = age, y = v, color = hosp_cat)) + geom_jitter(width = 1) +
  facet_grid(health_2~sex) +
  geom_hline(yintercept = 0.004, color = "black", linetype = "dotted") +
  geom_hline(yintercept = 2*length(BIC.modified$coefficients)/nrow(data2), 
             color = "black") +
  geom_point(data = data2[v.strange, ], color = "black", size = 3, shape = 24) +
  labs(title = "Hospital: Leverage vs Age") +
  xlab("Age") + 
  ylab("Leverage") + 
  labs(caption = "y = 0.004 (dotted) and 1/5921 (filled)") +
  theme(text = element_text(size = 18))


#### Calculate standardized deviance residuals ####
data2 <- cbind(
  data2,
  sdres = influence(BIC.modified)$dev.res)

#Plot against linear predictor
ggplot(data2, aes(x = xb, y = sdres, color = hosp_cat)) + geom_jitter(width = 1) +
  facet_grid(health_2~sex) +
  geom_point(data = data2[v.strange, ], color = "black", size = 3, shape = 24) +
  geom_hline(yintercept = 2) +
  geom_hline(yintercept = -2) +
  labs(title = "Hospital: Standardized deviance residuals vs Linear predictor") +
  xlab("Linear predictor") + 
  ylab("Standardized deviance residuals") + 
  theme(text = element_text(size = 18))


#### Cook´s distance ####
data2$Dcook <- cooks.distance(BIC.modified)
head(data2)
(d.strange <- which(data2$Dcook == max(data2$Dcook)))

#Plot
ggplot(data2, aes(x = age, y = Dcook, color = hosp_cat)) + geom_jitter(width = 1) +
  facet_grid(health_2~sex) +
  geom_point(data = data2[v.strange, ], color = "black", size = 3, shape = 24) +
  geom_point(data = data2[d.strange, ], color = "red", size = 3, shape = 24) +
  labs(title = "Hospital: Cooks distance vs Age") +
  xlab("Age") + 
  ylab("Cooks distance") + 
  labs(caption = "Triangular in black = high leverage, Triangular in red = high Cook´s distance", size = 6) +
  theme(text = element_text(size = 18))


#### DFBETAS ####
data2$dfbetas <- dfbetas(BIC.modified)
head(data2)

#Plot
ggplot(data2, aes(x = age, y = dfbetas[,"(Intercept)"], color = hosp_cat)) + geom_jitter(width = 1) +
  facet_grid(health_2~sex) +
  labs(title = "Hospital: Dfbetas Intercept vs Age") +
  geom_hline(yintercept = 2/sqrt(5921)*c(-1, 1), color = "black") +
  geom_point(data = data2[v.strange, ], 
             color = "black", size = 4, shape = 24) +
  geom_point(data = data2[d.strange, ],
             color = "red", size = 4, shape = 24) + 
  xlab("Age") + 
  ylab("Dfbetas") + 
  labs(caption = "F_0.5, p+1, n-(p+1) (solid), black triangle = high leverage, red triangle = high Cook's distance") +
  theme(text = element_text(size = 18))

ggplot(data2, aes(x = age, y = dfbetas[,"age"], color = hosp_cat)) + geom_jitter(width = 1) +
  facet_grid(health_2~sex) +
  labs(title = "Hospital: Dfbetas Age vs Age") +
  geom_hline(yintercept = 2/sqrt(5921)*c(-1, 1), color = "black") +
  geom_point(data = data2[v.strange, ], 
             color = "black", size = 4, shape = 24) +
  geom_point(data = data2[d.strange, ],
             color = "red", size = 4, shape = 24) + 
  xlab("Age") + 
  ylab("Dfbetas") + 
  labs(caption = "F_0.5, p+1, n-(p+1) (solid), black triangle = high leverage, red triangle = high Cook's distance") +
  theme(text = element_text(size = 18))

ggplot(data2, aes(x = age, y = dfbetas[,"I(age^2)"], color = hosp_cat)) + geom_jitter(width = 1) +
  facet_grid(health_2~sex) +
  labs(title = "Hospital: Dfbetas Age^2 vs Age") +
  geom_hline(yintercept = 2/sqrt(5921)*c(-1, 1), color = "black") +
  geom_point(data = data2[v.strange, ], 
             color = "black", size = 4, shape = 24) +
  geom_point(data = data2[d.strange, ],
             color = "red", size = 4, shape = 24) + 
  xlab("Age") + 
  ylab("Dfbetas") + 
  labs(caption = "F_0.5, p+1, n-(p+1) (solid), black triangle = high leverage, red triangle = high Cook's distance") +
  theme(text = element_text(size = 18))

ggplot(data2, aes(x = age, y = dfbetas[,"health_2Good"], color = hosp_cat)) + geom_jitter(width = 1) +
  facet_grid(health_2~sex) +
  labs(title = "Hospital: Dfbetas Health Good vs Age") +
  geom_hline(yintercept = 2/sqrt(5921)*c(-1, 1), color = "black") +
  geom_point(data = data2[v.strange, ], 
             color = "black", size = 4, shape = 24) +
  geom_point(data = data2[d.strange, ],
             color = "red", size = 4, shape = 24) + 
  xlab("Age") + 
  ylab("Dfbetas") + 
  labs(caption = "F_0.5, p+1, n-(p+1) (solid), black triangle = high leverage, red triangle = high Cook's distance") +
  theme(text = element_text(size = 18))

ggplot(data2, aes(x = age, y = dfbetas[,"sexMale"], color = hosp_cat)) + geom_jitter(width = 1) +
  facet_grid(health_2~sex) +
  labs(title = "Hospital: Dfbetas Sex: Male vs Age") +
  geom_hline(yintercept = 2/sqrt(5921)*c(-1, 1), color = "black") +
  geom_point(data = data2[v.strange, ], 
             color = "black", size = 4, shape = 24) +
  geom_point(data = data2[d.strange, ],
             color = "red", size = 4, shape = 24) + 
  xlab("Age") + 
  ylab("Dfbetas") + 
  labs(caption = "F_0.5, p+1, n-(p+1) (solid), black triangle = high leverage, red triangle = high Cook's distance") +
  theme(text = element_text(size = 18))


## Goodness-of-fit ##

#### Confusion matrix ####
nullmodel <- glm(hosp ~ 1, family = "binomial", data = data) 
model.age.sq <- glm(hosp ~ age + I(age^2), family = "binomial", data = data) 
model.health <- glm(hosp ~ health, family = "binomial", data = data)
AICmodel <- glm(hosp ~ health + sex  + age + I(age^2) + civilst + inc_hh, 
                family = "binomial", data = data )
BICmodel3 <- glm(hosp ~ health + sex  + age + I(age^2), 
                family = "binomial", data = data )
BICmodel2 <- glm(hosp ~ health_2 + sex  + age + I(age^2), 
                    family = "binomial", data = data )

# estimate p_i using all the different models:

pred.phat <- cbind(
  data,
  p.0 = predict(nullmodel, type = "response"),
  p.1 = predict(model.health, type = "response"),
  p.2 = predict(model.age.sq, type = "response"),
  p.3 = predict(AICmodel, type = "response"),
  p.4 = predict(BICmodel3, type = "response"),
  p.5 = predict(BICmodel2, type = "response"))
  
head(pred.phat)

# Confusion matrix for AIC model####
# Calculate Y-hat using AIC model

pred.phat$yhat.AIC <- as.numeric(pred.phat$p.3 > 0.5)

pred.phat$yhat.AIC.cat <- factor(pred.phat$yhat.AIC,
                                 levels = c(0,1),
                                 labels = c("Failure", "Success"))

(row.01 <- table(data$hosp))

(col.01.AIC <- table(pred.phat$yhat.AIC.cat))
(confusion.AIC <- table(pred.phat$hosp, pred.phat$yhat.AIC.cat))
(spec.AIC <- confusion.AIC[1, 1] / row.01[1])
(sens.AIC <- confusion.AIC[2, 2] / row.01[2])
(accu.AIC <- sum(diag(confusion.AIC)) / sum(confusion.AIC))
(prec.AIC <- confusion.AIC[2, 2] / col.01.AIC[2])

print(xtable(confusion.AIC, type = "latex"), file = "confusion_part1.tex")

#### ROC ####
library(pROC)
# Calculate ROC for all models
# save the coordinates in a data frame for plotting.
(roc.null <- roc(hosp ~ p.0, data = pred.phat))
roc.df.null <- coords(roc.null, transpose = FALSE)
roc.df.null$model <- "null"

(roc.health <- roc(hosp ~ p.1, data = pred.phat))
roc.df.health <- coords(roc.health, transpose = FALSE)
roc.df.health$model <- "health"

(roc.age.sq <- roc(hosp ~ p.2, data = pred.phat))
roc.df.age.sq <- coords(roc.age.sq, transpose = FALSE)
roc.df.age.sq$model <- "age squared"

(roc.AIC <- roc(hosp ~ p.3, data = pred.phat))
roc.df.AIC <- coords(roc.AIC, transpose = FALSE)
roc.df.AIC$model <- "AIC"

(roc.BIC3 <- roc(hosp ~ p.4, data = pred.phat))
roc.df.BIC3 <- coords(roc.BIC3, transpose = FALSE)
roc.df.BIC3$model <- "BIC3"

(roc.BIC2 <- roc(hosp ~ p.5, data = pred.phat))
roc.df.BIC2 <- coords(roc.BIC2, transpose = FALSE)
roc.df.BIC2$model <- "BIC2"

# Bind all RIC
roc.df <- rbind(roc.df.null, roc.df.health, roc.df.age.sq, roc.df.AIC, 
                roc.df.BIC3, roc.df.BIC2)

# Plot all the curves, in different colors:
ggplot(roc.df, aes(specificity, sensitivity,
                   color = model)) +
  geom_path(size = 0.5) +
  coord_fixed() +       
  scale_x_reverse() + 
  labs(title = "ROC-curves for all the models") +
  theme(text = element_text(size = 14))


# AUC for all models####
roc.null
auc(roc.null)
roc.health
auc(roc.health)
roc.age.sq
auc(roc.age.sq)
roc.AIC
auc(roc.AIC)
roc.BIC2
auc(roc.BIC2)
roc.BIC3
auc(roc.BIC3)
# Confidence interval for AUC
(ci.null <- ci(roc.null))
(ci.health <- ci(roc.health))
(ci.age.sq <- ci(roc.age.sq))
(ci.AIC <- ci(roc.AIC))
(ci.BIC3 <- ci(roc.BIC3))
(ci.BIC2 <- ci(roc.BIC2))

#Collect AUC and intervals for all the models:
(aucs <- 
    data.frame(
      model = c("null", "health", "age^2", "AIC", "BIC3", "BIC2"),
      auc = c(auc(roc.null), auc(roc.health), auc(roc.age.sq), auc(roc.AIC),
              auc(roc.BIC3), auc(roc.BIC2)),
      lwr = c(ci(roc.null)[1], ci(roc.health)[1],
              ci(roc.age.sq)[1], ci(roc.AIC)[1],
              ci(roc.BIC3)[1], ci(roc.BIC2)[1]),
      upr = c(ci(roc.null)[3], ci(roc.health)[3],
              ci(roc.age.sq)[3], ci(roc.AIC)[3],
              ci(roc.BIC3)[3], ci(roc.BIC2)[3])))

print(xtable(aucs, type = "latex"), file = "table_aucs.tex")

# Compare the AUC for the models:
roc.test(roc.null, roc.AIC)
roc.test(roc.health, roc.AIC)
roc.test(roc.age.sq, roc.AIC)
roc.test(roc.BIC3, roc.AIC)
roc.test(roc.BIC2, roc.AIC)

### Optimal threshold ####
# experiment with different values of levels for sens and spec, level.ss
level.ss <- 0.6255
roc.df.AIC[roc.df.AIC$sensitivity > level.ss & 
           roc.df.AIC$specificity > level.ss, ]
##
(I_max.AIC <- which(roc.df.AIC$sensitivity > level.ss & 
                    roc.df.AIC$specificity > level.ss))
roc.df.AIC[I_max.AIC, ]
# Pick out the corresponding threshold for p:
opt.threshold.AIC <- roc.df.AIC[I_max.AIC, "threshold"]

#### Updated confusion matrix ####
pred.phat$yhat.AIC <- as.numeric(pred.phat$p.3 > opt.threshold.AIC)

(row.01 <- table(data$hosp))

(col.01.AIC <- table(pred.phat$yhat.AIC))
(confusion.AIC <- table(pred.phat$hosp, pred.phat$yhat.AIC))
(spec.AIC <- confusion.AIC[1, 1] / row.01[1])
(sens.AIC <- confusion.AIC[2, 2] / row.01[2])
(accu.AIC <- sum(diag(confusion.AIC)) / sum(confusion.AIC))
(prec.AIC <- confusion.AIC[2, 2] / col.01.AIC[2])

print(xtable(confusion.AIC, type = "latex"), file = "confusion_new.tex")


#### Hosmer-Lemeshow goodness-of-fit AIC #### 
# It can then be used to sort the data frame:
pred.sort <- pred.phat[order(pred.phat$p.3), ]
pred.sort$rank <- seq(1, nrow(pred.sort))
head(pred.sort)

# Divide the n observations into g groups:
(n <- nrow(pred.sort))
g <- 20
ng <- n/g

# Plot
ggplot(pred.sort, aes(rank, p.3)) +
  geom_point(size = 0.3) +
  geom_jitter(aes(y = hosp), height = 0.01, size = 0.2) +
  geom_vline(xintercept = seq(ng, nrow(pred.sort) - ng, ng), linetype = "dashed", color = "darkgray") +
  labs(title = "Model AIC: Estimated probabilities by increasing size",
       caption = "g = 12 groups",
       x = "(i) = 1,...,n", y = "p-hat") +
  theme(text = element_text(size = 14))

# A for-loop to set the group numbers:
pred.sort$group <- NA
for (k in seq(1, g)) {
  I <- (k - 1)*ng + seq(1, ng)
  pred.sort$group[I] <- k
}
head(pred.sort)

# Number of expected success
OE1 <- merge(aggregate(hosp ~ group, data = pred.sort, FUN = sum),
             aggregate(p.3 ~ group, data = pred.sort, FUN = sum))
OE1
# Number of failures = n_g - successes:
OE0 <- OE1
OE0$hosp <- ng - OE1$hosp
OE0$p.3 <- ng - OE1$p.3
OE0
# A variable to use for color coding:
OE1$outcome <- "Y = 1"
OE0$outcome <- "Y = 0"
# Bind the two data sets as rows (r):
(OE <- rbind(OE1, OE0))

# Plot 
ggplot(OE, aes(group, p.3, color = outcome)) +
  geom_line(aes(linetype = "expected"), size = 0.7) +
  geom_line(aes(y = hosp, linetype = "observed"), size = 1) +
  labs(title = "Model AIC: Observed and expected in each group",
       y = "number of observations") +
  theme(text = element_text(size = 14)) +
  scale_x_continuous(breaks = seq(1, g))

# The test:
(chi2HL <- sum((OE$hosp - OE$p.3)^2/OE$p.3))
# chi2-quantile to compare with:
chi.quant <- qchisq(1 - 0.05, g - 2)
# or P-value:
p.value <- pchisq(chi2HL, g - 2, lower.tail = FALSE)

(HL_test <- cbind(chi2HL, chi.quant, p.value))

print(xtable(HL_test, type = "latex"), file = "HL_Test_g20.tex")
# n = 5921, g = 12 (ng = 493.4), g = 16 (ng = 370,1), g = 20 (ng = 296,1)

## Using hoslem.test
library(ResourceSelection)
(HL.AIC <- hoslem.test(pred.sort$hosp, pred.sort$p.3, g = g))
HL.AIC$expected

print(xtable(HL.AIC$expected, type = "latex"), file = "HL_AIC_Expected.tex")

# Collect the data in a useful form for plotting:
(HL.df.AIC <- data.frame(group = seq(1, g),
                       Obs.0 = HL.AIC$observed[, 1],
                       Obs.1 = HL.AIC$observed[, 2],
                       Exp.0 = HL.AIC$expected[, 1],
                       Exp.1 = HL.AIC$expected[, 2]))

ggplot(HL.df.AIC, aes(x = group)) +
  geom_line(aes(y = Obs.0, linetype = "observed", color = "Y = 0"), size = 0.8) +
  geom_line(aes(y = Obs.1, linetype = "observed", color = "Y = 1"), size = 0.8) +
  geom_line(aes(y = Exp.0, linetype = "expected", color = "Y = 0"), size = 0.8) +
  geom_line(aes(y = Exp.1, linetype = "expected", color = "Y = 1"), size = 0.8) +
  labs(title = "Model AIC: Observed and expected in each group",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, g)) +
  theme(text = element_text(size = 14))

# How many observations in each group?
(HL.df.AIC$Obs.0 + HL.df.AIC$Obs.1)

# g = 12 obs = 493, g = 16 obs = 370, g = 20 obs = 296