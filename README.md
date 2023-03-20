# Project 2: Logistic Regression - Cardivascular Diseases in Sweden
Project 1 in the course FMSN40 Linear and Logisitc regression with data collection, spring of 2022. Lund School of Engineering
Contributors: Sofia Larsson, Sofie Thulin and Joel Bengs.

Code can be found in the folder "R".

Data can be found in the folder "Data"

# Abstract (from report)
This report investigates which factors affect the plasma beta-carotene levels in the body. After constructing two models, one lin-lin model and one log-lin model, it was found that the log-lin model was the better model after analyzing the residuals. After that, a background model and a dietary model was constructed and later combined into one model. The variables included in the final model were vitamin use, calories, fiber, betadiet, quetelet, smokstat, sex and age. Vitamin use, calories, quetelet, former smoker, current smoker and male were variables that decreased the plasma beta-carotene levels and fiber, betadiet and age increased the plasma beta-carotene levels. 24.4 % of the variability in the used data set can be explained by the final model.

# Conclusion (from report) 
First, the relationship between hospitalization and health was investigated and a strong relationship between general health status and hospitalization was found; the probability of someone with bad health spending at least one day in the hospital was twice as large as someone with good health.

Furthermore, the relationship between hospitalization and age was studied and it was established that age had a quadratic relationship with hospitalization. The probability of spending at least one day in the hospital increased with age until the age of 80, from when the probability started to decrease with age.

The remainder of variables available in the data set were then investigated. Frequency tables for each of the categorical variables were used to determine which category should be chosen as the reference variable. For the continuous variables, the correlation was studied and it was found some had a reasonably strong
correlation, but when constructing a logistic regression of the Full Model, it was decided to include all variables, including the two different income-variables. Partial likelihood ratio tests for all variables concluded that exercise, normal working hours per week and individual income were not significant variables.

A step wise selection using AIC as criterion was performed and the resulting model included sex, general health status, civil status, household income, age and quadratic age. Although, by performing a likelihood test it was found that the AIC was not significantly different from the Full Model. Furthermore, a step
wise selection using BIC as criterion was performed and the resulting model was in this case found to be significant different from both the Full Model and the AIC Model. The model included sex, health, age and age squared. A new model with these variables was constructed, but the three health categories were merged
into only two categories of health: good and bad, since bad and somewhere in between could be considered the same. This model had a lower BIC and AIC than the BIC Model with three health categories, and therefore, the BIC-2 Model is considered to be the best model so far.

Further on a goodness-of-fit analysis was done, by calculating ROC-curves and AUC for the different models. The analysis concluded that the AIC, BIC-2 and BIC-3 Model is significantly better that the other models, since the AUC are significantly bigger. Although, the AIC, BIC-2 and BIC-3 are not necessarily different from each other. The conclusion from this is that the ideal model should include sex, age, age squared and some type of health.

Finally, a Hosmer-Lemeshow test on the AIC Model was performed, and an optimal threshold value was decided for. The Hosmer-Lemeshow test shows that the differences are small, and we can reject the null hypothesis of there being differences. Therefore, the AIC-model could be considered a correct model. A good model should contain as few variables as possible, whilst representing the real world sufficiently well. Since the BIC-2 Model performs just as well as the AIC Model and the BIC-3 Model in the goodness-of-fit analysis, whilst containing fewer variables, it should be considered to be the optimal model.
