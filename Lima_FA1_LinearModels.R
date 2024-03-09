#FA 1 - Paul Carlos T. Lima I

################## FOR SCENARIO 1 ###################################
model <- lm(Index ~ Perceptions.of.corruption, data = WHR2023)
summary(model)

#Can be found in the summary
beta_zero <- coef(model)[1]
beta_zero
beta_one <- coef(model)[2]
beta_one

var_beta_zero <- vcov(model)[1, 1]
var_beta_zero
var_beta_one <- vcov(model)[2, 2]
var_beta_one

#Can be Found in the summary (For Checking)
mean_ei <- mean(resid(model))
mean_ei

#Can be Found in the summary (For Checking)
R_squared <- summary(model)$r.squared
R_squared

# For Visual Representation
plot(WHR2023$Perceptions.of.corruption, WHR2023$Index, xlab = "Perceptions of Corruption", ylab = "Happiness Index", main = "Scatterplot of SLM: Happiness Index vs. Perception of Corruption")
abline(model, col = "red")
#OR
plot(Index ~ Perceptions.of.corruption, data=WHR2023, col="black", pch = 1, xlab = "Perceptions of Corruption", ylab = "Happiness Index", main = "Scatterplot of SLM: Happiness Index vs. Perception of Corruption")
abline(model, col = "red")

################## FOR SCENARIO 2 ###################################

model_scenario2 <- lm(Index ~ Healthy.life.expectancy, data = WHR2023)
summary(model_scenario2)

#Can be found in the summary
beta_zero <- coef(model_scenario2)[1]
beta_zero
beta_one <- coef(model_scenario2)[2]
beta_one

var_beta_zero <- vcov(model_scenario2)[1, 1]
var_beta_zero
var_beta_one <- vcov(model_scenario2)[2, 2]
var_beta_one

#Can be Found in the summary (For Checking)
mean_ei <- mean(resid(model_scenario2))
mean_ei

#Can be Found in the summary (For Checking)
R_squared <- summary(model_scenario2)$r.squared
R_squared

# For Visual Representation
plot(WHR2023$Healthy.life.expectancy, WHR2023$Index, xlab = "Healthy Life Expectancy", ylab = "Happiness Index", main = "Scatterplot of SLM: Happiness Index vs. Perception of Corruption")
abline(model_scenario2, col = "blue")
#OR
plot(Index ~ Healthy.life.expectancy, data=WHR2023, col="black", pch = 18, xlab = "Healthy Life Expectancy", ylab = "Happiness Index", main = "Scatterplot of SLM: Happiness Index vs. Healthy Life Expectancy")
abline(model_scenario2, col = "blue")

