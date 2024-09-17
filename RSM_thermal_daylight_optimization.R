## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----echo=FALSE, results='hide',message=FALSE------------------------------------------------------------------------------------------------------------------
setwd("...")


## ----fractional design-----------------------------------------------------------------------------------------------------------------------------------------
suppressPackageStartupMessages(library(FrF2))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
factors <- read.csv("final_factors.csv", sep=';', header = FALSE)
colnames(factors) <- c("Factor", "-1", "+1")
factors


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
frac_design_rhino <- FrF2(nfactors = 8, 
                          randomize = FALSE,
                          resolution = 5,
                          default.levels = c(0, 1),
                          factor.names=c("WWR_N","WWR_S","WWR_E","WWR_W","RoofOvrhng_N",
                                 "RoofOvrhng_S","RoofOvrhng_E","RoofOvrhng_W"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
frac_design_rhino <- as.data.frame(frac_design_rhino)
head(frac_design_rhino)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
nrow(frac_design_rhino)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
write.csv(frac_design_rhino, "frac_design_2k8_R5.csv")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
frac_design_confirm <- FrF2(nfactors = 8, 
                      randomize = FALSE,
                      nruns = 64,
                      default.levels = c(0, 1),
                      factor.names=c("WWR_N","WWR_S","WWR_E","WWR_W","RoofOvrhng_N",
                                 "RoofOvrhng_S","RoofOvrhng_E","RoofOvrhng_W"))
nrow(frac_design_confirm)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
frac_design_rhino <- FrF2(nfactors = 8, 
                          randomize = FALSE,
                          resolution = 5,
                          default.levels = c(-1, 1),
                          factor.names=c("WWR_N","WWR_S","WWR_E","WWR_W","RoofOvrhng_N",
                                 "RoofOvrhng_S","RoofOvrhng_E","RoofOvrhng_W"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
head(frac_design_rhino)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
data_ioh <- read.csv("data_IOH_64.csv")
data_da <- read.csv("data_DA_64.csv")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
df_ioh <- as.data.frame(data_ioh)
df_da <- as.data.frame(data_da)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
frac_design_rhino$IOH  <- df_ioh$out.IOH 
frac_design_rhino$UDI  <- df_da$out.UDI
frac_design_rhino$DA  <- df_da$out.DA 
frac_design_rhino$cDA  <- df_da$out.cDA
frac_design_rhino$sDA50  <- df_da$out.sDA50
frac_design_rhino$sDA80  <- df_da$out.sDA80



## --------------------------------------------------------------------------------------------------------------------------------------------------------------
df = frac_design_rhino
head(df)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Assuming 'IOH' is the column name in your dataframe 'df'
hist(df$IOH, main="Histogram with Density Curve", xlab="IOH Values", col="lightblue", border="black", probability=TRUE)

# Add a density curve
lines(density(df$IOH), col="darkred", lwd=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Assuming 'UDI' is the column name in your dataframe 'df'
hist(df$UDI, main="Histogram with Density Curve", xlab="sDA Values", col="lightblue", border="black", probability=TRUE)

# Add a density curve
lines(density(df$UDI), col="darkred", lwd=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
ks.test(df$IOH, "pnorm")
ks.test(df$UDI, "pnorm")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
cor.test(df$IOH, df$UDI, method="spearman")
cor.test(df$IOH, df$DA, method="spearman")
cor.test(df$IOH, df$cDA, method="spearman")
cor.test(df$IOH, df$sDA50, method="spearman")
cor.test(df$IOH, df$sDA80, method="spearman")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------

cor.test(df$UDI, df$DA, method="spearman")
cor.test(df$UDI, df$cDA, method="spearman")
cor.test(df$UDI, df$sDA50, method="spearman")



## --------------------------------------------------------------------------------------------------------------------------------------------------------------
min(df$IOH)
max(df$IOH)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
min(df$UDI)
max(df$UDI)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
min(df$sDA50)
max(df$sDA50)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
min(df$sDA80)
max(df$sDA80)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
predOutcomes <- data.frame(
  ioh = df$IOH,
  udi = df$UDI
)
head(predOutcomes)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
library(desirability)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
iohD <- dMin(0, 0.193161, scale = 1)
udiD <- dMax(35.23524, 100, scale = 1)
overallD <- dOverall(iohD, udiD)
head(overallD)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
predict(iohD, predOutcomes[1])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
predict(udiD, predOutcomes[2])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
predict(overallD, predOutcomes)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
overall_desirability <- predict(overallD, predOutcomes, all = TRUE)
overall_desirability <- as.data.frame(overall_desirability)
head(overall_desirability)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
df$d1 <- overall_desirability$D1
df$d2 <- overall_desirability$D2
df$Overall <- overall_desirability$Overall
df


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Assuming df is your dataframe containing the variables IOH, sDA, and Overall
library(ggplot2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Scatterplot with hue based on Overall
ggplot(df, aes(x = IOH, y = UDI, color = Overall)) +
  geom_point(size = 3) +  # Set the size of points to 3 (adjust as needed)
  labs(x = "Indoor Overheating Hours (IOH)", y = "Useful Daylight Illuminance (UDI)", color = "Overall Desirability") +
  scale_color_gradient2(low = "red", high = "blue", mid = "grey", midpoint = 0.5, guide = "legend") +
  scale_x_continuous(breaks = seq(0, 0.2, by = 0.02), limits = c(0, 0.2)) +  # Set x-axis ticks from 0 to 20
  scale_y_continuous(breaks = seq(30, 100, by = 5), limits = c(30, 100)) +  # Set y-axis ticks from 40 to 100
  theme_minimal()  +
  theme(
    panel.background = element_rect(fill = "white"),  # Set background color to white
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Show axis lines
    axis.title.x = element_text(size = 14),  # Increase font size of x-axis title
    axis.title.y = element_text(size = 14),  # Increase font size of y-axis title
    axis.text.x = element_text(size = 12),  # Increase font size of x-axis tick labels
    axis.text.y = element_text(size = 12),   # Increase font size of y-axis tick labels
    axis.ticks = element_line(color = "black") # Show axis ticks
  )


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Calculate the density of the 'Overall' column
density_values <- density(df$Overall)

# Assuming 'Overall' is the column name in your dataframe 'df'
hist(df$Overall, 
     main="Histogram with Density Curve", 
     xlab="Overall Desirability Values", 
     col="lightblue", 
     border="black", 
     probability=TRUE, 
     ylim=c(0, max(density_values$y)))

# Add a density curve
lines(density(df$Overall), col="darkred", lwd=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
min(df$Overall)
max(df$Overall)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
cor.test(df$Overall, df$UDI, method="kendall")
cor.test(df$Overall, df$IOH, method="kendall")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
library(MASS)
library(dplyr)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Specify the variables to drop
variables_to_drop <- c("d1", "d2", "DA", "cDA", "sDA50", "sDA80")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Drop the specified variables from the dataframe
df_sw <- df[, !names(df) %in% variables_to_drop]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
library(corrplot)

# Convert selected features to numeric, handling non-numeric values
df_sw[] <- lapply(df_sw, function(col) {
  as.numeric(as.character(col))
})

# Calculate the correlation matrix
correlation_matrix <- cor(df_sw, method="spearman", use = "pairwise.complete.obs")
correlation_matrix

# Initialize an empty matrix to store p-values
p_value_matrix <- matrix(NA, nrow = ncol(df_sw), ncol = ncol(df_sw))
rownames(p_value_matrix) <- colnames(df_sw)
colnames(p_value_matrix) <- colnames(df_sw)

# Calculate the p-values for each pair of variables
for(i in 1:ncol(df_sw)) {
  for(j in 1:ncol(df_sw)) {
    if(i <= j) {
      test_result <- cor.test(df_sw[[i]], df_sw[[j]], method = "kendall", use = "pairwise.complete.obs")
      p_value_matrix[i, j] <- test_result$p.value
      p_value_matrix[j, i] <- test_result$p.value
    }
  }
}


p_value_matrix

# Open a PNG device
png("correlogram.png", height = 5, width = 5, units = "in", res = 300)


# Plot the correlogram using corrplot
corrplot(correlation_matrix, method = "circle", type = "upper",  tl.col = "black", tl.srt = 45, addCoef.col = 1,  number.cex = 0.6, tl.cex = 0.75, number.font = 1, p.mat = p_value_matrix, sig.level = 0.05, insig = "blank", addgrid.col="white")


# Close the PNG device
dev.off()



## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Specify the variables to drop
variables_to_drop <- c("UDI", "IOH")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Drop the specified variables from the dataframe
df_sw <- df_sw[, !names(df_sw) %in% variables_to_drop]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
#F1
fullModel = lm(Overall ~ ., data = df_sw) 
nullModel = lm(Overall ~ 1, data = df_sw)
n = nrow(df_sw)

stepwise_model <- lm(Overall ~ 1, data = df_sw) %>%
  stepAIC(trace = FALSE, direction = "both", scope = list(upper = fullModel, lower = nullModel), k = log(n))

summary(stepwise_model)
stepwise_model$anova
model_summary <- summary(stepwise_model)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Extract p-values from the model coefficients
p_values <- coef(model_summary)[, "Pr(>|t|)"]

# Apply FDR adjustment using the Benjamini-Hochberg (BH) method
adjusted_p_values <- p.adjust(p_values, method = "BH")

# Identify significant coefficients based on FDR-adjusted p-values (threshold: 0.05)
significant <- adjusted_p_values < 0.05

# Display summary with only significant coefficients
significant_coefficients <- coef(model_summary)[significant, ]
print(significant_coefficients)

# Print the adjusted R-squared value of the model
cat("Adjusted R-squared:", summary(stepwise_model)$adj.r.squared, "\n")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
resst=rstandard(stepwise_model)
ks.test(resst, "pnorm")
shapiro.test(resst)
plot(stepwise_model, 2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(stepwise_model, 3)

bartlett.test(Overall ~ RoofOvrhng_S, data = df_sw)
bartlett.test(Overall ~ WWR_E, data = df_sw)
bartlett.test(Overall ~ RoofOvrhng_W, data = df_sw)
bartlett.test(Overall ~ WWR_N, data = df_sw)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
library(glmnet)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
Y <- data.matrix(df[, c("Overall")])
x <- data.matrix(df[, c("RoofOvrhng_N", "RoofOvrhng_S", "RoofOvrhng_E", "RoofOvrhng_W", "WWR_N", "WWR_S", "WWR_E", "WWR_W")])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
Y


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
x


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)  # Set seed for reproducibility


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
cv_model <- cv.glmnet(x, Y, alpha = 1, family = "gaussian", nfolds = 3)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
best_lambda <- cv_model$lambda.min
best_lambda


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
best_lambda2 <- cv_model$lambda.1se
best_lambda2


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(cv_model) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
best_model <- glmnet(x, Y, alpha = 1, lambda = best_lambda2, family = "gaussian")
coef(best_model)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
library(rsm)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg1_fo <- cube(3, n0=3, randomize=FALSE)
dsg1_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
as.data.frame(dsg1_fo)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
WWR_N_random <- rnorm(11, mean = 0, sd=2)
WWR_E_random <- rnorm(11, mean = 0, sd=2)
RoofOvrhng_N_random <- rnorm(11, mean = 0, sd=2)
RoofOvrhng_E_random <- rnorm(11, mean = 0, sd=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
write.csv(WWR_N_random, "WWR_N_random.csv")
write.csv(WWR_E_random, "WWR_E_random.csv")
write.csv(RoofOvrhng_N_random, "RoofOvrhng_N_random.csv")
write.csv(RoofOvrhng_E_random, "RoofOvrhng_E_random.csv")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
WWR_N_random <- read.csv('randomNumbers/dsg1_fo/WWR_N_random.csv')
WWR_E_random <- read.csv('randomNumbers/dsg1_fo/WWR_E_random.csv')
RoofOvrhng_N_random <- read.csv('randomNumbers/dsg1_fo/RoofOvrhng_N_random.csv')
RoofOvrhng_E_random <- read.csv('randomNumbers/dsg1_fo/RoofOvrhng_E_random.csv')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg1_fo$WWR_N <- ((WWR_N_random$x * 2) + 15)/100
dsg1_fo$WWR_E <- ((WWR_E_random$x * 2) + 15)/100
dsg1_fo$RoofOvrhng_N <- ((RoofOvrhng_N_random$x * 2) + 50)/100
dsg1_fo$RoofOvrhng_E <- ((RoofOvrhng_E_random$x * 2) + 50)/100


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg1_fo <- as.data.frame(dsg1_fo)
write.csv(dsg1_fo, "dsg_fo_1.csv")
dsg1_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
ioh <- read.csv('CCD/FO_data_ioh_1.csv')
sda <- read.csv('CCD/FO_data_udi_1.csv')

ioh <- as.data.frame(ioh)
sda <- as.data.frame(sda)

print(ioh)
print(sda)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg1_fo$IOH  <- ioh$out.IOH 
dsg1_fo$sDA  <- sda$out.UDI
dsg1_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
predOutcomes <- data.frame(
  ioh = dsg1_fo$IOH,
  sda = dsg1_fo$sDA
)
predOutcomes


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
library(desirability)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
iohD <- dMin(0, 0.193161, scale = 1)
sdaD <- dMax(35.23524, 100, scale = 1)
overallD <- dOverall(iohD, sdaD)
head(overallD)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
predict(iohD, predOutcomes[1])
predict(sdaD, predOutcomes[2])
predict(overallD, predOutcomes)
overall_desirability <- predict(overallD, predOutcomes, all = TRUE)
overall_desirability <- as.data.frame(overall_desirability)
overall_desirability


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg1_fo$d1 <- overall_desirability$D1
dsg1_fo$d2 <- overall_desirability$D2
dsg1_fo$Overall <- overall_desirability$Overall
dsg1_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg1_fo = subset(dsg1_fo, select = -c(run.order, std.order,RoofOvrhng_N, RoofOvrhng_E, WWR_N, WWR_E))
dsg1_fo <- coded.data(
  dsg1_fo, formulas = list(x1 ~ (RoofOvrhng_S_W - 2.5)/.5,
                  x2 ~ (WWR_W - 0.15)/.1,
                  x3 ~ (WWR_S - 0.4)/.1)
  )

dsg1_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
anal1_fo <- rsm(Overall ~ FO(x1, x2, x3), data = dsg1_fo)
summary(anal1_fo)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
steepest(anal1_fo, descent = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
steepest(anal1_fo, dist = (1:12)/8, descent = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg2_fo = dupe(steepest(anal1_fo, dist = (1:12)/8, descent = FALSE),randomize=FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg2_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg2_fo <- data.frame(
  run.order = dsg2_fo$run.order,
  std.order = dsg2_fo$std.order,
  dist = dsg2_fo$dist,
  x1 = dsg2_fo$x1,
  x2 = dsg2_fo$x2,
  x3 = dsg2_fo$x3
)

dsg2_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
WWR_N_random <- rnorm(12, mean = 0, sd=2)
WWR_E_random <- rnorm(12, mean = 0, sd=2)
RoofOvrhng_N_random <- rnorm(12, mean = 0, sd=2)
RoofOvrhng_E_random <- rnorm(12, mean = 0, sd=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
write.csv(WWR_N_random, "WWR_N_random.csv")
write.csv(WWR_E_random, "WWR_E_random.csv")
write.csv(RoofOvrhng_N_random, "RoofOvrhng_N_random.csv")
write.csv(RoofOvrhng_E_random, "RoofOvrhng_E_random.csv")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
WWR_N_random <- read.csv('randomNumbers/dsg2_fo/WWR_N_random.csv')
WWR_E_random <- read.csv('randomNumbers/dsg2_fo/WWR_E_random.csv')
RoofOvrhng_N_random <- read.csv('randomNumbers/dsg2_fo/RoofOvrhng_N_random.csv')
RoofOvrhng_E_random <- read.csv('randomNumbers/dsg2_fo/RoofOvrhng_E_random.csv')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg2_fo$WWR_N <- ((WWR_N_random$x * 2) + 15)/100
dsg2_fo$WWR_E <- ((WWR_E_random$x * 2) + 15)/100
dsg2_fo$RoofOvrhng_N <- ((RoofOvrhng_N_random$x * 2) + 50)/100
dsg2_fo$RoofOvrhng_E <- ((RoofOvrhng_E_random$x * 2) + 50)/100


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg2_fo <- as.data.frame(dsg2_fo)
write.csv(dsg2_fo, "dsg_fo_2.csv")
dsg2_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
ioh <- read.csv('CCD/FO_data_ioh_2.csv')
sda <- read.csv('CCD/FO_data_udi_2.csv')

ioh <- as.data.frame(ioh)
sda <- as.data.frame(sda)

print(ioh)
print(sda)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg2_fo$IOH  <- ioh$out.IOH 
dsg2_fo$sDA  <- sda$out.UDI
dsg2_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
predOutcomes <- data.frame(
  ioh = dsg2_fo$IOH,
  sda = dsg2_fo$sDA
)
predOutcomes


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
library(desirability)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
iohD <- dMin(0, 0.193161, scale = 1)
sdaD <- dMax(35.23524, 100, scale = 1)
overallD <- dOverall(iohD, sdaD)
head(overallD)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
predict(iohD, predOutcomes[1])
predict(sdaD, predOutcomes[2])
predict(overallD, predOutcomes)
overall_desirability <- predict(overallD, predOutcomes, all = TRUE)
overall_desirability <- as.data.frame(overall_desirability)
overall_desirability


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg2_fo$d1 <- overall_desirability$D1
dsg2_fo$d2 <- overall_desirability$D2
dsg2_fo$Overall <- overall_desirability$Overall
dsg2_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a scatterplot
p <- ggplot(dsg2_fo, aes(x = dist, y = Overall)) +
  geom_point() +
  
  # Fit a quadratic regression model
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
              se = FALSE, color = "blue") +
  
  # Add labels and a title
  labs(x = "Distance (in coded units)", y = "Overall Desirability (D)", title = "Design of Experiment 2 (DoE 2)") 


ggsave("experiment 2 plot.png", plot= p, height = 5 , width = 5)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
p


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg3_fo <- cube(3, n0=3, randomize=FALSE)
dsg3_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
as.data.frame(dsg3_fo)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
WWR_N_random <- rnorm(11, mean = 0, sd=2)
WWR_E_random <- rnorm(11, mean = 0, sd=2)
RoofOvrhng_N_random <- rnorm(11, mean = 0, sd=2)
RoofOvrhng_E_random <- rnorm(11, mean = 0, sd=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
write.csv(WWR_N_random, "WWR_N_random.csv")
write.csv(WWR_E_random, "WWR_E_random.csv")
write.csv(RoofOvrhng_N_random, "RoofOvrhng_N_random.csv")
write.csv(RoofOvrhng_E_random, "RoofOvrhng_E_random.csv")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
WWR_N_random <- read.csv('randomNumbers/dsg3_fo/WWR_N_random.csv')
WWR_E_random <- read.csv('randomNumbers/dsg3_fo/WWR_E_random.csv')
RoofOvrhng_N_random <- read.csv('randomNumbers/dsg3_fo/RoofOvrhng_N_random.csv')
RoofOvrhng_E_random <- read.csv('randomNumbers/dsg3_fo/RoofOvrhng_E_random.csv')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg3_fo$WWR_N <- ((WWR_N_random$x * 2) + 15)/100
dsg3_fo$WWR_E <- ((WWR_E_random$x * 2) + 15)/100
dsg3_fo$RoofOvrhng_N <- ((RoofOvrhng_N_random$x * 2) + 50)/100
dsg3_fo$RoofOvrhng_E <- ((RoofOvrhng_E_random$x * 2) + 50)/100


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg3_fo <- as.data.frame(dsg3_fo)
write.csv(dsg3_fo, "dsg_fo_3.csv")
dsg3_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
ioh <- read.csv('CCD/FO_data_ioh_3.csv')
sda <- read.csv('CCD/FO_data_udi_3.csv')

ioh <- as.data.frame(ioh)
sda <- as.data.frame(sda)

print(ioh)
print(sda)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg3_fo$IOH  <- ioh$out.IOH 
dsg3_fo$sDA  <- sda$out.UDI
dsg3_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
predOutcomes <- data.frame(
  ioh = dsg3_fo$IOH,
  sda = dsg3_fo$sDA
)
predOutcomes


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
library(desirability)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
iohD <- dMin(0, 0.193161, scale = 1)
sdaD <- dMax(35.23524, 100, scale = 1)
overallD <- dOverall(iohD, sdaD)
head(overallD)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
predict(iohD, predOutcomes[1])
predict(sdaD, predOutcomes[2])
predict(overallD, predOutcomes)
overall_desirability <- predict(overallD, predOutcomes, all = TRUE)
overall_desirability <- as.data.frame(overall_desirability)
overall_desirability


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg3_fo$d1 <- overall_desirability$D1
dsg3_fo$d2 <- overall_desirability$D2
dsg3_fo$Overall <- overall_desirability$Overall
dsg3_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg3_fo = subset(dsg3_fo, select = -c(run.order, std.order,RoofOvrhng_N, RoofOvrhng_E, WWR_N, WWR_E))
dsg3_fo <- coded.data(
  dsg3_fo, formulas = list(x1 ~ (RoofOvrhng_S_W - 2.9580)/.5,
                  x2 ~ (WWR_W - 0.0505)/.025,
                  x3 ~ (WWR_S - 0.3353)/.1)
  )

dsg3_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
anal3_fo <- rsm(Overall ~ FO(x1, x2, x3), data = dsg3_fo)
summary(anal3_fo)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
steepest(anal3_fo, descent = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
steepest(anal3_fo, dist = (1:12)/8, descent = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg4_fo = dupe(steepest(anal3_fo, dist = (1:12)/8, descent = FALSE),randomize=FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg4_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg4_fo <- data.frame(
  run.order = dsg4_fo$run.order,
  std.order = dsg4_fo$std.order,
  dist = dsg4_fo$dist,
  x1 = dsg4_fo$x1,
  x2 = dsg4_fo$x2,
  x3 = dsg4_fo$x3
)

dsg4_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
WWR_N_random <- rnorm(12, mean = 0, sd=2)
WWR_E_random <- rnorm(12, mean = 0, sd=2)
RoofOvrhng_N_random <- rnorm(12, mean = 0, sd=2)
RoofOvrhng_E_random <- rnorm(12, mean = 0, sd=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
write.csv(WWR_N_random, "WWR_N_random.csv")
write.csv(WWR_E_random, "WWR_E_random.csv")
write.csv(RoofOvrhng_N_random, "RoofOvrhng_N_random.csv")
write.csv(RoofOvrhng_E_random, "RoofOvrhng_E_random.csv")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
WWR_N_random <- read.csv('randomNumbers/dsg4_fo/WWR_N_random.csv')
WWR_E_random <- read.csv('randomNumbers/dsg4_fo/WWR_E_random.csv')
RoofOvrhng_N_random <- read.csv('randomNumbers/dsg4_fo/RoofOvrhng_N_random.csv')
RoofOvrhng_E_random <- read.csv('randomNumbers/dsg4_fo/RoofOvrhng_E_random.csv')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg4_fo$WWR_N <- ((WWR_N_random$x * 2) + 15)/100
dsg4_fo$WWR_E <- ((WWR_E_random$x * 2) + 15)/100
dsg4_fo$RoofOvrhng_N <- ((RoofOvrhng_N_random$x * 2) + 50)/100
dsg4_fo$RoofOvrhng_E <- ((RoofOvrhng_E_random$x * 2) + 50)/100


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg4_fo <- as.data.frame(dsg4_fo)
write.csv(dsg4_fo, "dsg_fo_4.csv")
dsg4_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
ioh <- read.csv('CCD/FO_data_ioh_4.csv')
sda <- read.csv('CCD/FO_data_udi_4.csv')

ioh <- as.data.frame(ioh)
sda <- as.data.frame(sda)

print(ioh)
print(sda)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg4_fo$IOH  <- ioh$out.IOH 
dsg4_fo$sDA  <- sda$out.UDI
dsg4_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
predOutcomes <- data.frame(
  ioh = dsg4_fo$IOH,
  sda = dsg4_fo$sDA
)
predOutcomes


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
library(desirability)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
iohD <- dMin(0, 0.193161, scale = 1)
sdaD <- dMax(35.23524, 100, scale = 1)
overallD <- dOverall(iohD, sdaD)
head(overallD)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
predict(iohD, predOutcomes[1])
predict(sdaD, predOutcomes[2])
predict(overallD, predOutcomes)
overall_desirability <- predict(overallD, predOutcomes, all = TRUE)
overall_desirability <- as.data.frame(overall_desirability)
overall_desirability


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg4_fo$d1 <- overall_desirability$D1
dsg4_fo$d2 <- overall_desirability$D2
dsg4_fo$Overall <- overall_desirability$Overall
dsg4_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a scatterplot
p <- ggplot(dsg4_fo, aes(x = dist, y = Overall)) +
  geom_point() +
  
  # Fit a quadratic regression model
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
              se = FALSE, color = "blue") +
  
  # Add labels and a title
  labs(x = "Distance (in coded units)", y = "Overall Desirability (D)", title = "Design of Experiment 4 (DoE 4)") 


ggsave("experiment 4 plot.png", plot= p, height = 5 , width = 5)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
p


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg5_fo <- cube(3, n0=3, randomize=FALSE)
dsg5_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
as.data.frame(dsg5_fo)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
WWR_N_random <- rnorm(11, mean = 0, sd=2)
WWR_E_random <- rnorm(11, mean = 0, sd=2)
RoofOvrhng_N_random <- rnorm(11, mean = 0, sd=2)
RoofOvrhng_E_random <- rnorm(11, mean = 0, sd=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
write.csv(WWR_N_random, "WWR_N_random.csv")
write.csv(WWR_E_random, "WWR_E_random.csv")
write.csv(RoofOvrhng_N_random, "RoofOvrhng_N_random.csv")
write.csv(RoofOvrhng_E_random, "RoofOvrhng_E_random.csv")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
WWR_N_random <- read.csv('randomNumbers/dsg5_fo/WWR_N_random.csv')
WWR_E_random <- read.csv('randomNumbers/dsg5_fo/WWR_E_random.csv')
RoofOvrhng_N_random <- read.csv('randomNumbers/dsg5_fo/RoofOvrhng_N_random.csv')
RoofOvrhng_E_random <- read.csv('randomNumbers/dsg5_fo/RoofOvrhng_E_random.csv')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg5_fo$WWR_N <- ((WWR_N_random$x * 2) + 15)/100
dsg5_fo$WWR_E <- ((WWR_E_random$x * 2) + 15)/100
dsg5_fo$RoofOvrhng_N <- ((RoofOvrhng_N_random$x * 2) + 50)/100
dsg5_fo$RoofOvrhng_E <- ((RoofOvrhng_E_random$x * 2) + 50)/100


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg5_fo <- as.data.frame(dsg5_fo)
write.csv(dsg5_fo, "dsg_fo_5.csv")
dsg5_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
ioh <- read.csv('CCD/FO_data_ioh_5.csv')
sda <- read.csv('CCD/FO_data_udi_5.csv')

ioh <- as.data.frame(ioh)
sda <- as.data.frame(sda)

print(ioh)
print(sda)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg5_fo$IOH  <- ioh$out.IOH 
dsg5_fo$sDA  <- sda$out.UDI
dsg5_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
predOutcomes <- data.frame(
  ioh = dsg5_fo$IOH,
  sda = dsg5_fo$sDA
)
predOutcomes


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
library(desirability)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
iohD <- dMin(0, 0.193161, scale = 1)
sdaD <- dMax(35.23524, 100, scale = 1)
overallD <- dOverall(iohD, sdaD)
head(overallD)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
predict(iohD, predOutcomes[1])
predict(sdaD, predOutcomes[2])
predict(overallD, predOutcomes)
overall_desirability <- predict(overallD, predOutcomes, all = TRUE)
overall_desirability <- as.data.frame(overall_desirability)
overall_desirability


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg5_fo$d1 <- overall_desirability$D1
dsg5_fo$d2 <- overall_desirability$D2
dsg5_fo$Overall <- overall_desirability$Overall
dsg5_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg5_fo = subset(dsg5_fo, select = -c(run.order, std.order,RoofOvrhng_N, RoofOvrhng_E, WWR_N, WWR_E))
dsg5_fo <- coded.data(
  dsg5_fo, formulas = list(x1 ~ (RoofOvrhng_S_W - 3.5130)/.25,
                  x2 ~ (WWR_W - 0.047225)/.025,
                  x3 ~ (WWR_S - 0.2356)/.1)
  )

dsg5_fo


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
anal5_fo <- rsm(Overall ~ FO(x1, x2, x3), data = dsg5_fo)
summary(anal5_fo)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg1_so = ccd(3, n0 = c(0,3), randomize = FALSE, alpha = "rotatable",
            coding = c(x1 ~ (RoofOvrhng_S_W - 3.5130)/.25,
                  x2 ~ (WWR_W - 0.047225)/.025,
                  x3 ~ (WWR_S - 0.2356)/.1))
dsg1_so


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
as.data.frame(dsg1_so)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
WWR_N_random <- rnorm(17, mean = 0, sd=2)
WWR_E_random <- rnorm(17, mean = 0, sd=2)
RoofOvrhng_N_random <- rnorm(17, mean = 0, sd=2)
RoofOvrhng_E_random <- rnorm(17, mean = 0, sd=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
write.csv(WWR_N_random, "WWR_N_random.csv")
write.csv(WWR_E_random, "WWR_E_random.csv")
write.csv(RoofOvrhng_N_random, "RoofOvrhng_N_random.csv")
write.csv(RoofOvrhng_E_random, "RoofOvrhng_E_random.csv")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
WWR_N_random <- read.csv('randomNumbers/dsg1_so/WWR_N_random.csv')
WWR_E_random <- read.csv('randomNumbers/dsg1_so/WWR_E_random.csv')
RoofOvrhng_N_random <- read.csv('randomNumbers/dsg1_so/RoofOvrhng_N_random.csv')
RoofOvrhng_E_random <- read.csv('randomNumbers/dsg1_so/RoofOvrhng_E_random.csv')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg1_so$WWR_N <- ((WWR_N_random$x * 2) + 15)/100
dsg1_so$WWR_E <- ((WWR_E_random$x * 2) + 15)/100
dsg1_so$RoofOvrhng_N <- ((RoofOvrhng_N_random$x * 2) + 50)/100
dsg1_so$RoofOvrhng_E <- ((RoofOvrhng_E_random$x * 2) + 50)/100


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Specify the variables to drop
variables_to_drop <- c("Block")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Drop the specified variables from the dsg1_so
dsg1_so <- dsg1_so[, !names(dsg1_so) %in% variables_to_drop]
dsg1_so


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg1_so <- as.data.frame(dsg1_so)
write.csv(dsg1_so, "dsg_so_1.csv")
dsg1_so


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
ioh <- read.csv('CCD/SO_data_ioh_1.csv')
sda <- read.csv('CCD/SO_data_udi_1.csv')

ioh <- as.data.frame(ioh)
sda <- as.data.frame(sda)

print(ioh)
print(sda)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg1_so$IOH  <- ioh$out.IOH 
dsg1_so$sDA  <- sda$out.UDI
dsg1_so


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
predOutcomes <- data.frame(
  ioh = dsg1_so$IOH,
  sda = dsg1_so$sDA
)
predOutcomes


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
library(desirability)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
iohD <- dMin(0, 0.193161, scale = 1)
sdaD <- dMax(35.23524, 100, scale = 1)
overallD <- dOverall(iohD, sdaD)
head(overallD)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
predict(iohD, predOutcomes[1])
predict(sdaD, predOutcomes[2])
predict(overallD, predOutcomes)
overall_desirability <- predict(overallD, predOutcomes, all = TRUE)
overall_desirability <- as.data.frame(overall_desirability)
overall_desirability


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg1_so$d1 <- overall_desirability$D1
dsg1_so$d2 <- overall_desirability$D2
dsg1_so$Overall <- overall_desirability$Overall
dsg1_so


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsg1_so = subset(dsg1_so, select = -c(run.order, std.order,RoofOvrhng_N, RoofOvrhng_E, WWR_N, WWR_E))
dsg1_so <- coded.data(
  dsg1_so, formulas = list(x1 ~ (RoofOvrhng_S_W - 3.5130)/.25,
                  x2 ~ (WWR_W - 0.047225)/.025,
                  x3 ~ (WWR_S - 0.2356)/.1)
  )

dsg1_so


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
anal1_so <- rsm(Overall ~ SO(x1, x2, x3), data = dsg1_so)
summary(anal1_so)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow = c(1, 1), pty = 's', mar = c(5, 5, 2, 2))  # Adjust the margin as needed

# Define your custom color gradient
my_colors <- colorRampPalette(c("darkgreen","gold", "orange","red","white"))

# Generate the desired number of colors from this palette
nbcol <- 100  # Adjust the number of colors as needed
custom_color <- my_colors(nbcol)

contour(anal1_so, ~ x1 + x2, at = xs(anal1_so), image = TRUE, img.col = custom_color)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow = c(1, 1), pty = 's', mar = c(5, 5, 2, 2))  # Adjust the margin as needed

# Define your custom color gradient
my_colors <- colorRampPalette(c("darkgreen","gold", "orange","red","white"))

# Generate the desired number of colors from this palette
nbcol <- 100  # Adjust the number of colors as needed
custom_color <- my_colors(nbcol)

contour(anal1_so, ~ x2 + x3, at = xs(anal1_so), image = TRUE, img.col = custom_color)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow = c(1, 1), pty = 's', mar = c(5, 5, 2, 2))  # Adjust the margin as needed

# Define your custom color gradient
my_colors <- colorRampPalette(c("darkgreen","gold", "orange","red","white"))

# Generate the desired number of colors from this palette
nbcol <- 100  # Adjust the number of colors as needed
custom_color <- my_colors(nbcol)

contour(anal1_so, ~ x1 + x3, at = xs(anal1_so), image = TRUE, img.col = custom_color)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Define your custom color gradient
my_colors <- colorRampPalette(c("darkgreen","gold", "orange","red","white"))

# Generate the desired number of colors from this palette
nbcol <- 50  # Adjust the number of colors as needed
custom_color <- my_colors(nbcol)

# Open a PNG device
png("rsm_1_plot.png", height = 5, width = 5, units = "in", res = 300)

# Create the contour plot and save it directly to the file
persp(anal1_so, ~ x1 + x3, at = xs(anal1_so), col = custom_color, contours = "colors", zlab = "Overall Desirability", cex.axis = 0.7, border = NA)

# Close the PNG device
dev.off()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Define your custom color gradient
my_colors <- colorRampPalette(c("darkgreen","gold", "orange","red","white"))

# Generate the desired number of colors from this palette
nbcol <- 50  # Adjust the number of colors as needed
custom_color <- my_colors(nbcol)

# Open a PNG device
png("rsm_2_plot.png", height = 5, width = 5, units = "in", res = 300)

# Create the contour plot and save it directly to the file
persp(anal1_so, ~ x1 + x2, at = xs(anal1_so), col = custom_color, contours = "colors", zlab = "Overall Desirability", cex.axis = 0.7, border = NA)

# Close the PNG device
dev.off()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Define your custom color gradient
my_colors <- colorRampPalette(c("darkgreen","gold", "orange","red","white"))

# Generate the desired number of colors from this palette
nbcol <- 50  # Adjust the number of colors as needed
custom_color <- my_colors(nbcol)

# Open a PNG device
png("rsm_3_plot.png", height = 5, width = 5, units = "in", res = 300)

# Create the contour plot and save it directly to the file
persp(anal1_so, ~ x2 + x3, at = xs(anal1_so), col = custom_color, contours = "colors", zlab = "Overall Desirability", cex.axis = 0.7, border = NA)

# Close the PNG device
dev.off()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
summary_model5 <- summary(anal5_fo)
adj_r_squared5 <- summary_model5$adj.r.squared
sigma5 <- summary_model5$sigma
AIC5 <- AIC(anal5_fo)
BIC5 <- BIC(anal5_fo)
p_value5 <- summary_model5$coefficients["(Intercept)", "Pr(>|t|)"]
print(AIC5)
print(BIC5)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
summary_model6 <- summary(anal1_so)
adj_r_squared6 <- summary_model6$adj.r.squared
sigma6 <- summary_model6$sigma
AIC6 <- AIC(anal1_so)
BIC6 <- BIC(anal1_so)
p_value6 <- summary_model6$coefficients["(Intercept)", "Pr(>|t|)"]
print(AIC6)
print(BIC6)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Extracting Stationary Point in Original Units from your model summary
stationary_point <- c(RoofOvrhng_S_W = 3.7802643, WWR_W = 0.0375997, WWR_S = 0.2933929)

# Set seed for reproducibility
set.seed(123)  # You can use any seed value you like

# Simulating Bootstrap Replications
fits <- predict(anal1_so)
resids <- resid(anal1_so)


## ----echo=FALSE, results='hide',message=FALSE------------------------------------------------------------------------------------------------------------------
boot.raw <- replicate(1000, xs(update(anal1_so, fits + sample(resids, replace=TRUE) ~ .)))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Converting coded values to original units
boot <- code2val(as.data.frame(t(boot.raw)), codings = codings(anal1_so))

  
# Calculate confidence intervals
ci_95 <- function(x) {
  quantile(x, probs = c(0.025, 0.975))
}

ci_RoofOvrhng_S_W <- ci_95(boot$RoofOvrhng_S_W)
ci_WWR_W <- ci_95(boot$WWR_W)
ci_WWR_S <- ci_95(boot$WWR_S)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
print(paste("95% CI for RoofOvrhng_S_W:", ci_RoofOvrhng_S_W))
print(paste("95% CI for WWR_W:", ci_WWR_W))
print(paste("95% CI for WWR_S:", ci_WWR_S))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot for WWR_W vs. WWR_S with individual dots and ablines for confidence intervals
p <- ggplot(boot, aes(x = WWR_W, y = WWR_S)) +
  geom_point(alpha = 0.05, color = "black", size = 3) +  # Plot individual points with transparency
  geom_point(data = data.frame(WWR_W = stationary_point["WWR_W"], WWR_S = stationary_point["WWR_S"]), 
             aes(WWR_W, WWR_S), color = "red", shape = 17, size = 3) +  # Plot the stationary point
  geom_vline(xintercept = ci_WWR_W, color = "blue", linetype = "dashed") +  # Vertical lines for ci_WWR_W
  geom_hline(yintercept = ci_WWR_S, color = "blue", linetype = "dashed") +  # Horizontal lines for ci_WWR_S
  labs(x = "WWR_W", y = "WWR_S") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black"),  # Add back x and y ticks
    axis.text.x = element_text(size = 11),  # Increase x-axis tick label size
    axis.text.y = element_text(size = 11)   # Increase y-axis tick label size
  ) +
  #theme(aspect.ratio = 1) +
  coord_cartesian(xlim = c(0.00, 0.10), ylim = c(0.25, 0.35)) +
  scale_x_continuous(breaks = seq(0.00, 0.10, by = 0.05)) +
  scale_y_continuous(breaks = seq(0.25, 0.35, by = 0.01))

ggsave("robustness 1 plot.png", plot= p, height = 5 , width = 2.5)


p


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot for RoofOvrhng_S_W vs. WWR_S with individual dots and ablines for confidence intervals
p <- ggplot(boot, aes(x = RoofOvrhng_S_W, y = WWR_S)) +
  geom_point(alpha = 0.05, color = "black", size = 3) +  # Plot individual points with transparency
  geom_point(data = data.frame(RoofOvrhng_S_W = stationary_point["RoofOvrhng_S_W"], WWR_S = stationary_point["WWR_S"]), 
             aes(RoofOvrhng_S_W, WWR_S), color = "red", shape = 17, size = 3) +  # Plot the stationary point
  geom_vline(xintercept = ci_RoofOvrhng_S_W, color = "blue", linetype = "dashed") +  # Vertical lines for ci_RoofOvrhng_S_W
  geom_hline(yintercept = ci_WWR_S, color = "blue", linetype = "dashed") +  # Horizontal lines for ci_WWR_S
  labs(x = "RoofOvrhng_S_W", y = "WWR_S") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black"),  # Add back x and y ticks
    axis.text.x = element_text(size = 11),  # Increase x-axis tick label size
    axis.text.y = element_text(size = 11)   # Increase y-axis tick label size
  ) +
  #theme(aspect.ratio = 1) +
  coord_cartesian(xlim = c(3.00, 4.20), ylim = c(0.25, 0.35)) +
  scale_x_continuous(breaks = seq(3.00, 4.20, by =0.2)) +
  scale_y_continuous(breaks = seq(0.25, 0.35, by = 0.01))

ggsave("robustness 2 plot.png", plot= p, height = 5 , width = 2.5)


p


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot for RoofOvrhng_S_W vs. WWR_W with individual dots and ablines for confidence intervals
p <- ggplot(boot, aes(x = RoofOvrhng_S_W, y = WWR_W)) +
  geom_point(alpha = 0.05, color = "black", size = 3) +  # Plot individual points with transparency
  geom_point(data = data.frame(RoofOvrhng_S_W = stationary_point["RoofOvrhng_S_W"], WWR_W = stationary_point["WWR_W"]), 
             aes(RoofOvrhng_S_W, WWR_W), color = "red", shape = 17, size = 3) +  # Plot the stationary point
  geom_vline(xintercept = ci_RoofOvrhng_S_W, color = "blue", linetype = "dashed") +  # Vertical lines for ci_RoofOvrhng_S_W
  geom_hline(yintercept = ci_WWR_W, color = "blue", linetype = "dashed") +  # Horizontal lines for ci_WWR_W
  labs(x = "RoofOvrhng_S_W", y = "WWR_W") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black"),  # Add back x and y ticks
    axis.text.x = element_text(size = 11),  # Increase x-axis tick label size
    axis.text.y = element_text(size = 11)   # Increase y-axis tick label size
  ) +
  #theme(aspect.ratio = 1) +
  coord_cartesian(xlim = c(3.00, 4.20), ylim = c(0.00, 0.10)) +
  scale_x_continuous(breaks = seq(3.00, 4.20, by = 0.20)) +
  scale_y_continuous(breaks = seq(0.00, 0.10, by = 0.01))

ggsave("robustness 3 plot.png", plot= p, height = 5 , width = 2.5)


p


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
opt_point <- summary(anal1_so)$canonical$xs
opt_point


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
op_point_ru <- code2val(
  opt_point,                     # Optimal point in coded units
  codings = codings(anal1_so)  # Formulas to convert to factor units
)

op_point_ru


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
opt_point_df <- data.frame(  # predict() needs a data frame with the points 
  x1 = opt_point[1],         # to be predicted 
  x2 = opt_point[2],
  x3 = opt_point[3]
  )

best_response <- predict(
  anal1_so,             # Our model
  opt_point_df             # Data frame with points to be predicted 
  )

names(best_response) <- "Best Overall" # A nice name to our best point

best_response


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
WWR_N_random <- rnorm(1, mean = 0, sd=2)
WWR_E_random <- rnorm(1, mean = 0, sd=2)
RoofOvrhng_N_random <- rnorm(1, mean = 0, sd=2)
RoofOvrhng_E_random <- rnorm(1, mean = 0, sd=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
write.csv(WWR_N_random, "WWR_N_random.csv")
write.csv(WWR_E_random, "WWR_E_random.csv")
write.csv(RoofOvrhng_N_random, "RoofOvrhng_N_random.csv")
write.csv(RoofOvrhng_E_random, "RoofOvrhng_E_random.csv")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
WWR_N_random <- read.csv('randomNumbers/dsgOpt_so/WWR_N_random.csv')
WWR_E_random <- read.csv('randomNumbers/dsgOpt_so/WWR_E_random.csv')
RoofOvrhng_N_random <- read.csv('randomNumbers/dsgOpt_so/RoofOvrhng_N_random.csv')
RoofOvrhng_E_random <- read.csv('randomNumbers/dsgOpt_so/RoofOvrhng_E_random.csv')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsgOpt_so <- data.frame(t(opt_point))
colnames(dsgOpt_so) <- c("RoofOvrhng_S_W ", "WWR_W", "WWR_S")
dsgOpt_so


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsgOpt_so$WWR_N <- ((WWR_N_random$x * 2) + 15)/100
dsgOpt_so$WWR_E <- ((WWR_E_random$x * 2) + 15)/100
dsgOpt_so$RoofOvrhng_N <- ((RoofOvrhng_N_random$x * 2) + 50)/100
dsgOpt_so$RoofOvrhng_E <- ((RoofOvrhng_E_random$x * 2) + 50)/100


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsgOpt_so <- as.data.frame(dsgOpt_so)
write.csv(dsgOpt_so, "dsg_so_Opt.csv")
dsgOpt_so


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
ioh <- read.csv('CCD/SO_data_ioh_Opt.csv')
sda <- read.csv('CCD/SO_data_udi_Opt.csv')

ioh <- as.data.frame(ioh)
sda <- as.data.frame(sda)

print(ioh)
print(sda)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsgOpt_so$IOH  <- ioh$out.IOH 
dsgOpt_so$sDA  <- sda$out.UDI
dsgOpt_so


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
predOutcomes <- data.frame(
  ioh = dsgOpt_so$IOH,
  sda = dsgOpt_so$sDA
)
predOutcomes


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
library(desirability)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
iohD <- dMin(0, 0.193161, scale = 1)
sdaD <- dMax(35.23524, 100, scale = 1)
overallD <- dOverall(iohD, sdaD)
head(overallD)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
predict(iohD, predOutcomes[1])
predict(sdaD, predOutcomes[2])
predict(overallD, predOutcomes)
overall_desirability <- predict(overallD, predOutcomes, all = TRUE)
overall_desirability <- as.data.frame(overall_desirability)
overall_desirability


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
dsgOpt_so$d1 <- overall_desirability$D1
dsgOpt_so$d2 <- overall_desirability$D2
dsgOpt_so$Overall <- overall_desirability$Overall
dsgOpt_so


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Select only the required columns and add an identifier column to each dataframe
df_subset <- df[, c("IOH", "UDI", "Overall")]
df_subset$identifier <- "Fractional factorial design (n = 64)"
dsg1_fo_subset <- dsg1_fo[, c("IOH", "sDA", "Overall")]
dsg1_fo_subset$identifier <- "Response surface designs (n = 74)"
dsg2_fo_subset <- dsg2_fo[, c("IOH", "sDA", "Overall")]
dsg2_fo_subset$identifier <- "Response surface designs (n = 74)"
dsg3_fo_subset <- dsg3_fo[, c("IOH", "sDA", "Overall")]
dsg3_fo_subset$identifier <- "Response surface designs (n = 74)"
dsg4_fo_subset <- dsg4_fo[, c("IOH", "sDA", "Overall")]
dsg4_fo_subset$identifier <- "Response surface designs (n = 74)"
dsg5_fo_subset <- dsg5_fo[, c("IOH", "sDA", "Overall")]
dsg5_fo_subset$identifier <- "Response surface designs (n = 74)"
dsg1_so_subset <- dsg1_so[, c("IOH", "sDA", "Overall")]
dsg1_so_subset$identifier <- "Response surface designs (n = 74)"

# Replace the column name "UDI" with "sDA" in df_subset
colnames(df_subset)[colnames(df_subset) == "UDI"] <- "sDA"


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Concatenate the dataframes
df_combined <- rbind(df_subset, dsg1_fo_subset, dsg2_fo_subset, dsg3_fo_subset, dsg4_fo_subset, dsg5_fo_subset, dsg1_so_subset)

# View the combined dataframe
print(df_combined)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Open a PNG device
png("scatterplot.png", height = 5, width = 7.5, units = "in", res = 300)

# Adding an optimal point (black triangle) annotation
optimal_point <- data.frame(IOH = 0.083288, sDA = 79.66763)

# Plotting
ggplot(df_combined, aes(x = IOH, y = sDA, color = Overall, shape = identifier)) +
  geom_point(size = 2, alpha = 0.7) +
  annotate("point", x = optimal_point$IOH, y = optimal_point$sDA, color = "black", shape = 17, size = 3) +
  labs(x = "Indoor Overheating Hours (IOH)", y = "Useful Daylight Illuminance (UDI)", color = "Overall Desirability", shape = "Design Type") +
  scale_color_gradient2(low = "red", high = "blue", mid = "grey", midpoint = 0.5) +
  scale_x_continuous(breaks = seq(0.075, 0.2, by = 0.025), limits = c(0.075, 0.2)) +
  scale_y_continuous(breaks = seq(30, 85, by = 5), limits = c(30, 85)) +
  scale_shape_manual(name = "Design Type", 
                     values = c("Fractional factorial design (n = 64)" = 15, "Response surface designs (n = 74)" = 10, "Optimal Point" = 17)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.ticks = element_line(color = "black")
  )

# Close the PNG device
dev.off()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
library(htmlwidgets)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
library(plotly)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
# Sample data
WWR_W <- boot$WWR_W
WWR_S <- boot$WWR_S
RoofOvrhng_S_W <- boot$RoofOvrhng_S_W

# Stationary point values
stationary_point <- data.frame(
  RoofOvrhng_S_W = 3.7802643,
  WWR_W = 0.0375997,
  WWR_S = 0.2933929
)

# Create 3D plot
p <- plot_ly() %>%
  add_trace(
    x = ~WWR_W, y = ~WWR_S, z = ~RoofOvrhng_S_W, type = 'scatter3d', mode = 'markers',
    marker = list(size = 3, color = 'black', opacity = 0.15)
  ) %>%
  add_trace(
    x = stationary_point$WWR_W, y = stationary_point$WWR_S, z = stationary_point$RoofOvrhng_S_W,
    type = 'scatter3d', mode = 'markers',
    marker = list(size = 5, color = 'red', symbol = 'cross')
  ) %>%
  layout(
    scene = list(
      xaxis = list(title = 'WWR_W'),
      yaxis = list(title = 'WWR_S'),
      zaxis = list(title = 'RoofOvrhng_S_W')
    )
  )


# Save the plot as an HTML file
saveWidget(p, file = "3D_plot.html")
