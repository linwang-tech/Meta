library(readxl)
library(dplyr)
library(data.table)
library("meta")
library("metafor")
library(readxl)
as <- read_excel("C:/.../meta_github.xlsx", sheet = "AS")####load right place and target sheet

###Calculate RR_effect_sizes
RR_effect_sizes <- escalc( # Function for calculating effect sizes.
  "ROM",
  # Specify the effect size we want to calculate. In this case ROM for the log of Ratio of Means
  m1i = m1,# mean of soil indicators of treatment
  n1i = n1,# treatment sample size
  sd1i = sd1, # treatment SD
  m2i = m2,# mean of soil indicators of control
  n2i = n2, # control sample size
  sd2i = sd2,# control SD
  data = as # This is where the escalc function can find all the data for our meta-analysis
)

data.table(RR_effect_sizes, fillContainer = TRUE)

##### Random model
random_effect_model_results <- rma(yi, #yi = lnT - lnC, this is the effect size from each row in database
                                   vi, # measure of variance from each row in database
                                   method = "REML", # Using  a REML estimator which is common for random effect meta-analyses
                                   slab = paste(study, year, sep = ""),
                                   # subset=(tww=="2"), ###pick subset for tww or soil
                                   #soil=="Fine-textured", 
                                   #soil=="Medium-textured",
                                   # soil=="Coarse-textured"
                                   data = RR_effect_sizes) # Let R know the dataframe we'll use for our model calculations

random_effect_model_results



####display random model results as a percent of change
lnRR <- coef(random_effect_model_results)
normal_percent <- 100 * (exp(lnRR)-1)
se_percent <-  100 * (exp(summary(random_effect_model_results)$se) - 1)

upper_bound <- normal_percent + 1.96*sqrt(se_percent)
lower_bound <- normal_percent - 1.96*sqrt(se_percent)

cat("Normal percent change:", normal_percent, "\n")
cat("Lower bound (96% CI):", lower_bound, "\n")
cat("Upper bound (96% CI):", upper_bound, "\n")



