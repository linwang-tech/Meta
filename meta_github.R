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


###plot mean effect by the results get from above code
library(ggplot2)
me <- read_excel("C:/.../mean_effect.xlsx", sheet = "mean_effect")

me_filtered <- na.omit(me)

y_breaks <- seq(0, 9, 1)
y_labels <- c("Label0", "Electrical conductivity (EC)", "Sodium adsorption ratio (SAR)", "Exchangeable sodium percentage (ESP)", "pH", "Soil organic carbon (SOC)",
              "Saturated hydraulic conductivity (Ks)", "Total porosity(TP)", "Bulk density (BD)", "Aggregate stability (AS)")

p1 <- ggplot(data = me_filtered, aes(x = me, y = y)) +
  labs(x = "Mean effect(%)", y = "") +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmax = max, xmin = min, height = .2)) +
  geom_text(aes(label = paste(o, n, sep = " / ")), hjust = -0.8, vjust = 0.4, size = 4.5, color = "black") +
  theme_classic() +  
  scale_y_continuous(breaks = y_breaks, labels = y_labels) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  xlim(-50, 260) +
  theme(
    axis.text.y = element_text(face = "bold"),
    axis.title.x = element_text(size = 11), # Adjust size as needed
    axis.title.y = element_text(size = 15)  # Adjust size as needed
  )

Figure_2 <- p1


#####plot mean effect according to TWW and soil classification
library(cowplot)
tww <- read_excel("C:/.../mean_effect.xlsx", sheet = "TWW")

p2 <- ggplot(data = tww, aes(x = me, y = y, color = x)) +
  labs(x = "Mean effect (%)", y = "", size = 3) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmax = max, xmin = min, height = .2)) +
  geom_text(aes(label = paste(o, n, sep = " / ")), hjust = 0.6, vjust = -0.65, size = 3.5, color = "black") +
  theme_classic() +
  theme(axis.line.y = element_blank(),  # Remove y-axis line
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(color = "black", linetype = "solid")) +  # Move x-axis to the top
  xlim(-149, 300)+
  scale_y_continuous(breaks = seq(0, 45, 5)) +
  geom_vline(aes(xintercept = 0), linetype = "solid") +
  geom_vline(aes(xintercept = -50), linetype = "dashed", color = "grey") +
  geom_vline(aes(xintercept = -100), linetype = "dashed", color = "grey") +
  geom_vline(aes(xintercept = 50), linetype = "dashed", color = "grey") +
  geom_vline(aes(xintercept = 100), linetype = "dashed", color = "grey") +
  geom_vline(aes(xintercept = 150), linetype = "dashed", color = "grey") +
  geom_vline(aes(xintercept = 200), linetype = "dashed", color = "grey") +
  geom_vline(aes(xintercept = 250), linetype = "dashed", color = "grey") +
  geom_vline(aes(xintercept = 300), linetype = "dashed", color = "grey") +
  geom_hline(aes(yintercept = 5), linetype = "dashed", color = "grey") +
  geom_hline(aes(yintercept = 10), linetype = "dashed", color = "grey") +
  geom_hline(aes(yintercept = 15), linetype = "dashed", color = "grey") +
  geom_hline(aes(yintercept = 20), linetype = "dashed", color = "grey") +
  geom_hline(aes(yintercept = 25), linetype = "dashed", color = "grey") +
  geom_hline(aes(yintercept = 30), linetype = "dashed", color = "grey") +
  geom_hline(aes(yintercept = 35), linetype = "dashed", color = "grey") +
  geom_hline(aes(yintercept = 40), linetype = "dashed", color = "grey") +
  scale_color_manual(
    values = c("Mean" = "black", "PTW" = "red", "STW" = "orange", "TTW" = "yellow"),
    name = "TWW class") +
  scale_x_continuous(position = "top", breaks = seq(-100, 300, 50))

p2 <- p2 + theme(legend.position = "top", legend.box = "horizontal",
                 legend.text = element_text(size = 10, face= "bold"),
                 legend.title = element_text(size = 10,face="bold"),
                 axis.text.x = element_text(size = 10, face = "bold"),
                 axis.title.x = element_text(size = 10, face = "bold"))
p2


p2<- p2 + geom_text(data = tww, aes(x = -140, y = 2.5, label = "EC"), color = "black")+
  geom_text(data = tww, aes(x = -140, y = 7.5, label = "SAR"), color = "black")+
  geom_text(data = tww, aes(x = -140, y = 12.5, label = "ESP"), color = "black")+
  geom_text(data = tww, aes(x = -140, y = 17.5, label = "pH"), color = "black")+
  geom_text(data = tww, aes(x = -140, y = 22.5, label = "SOC"), color = "black")+
  geom_text(data = tww, aes(x = -140, y = 27.5, label = "Ks"), color = "black")+
  geom_text(data = tww, aes(x = -140, y = 32.5, label = "TP"), color = "black")+
  geom_text(data = tww, aes(x = -140, y = 37.5, label = "BD"), color = "black")+  
  geom_text(data = tww, aes(x = -140, y = 42.5, label = "AS"), color = "black")


soil <- read_excel("C:/.../mean_effect.xlsx", sheet = "Soil")
soil

p3 <- ggplot(data = soil, aes(x = me, y = y, color = x)) +
  labs(x = "Mean effect (%)", y = "", size = 3, face = "bold") +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmax = max, xmin = min, height = .2)) +
  geom_text(aes(label = paste(o, n, sep = " / ")), hjust = 0.6, vjust = -0.65, size = 3.5, color = "black") +
  theme_classic() +
  theme(axis.line.y = element_blank(),  # Remove y-axis line
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(color = "black", linetype = "solid")) +  # Move x-axis to the top
  xlim(-149, 300)+
  scale_y_continuous(breaks = seq(0, 45, 5)) +
  geom_vline(aes(xintercept = 0), linetype = "solid") +
  geom_vline(aes(xintercept = -50), linetype = "dashed", color = "grey") +
  geom_vline(aes(xintercept = -100), linetype = "dashed", color = "grey") +
  geom_vline(aes(xintercept = 50), linetype = "dashed", color = "grey") +
  geom_vline(aes(xintercept = 100), linetype = "dashed", color = "grey") +
  geom_vline(aes(xintercept = 150), linetype = "dashed", color = "grey") +
  geom_vline(aes(xintercept = 200), linetype = "dashed", color = "grey") +
  geom_vline(aes(xintercept = 250), linetype = "dashed", color = "grey") +
  geom_vline(aes(xintercept = 300), linetype = "dashed", color = "grey") +
  geom_hline(aes(yintercept = 5), linetype = "dashed", color = "grey") +
  geom_hline(aes(yintercept = 10), linetype = "dashed", color = "grey") +
  geom_hline(aes(yintercept = 15), linetype = "dashed", color = "grey") +
  geom_hline(aes(yintercept = 20), linetype = "dashed", color = "grey") +
  geom_hline(aes(yintercept = 25), linetype = "dashed", color = "grey") +
  geom_hline(aes(yintercept = 30), linetype = "dashed", color = "grey") +
  geom_hline(aes(yintercept = 35), linetype = "dashed", color = "grey") +
  geom_hline(aes(yintercept = 40), linetype = "dashed", color = "grey") +
  scale_color_manual(
    values = c("Mean" = "black", "Fine" = "red", "Medium" = "orange", "Coarse" = "yellow"),
    name = "Soil class")  + # Specify your own colors
  scale_x_continuous(position = "top", breaks = seq(-100, 300, 50))

p3 <- p3 + theme(legend.position = "top", legend.box = "horizontal",
                 legend.text = element_text(size = 10, face= "bold"),
                 legend.title = element_text(size = 10, face="bold"),
                 axis.text.x = element_text(size = 10, face = "bold"),
                 axis.title.x = element_text(size = 10, face = "bold")) # Make x-axis labels bigger and bold
p3

p3 <- p3 + geom_text(data = soil, aes(x = -140, y = 2.5, label = "EC"), color = "black")+
  geom_text(data = soil, aes(x = -140, y = 7.5, label = "SAR"), color = "black")+
  geom_text(data = soil, aes(x = -140, y = 12.5, label = "ESP"), color = "black")+
  geom_text(data = soil, aes(x = -140, y = 17.5, label = "pH"), color = "black")+
  geom_text(data = soil, aes(x = -140, y = 22.5, label = "SOC"), color = "black")+
  geom_text(data = soil, aes(x = -140, y = 27.5, label = "Ks"), color = "black")+
  geom_text(data = soil, aes(x = -140, y = 32.5, label = "TP"), color = "black")+
  geom_text(data = soil, aes(x = -140, y = 37.5, label = "BD"), color = "black")+  
  geom_text(data = soil, aes(x = -140, y = 42.5, label = "AS"), color = "black")


Figure_3 <- p2+p3


