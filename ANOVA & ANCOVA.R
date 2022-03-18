# Read/Load the data: '.csv file' (comma-separated values file)
HATCOdata = read.csv(file.choose(),header=TRUE)
View(HATCOdata)
str(HATCOdata)

'One Way ANOVA 
--------------'
'Is there a difference in the type of buy situation between purchaser with 
respect to Satisfaction level?'

#X14: Type of buying situation-- 1=new task, 2=modified rebuy & 3=straight rebuy
buy_situation <- factor(HATCOdata$X14, levels = c(1:3), 
                        labels = c("New Task", "Modified Rebuy", "Straight Rebuy"))
buy_situation

# X10: Satisfaction level-how satisfied the purchaser is with past purchases from HATCO
Satisfaction_level <- HATCOdata$X10

# Box Plot
library(ggplot2)
ggplot(HATCOdata, aes(x = buy_situation, y = Satisfaction_level)) +
  theme_bw() +
  geom_boxplot() +
  labs(y = "Satisfaction Level",
       x = "Type of buying situation",
       title = "Buying situation wise Satisfaction Level")

# Shapiro-Wilk normality test
'-----------------------------'
shapiro.test(Satisfaction_level)

# Test the homogeneity of variances
'----------------------------------'
bartlett.test(Satisfaction_level, buy_situation)
library(car)
leveneTest(Satisfaction_level, buy_situation)
leveneTest(Satisfaction_level, buy_situation, center = mean)

# One Way ANOVA with equal variance
'----------------------------------'
'Method-1:'
buy_situation_Model <- aov(Satisfaction_level ~ buy_situation)
summary(buy_situation_Model)
plot(buy_situation_Model)

'Method-2:'
oneway.test(Satisfaction_level ~ buy_situation, data = HATCOdata, var.equal = TRUE)

# Post Hoc Tests - Tuckey & bonferroni Methods
pairwise.t.test(Satisfaction_level,buy_situation, p.adjust.method = "bonferroni")
TukeyHSD(buy_situation_Model)

# One Way ANOVA with unequal variance (Welch approx.)
oneway.test(Satisfaction_level ~ buy_situation, data = HATCOdata, var.equal = FALSE)

# Post Hoc Tests - Games howell test 
library(PMCMRplus)
gamesHowellTest(Satisfaction_level ~ buy_situation, data = HATCOdata)
pw <- gamesHowellTest(Satisfaction_level ~ buy_situation, data = HATCOdata)
summary(pw)
summaryGroup(pw)

#Effect size:
library(effectsize)
eta_squared(buy_situation_Model, partial = FALSE)
omega_squared(buy_situation_Model)
cohens_f(buy_situation_Model)

'One Way ANCOVA 
---------------'
'Is there a difference in the three groups of individuals practicing physical 
exercises at different levels  for reducing anxiety level  
respect to Satisfaction level when we control for Delivery speed?'

# Load and prepare the data
library(datarium)
data(anxiety)
ax <- as.data.frame(anxiety)
head(ax)
tail(ax)

Pre_test <- ax$t1
Post_test <- ax$t3
Train_group <-as.factor(ax$group)

anxiety1 <- data.frame(Train_group,Pre_test,Post_test)
anxiety1

# Box Plot
library(ggplot2)
ggplot(anxiety1, aes(x = Train_group, y = Post_test)) +
  theme_bw() +
  geom_boxplot() +
  labs(y = "Post-Test Anxiety level",
       x = "Groups",
       title = "Effect of Physical Exercises on Anxiety Level")

# Check Assumptions

# 1: Linearity between the covariate and the outcome variable
# Scatter Plot
library(ggpmisc)
ggplot(anxiety1, aes(x = Pre_test, y = Post_test, 
                      color=Train_group, shape=Train_group)) +
  theme_bw() +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, formula = y ~ x)+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +    
  geom_point() + 
  labs(y = "Post Training Program Anxiety Score",
       x = "Pre Training Program Anxiety Score",
       title = "Relationship Between Pre & Post Training Program Anxiety Score")

# 2: Homogeneity of regression slopes
library(rstatix)
anova_test(data=anxiety1, Post_test ~ Train_group*Pre_test)

# 3: The outcome variable should be approximately normally distributed
'Shapiro-Wilk test of normality on the model residuals'
# Fit the model, the covariate goes first
model <- lm(Post_test ~ Pre_test + Train_group, data = anxiety1)
shapiro_test(model$residuals)

# 4: Homogeneity of residuals variance for all groups
levene_test(model$residuals ~ Train_group, data = anxiety1)

# 5: No significant outliers in the groups
model$cooks.distance<-cooks.distance(model)
round(model$cooks.distance, digits = 3)

# One Way ANCOVA
'---------------'
res.aov <- anova_test(Post_test ~ Pre_test + Train_group, data = anxiety1)
res.aov
get_anova_table(res.aov)

# Post-hoc test
library(emmeans)
pwc <- emmeans_test(Post_test ~ Train_group, covariate = Pre_test,
                                p.adjust.method = "bonferroni", data = anxiety1)
pwc





















