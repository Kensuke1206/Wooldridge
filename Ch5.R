#open the dataset
library(wooldridge)
data("htv")
head(htv)
#open the library, dplyr
library(dplyr)
count(htv, educ)

#    educ   n
# 1     6   2
# 2     7   3
# 3     8  29
# 4     9  41
# 5    10  47
# 6    11  64
# 7    12 512
# 8    13  93
# 9    14  67
# 10   15 206
# 11   16  70
# 12   17  47
# 13   18  19
# 14   19  14
# 15   20  16

#draw the histogram of educ
hist(htv$educ, prob=TRUE)
#overlay the normal distribution on the histogram
curve(dnorm(x,mean(htv$educ),sd(htv$educ)),add=TRUE,col="red")
#calculate skewness
skewness(htv$educ)
# [1] 0.427351
#calculate kurtosis
kurtosis(htv$educ)
# [1] 0.4683207

#model
Ch5educ <- lm(educ~motheduc+fatheduc+abil+abil^2, data=htv)
#report the result
library(stargazer)
stargazer(Ch5educ, type = "text", title = "Table 1: educ")

# Table 1: educ
# ===============================================
#                         Dependent variable:    
#                     ---------------------------
#                                educ            
# -----------------------------------------------
# motheduc                     0.189***          
#                               (0.029)          
                                               
# fatheduc                     0.111***          
#                               (0.020)          
                                               
# abil                         0.502***          
#                               (0.026)          
                                               
# Constant                     8.449***          
#                               (0.290)          
                                               
# -----------------------------------------------
# Observations                   1,230           
# R2                             0.428           
# Adjusted R2                    0.426           
# Residual Std. Error      1.784 (df = 1226)     
# F Statistic          305.172*** (df = 3; 1226) 
# ===============================================
# Note:               *p<0.1; **p<0.05; ***p<0.01