(1)
#calculate t-statistics
(-2.19-0)/0.53
# [1] -4.132075
#calculate p-value
2*pt (-4.132075, 4131, lower.tail = F)
# [1] 3.666245e-05

(2)
#calculate t-statistic
(-45.09-0)/4.29
# [1] -10.51049
#calculate p-value
2*pt (-10.510494131, lower.tail = F)
# [1] 1.617985e-25

(3)
#calculate t-statistic
(-169.81-0)/12.81
# [1] -13.25605
#calculate p-value
2*pt (-13.25605, 4131, lower.tail = F)
# [1] 2.616011e-39

(1)
#open the dataset
library(wooldridge)
data("sleep75")
head(sleep75)

#separate the data
slp75_male<-subset(sleep75, sleep75$male=="1")
slp75_female<-subset(sleep75, sleep75$male=="0")
#model for male
ch7sleep75.1male <- lm(sleep~totwrk+educ+age+I(age^2)+yngkid, data=slp75_male)
#model for female
ch7sleep75.1female <- lm(sleep~totwrk+educ+age+I(age^2)+yngkid, data=slp75_female)
#report the result
library(stargazer)
stargazer(ch7sleep75.1male, type = "text", title = "Table 1: sleep75 for male")

# Table 1: sleep75 for male
# ===============================================
#                         Dependent variable:    
#                     ---------------------------
#                                sleep           
# -----------------------------------------------
# totwrk                       -0.182***         
#                               (0.024)          
                                               
# educ                         -13.052*          
#                               (7.414)          
                                               
# age                            7.157           
#                              (14.320)          
                                               
# I(age2)                       -0.045           
#                               (0.168)          
                                               
# yngkid                        60.380           
#                              (59.023)          
                                               
# Constant                   3,648.208***        
#                              (310.039)         
                                               
# -----------------------------------------------
# Observations                    400            
# R2                             0.156           
# Adjusted R2                    0.146           
# Residual Std. Error     402.290 (df = 394)     
# F Statistic           14.590*** (df = 5; 394)  
# ===============================================
# Note:               *p<0.1; **p<0.05; ***p<0.01

stargazer(ch7sleep75.1female, type = "text", title = "Table 2: sleep75 for female")

# Table 2: sleep75 for female
# ===============================================
#                         Dependent variable:    
#                     ---------------------------
#                                sleep           
# -----------------------------------------------
# totwrk                       -0.140***         
#                               (0.028)          
                                               
# educ                          -10.205          
#                               (9.589)          
                                               
# age                           -30.357          
#                              (18.531)          
                                               
# I(age2)                        0.368           
#                               (0.223)          
                                               
# yngkid                       -118.283          
#                              (93.188)          
                                               
# Constant                   4,238.729***        
#                              (384.892)         
                                               
# -----------------------------------------------
# Observations                    306            
# R2                             0.098           
# Adjusted R2                    0.083           
# Residual Std. Error     436.992 (df = 300)     
# F Statistic           6.495*** (df = 5; 300)   
# ===============================================
# Note:               *p<0.1; **p<0.05; ***p<0.01

(2)
#model
ch7sleep75.2 <- lm(sleep~educ+age+agesq+yngkid+totwrk+male+male:totwrk+male:educ+male:age+male:agesq+male:yngkid, data=sleep75)
#report the result
stargazer(ch7sleep75.2, type = "text", title = "Table 3: sleep75 (2)")

# Table 3: sleep75 (2)
# ===============================================
#                         Dependent variable:    
#                     ---------------------------
#                                sleep           
# -----------------------------------------------
# educ                          -10.205          
#                               (9.164)          
                                               
# age                          -30.357*          
#                              (17.710)          
                                               
# agesq                         0.368*           
#                               (0.213)          
                                               
# yngkid                       -118.283          
#                              (89.062)          
                                               
# totwrk                       -0.140***         
#                               (0.026)          
                                               
# male                         -590.521          
#                              (488.792)         
                                               
# totwrk:male                   -0.042           
#                               (0.037)          
                                               
# educ:male                     -2.847           
#                              (11.968)          
                                               
# age:male                      37.513           
#                              (23.123)          
                                               
# agesq:male                    -0.413           
#                               (0.276)          
                                               
# yngkid:male                  178.663*          
#                              (108.105)         
                                               
# Constant                   4,238.729***        
#                              (367.852)         
                                               
# -----------------------------------------------
# Observations                    706            
# R2                             0.131           
# Adjusted R2                    0.117           
# Residual Std. Error     417.645 (df = 694)     
# F Statistic           9.479*** (df = 11; 694)  
# ===============================================
# Note:               *p<0.1; **p<0.05; ***p<0.01

library(car)
linearHypothesis(ch7sleep75.2, c("male=0", "totwrk:male=0", "educ:male=0", "age:male=0", "agesq:male=0", "yngkid:male=0"))

# Linear hypothesis test

# Hypothesis:
# male = 0
# totwrk:male = 0
# educ:male = 0
# age:male = 0
# agesq:male = 0
# yngkid:male = 0

# Model 1: restricted model
# Model 2: sleep ~ educ + age + agesq + yngkid + totwrk + male + male:totwrk + 
#     male:educ + male:age + male:agesq + male:yngkid

#   Res.Df       RSS Df Sum of Sq      F  Pr(>F)  
# 1    700 123267451                              
# 2    694 121052555  6   2214896 2.1164 0.04949 *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(3)
# Linear hypothesis test

# Hypothesis:
# totwrk:male = 0
# educ:male = 0
# age:male = 0
# agesq:male = 0
# yngkid:male = 0

# Model 1: restricted model
# Model 2: sleep ~ educ + age + agesq + yngkid + totwrk + male + male:totwrk + 
#     male:educ + male:age + male:agesq + male:yngkid

#   Res.Df       RSS Df Sum of Sq      F Pr(>F)
# 1    699 122147777                           
# 2    694 121052555  5   1095222 1.2558 0.2814

(4)
#model
ch7sleep75.4 <- lm(sleep~totwrk+educ+age+agesq+yngkid+male, data=sleep75)

#report the result
# Table 4: sleep75 (4)
# ===============================================
#                         Dependent variable:    
#                     ---------------------------
#                                sleep           
# -----------------------------------------------
# totwrk                       -0.163***         
#                               (0.018)          
                                               
# educ                         -11.713**         
#                               (5.872)          
                                               
# age                           -8.697           
#                              (11.329)          
                                               
# agesq                          0.128           
#                               (0.135)          
                                               
# yngkid                        -0.023           
#                              (50.276)          
                                               
# male                         87.755**          
#                              (34.668)          
                                               
# Constant                   3,840.852***        
#                              (239.414)         
                                               
# -----------------------------------------------
# Observations                    706            
# R2                             0.123           
# Adjusted R2                    0.115           
# Residual Std. Error     418.027 (df = 699)     
# F Statistic           16.302*** (df = 6; 699)  
# ===============================================
# Note:               *p<0.1; **p<0.05; ***p<0.01

library(qpcR)
RSS(ch7sleep75.1male)
# [1] 63763979
RSS(ch7sleep75.1female)
# [1] 57288576