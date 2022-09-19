#open the dataset
library(wooldridge)
data("rdchem")
head(rdchem)
#calculate t-statistics
(-70-0)/37
# [1] -1.891892
#calculate p-value
pt(-1.89,29,lower.tail=F)
# [1] 0.03439544

#open the dataset
library(wooldridge)
data("wage2")
head(wage2)
#model
Ch6wage2 <- lm(log(wage)~educ+exper+educ*exper, data=wage2)
#report the result
library(stargazer)
stargazer(Ch6wage2, type = "text", title = "Table 1: log(wage)")

# Table 1: log(wage)
# ===============================================
#                         Dependent variable:    
#                     ---------------------------
#                              log(wage)         
# -----------------------------------------------
# educ                          0.044**          
#                               (0.017)          
                                               
# exper                         -0.021           
#                               (0.020)          
                                               
# educ:exper                    0.003**          
#                               (0.002)          
                                               
# Constant                     5.949***          
#                               (0.241)          
                                               
# -----------------------------------------------
# Observations                    935            
# R2                             0.135           
# Adjusted R2                    0.132           
# Residual Std. Error      0.392 (df = 931)      
# F Statistic           48.407*** (df = 3; 931)  
# ===============================================
# Note:               *p<0.1; **p<0.05; ***p<0.01

#calculate t-statistics
0.003/0.002
# [1] 2.533
#calculate p-value
pt(2.095, 931, lower.tail = F)
coef(summary(Ch6wage2))
 
#     Estimate  Std. Error   t value      Pr(>|t|)
# (Intercept)  5.949454710 0.240826445 24.704325 5.034158e-104
# educ         0.044049794 0.017391115  2.532891  1.147585e-02
# exper       -0.021495935 0.019978278 -1.075965  2.822215e-01
# educ:exper   0.003202974 0.001529164  2.094592  3.647711e-02

#model
exper10<-wage2$exper-10
ch6wage21<-lm(log(wage)~educ+exper+educ:exper10, data=wage2)library(stargazer)
#report the result
library(stargazer)
stargazer(Ch6wage21, type = "text", title = "Table 1: log(wage)")

# Table 1: log(wage)
# ===============================================
#                         Dependent variable:    
#                     ---------------------------
#                              log(wage)         
# -----------------------------------------------
# educ                         0.076***          
#                               (0.007)          
                                               
# exper                         -0.021           
#                               (0.020)          
                                               
# educ:exper10                  0.003**          
#                               (0.002)          
                                               
# Constant                     5.949***          
#                               (0.241)          
                                               
# -----------------------------------------------
# Observations                    935            
# R2                             0.135           
# Adjusted R2                    0.132           
# Residual Std. Error      0.392 (df = 931)      
# F Statistic           48.407*** (df = 3; 931)  
# ===============================================
# Note:               *p<0.1; **p<0.05; ***p<0.01

# t-statistics
# 10.85714285714
#calculate 95% confidence interval 
0.076+1.96*(0.007)
# [1] 0.08972
0.076-1.96*(0.007)
# [1] 0.06228
confint(ch6wage21, level=0.95) 

#                      2.5 %      97.5 %
# (Intercept)   5.4768291195 6.422080300
# educ          0.0630973587 0.089061718
# exper        -0.0607036116 0.017711742
# educ:exper10  0.0002019664 0.006203983