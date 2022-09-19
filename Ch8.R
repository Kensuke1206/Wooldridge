#calculate t-statistics
0.900/0.166
# [1] 5.421687
#calculate p-value
2*pt (5.421687, 261, lower.tail = F)
# [1] 1.3444e-07
#calculate t-statistics
0.193/0.074
# [1] 2.608108
#calculate p-value
2*pt (2.608108, 261, lower.tail = F)
# [1] 0.009629007
#calculate t-statistics
0.0014/0.0012
# [1] 1.166667
#calculate p-value
2*pt (1.166667, 261, lower.tail = F)
# [1] 0.2444097

#calculate t-statistics
0.900/0.175
# [1] 5.142857
#calculate p-value
2*pt (5.142857, 261, lower.tail = F)
# [1] 5.317214e-07
#calculate t-statistics
0.193/0.064
# [1] 3.015625
#calculate p-value
2*pt (3.015625, 261, lower.tail = F)
# [1] 0.002817418
#calculate t-statistics
0.0014/0.0012
# [1] 1.166667
#calculate p-value
2*pt (1.166667, 269, lower.tail = F)
# [1] 0.2444097





#calculate t-statistics
(0.900-1)/0.166
# [1] -0.6024096
#calculate p-value
2*pt (-0.6024096, 261, lower.tail = T)
# [1] 0.5474245
#calculate t-statistics
(0.900-1)/0.175
# [1] -0.5714286
#calculate p-value
2*pt (-0.5714286, 261, lower.tail = T)
# [1] 0.5682009

#calculate t-statistics
-0.157/0.098
# [1] -1.602041
2*pt (-1.602041, 261, lower.tail = T)
# [1] 0.1103561

#calculate t-statistics
-0.157/0.080
# [1] -1.9625
2*pt (-1.9625, 261, lower.tail = T)
# [1] 0.05076656



#open the dataset
library(wooldridge)
data("hprice1")
head(hprice1)
ch8.1<-lm(price~lotsize+sqrft+bdrms, data=hprice1)
ch8.1<-lm(price~lotsize+sqrft+bdrms, data=hprice1)
stargazer(ch8.1, type = "text", title = "Table")

# Table
# ===============================================
#                         Dependent variable:    
#                     ---------------------------
#                                price           
# -----------------------------------------------
# lotsize                      0.002***          
#                               (0.001)          
                                               
# sqrft                        0.123***          
#                               (0.013)          
                                               
# bdrms                         13.853           
#                               (9.010)          
                                               
# Constant                      -21.770          
#                              (29.475)          
                                               
# -----------------------------------------------
# Observations                    88             
# R2                             0.672           
# Adjusted R2                    0.661           
# Residual Std. Error      59.833 (df = 84)      
# F Statistic           57.460*** (df = 3; 84)   
# ===============================================
# Note:               *p<0.1; **p<0.05; ***p<0.01

library(sandwich)
library(lmtest)
coeftest(ch8.1, vcov=vcovHC(ch8.1, type='HC1'))

# t test of coefficients:

#                Estimate  Std. Error t value  Pr(>|t|)    
# (Intercept) -21.7703081  37.1382106 -0.5862    0.5593    
# lotsize       0.0020677   0.0012514  1.6523    0.1022    
# sqrft         0.1227782   0.0177253  6.9267 8.096e-10 ***
# bdrms        13.8525217   8.4786250  1.6338    0.1060    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

coeftest(ch8.1)

# t test of coefficients:

#                Estimate  Std. Error t value  Pr(>|t|)    
# (Intercept) -2.1770e+01  2.9475e+01 -0.7386  0.462208    
# lotsize      2.0677e-03  6.4213e-04  3.2201  0.001823 ** 
# sqrft        1.2278e-01  1.3237e-02  9.2751 1.658e-14 ***
# bdrms        1.3853e+01  9.0101e+00  1.5374  0.127945    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

ch8.2<-lm(log(price)~log(lotsize)+log(sqrft)+bdrms, data=hprice1)
coeftest(ch8.2, vcov=vcovHC(ch8.2, type='HC1'))

stargazer(ch8.2, type = "text", title = "Table2")

# Table2
# ===============================================
#                         Dependent variable:    
#                     ---------------------------
#                             log(price)         
# -----------------------------------------------
# log(lotsize)                 0.168***          
#                               (0.038)          
                                               
# log(sqrft)                   0.700***          
#                               (0.093)          
                                               
# bdrms                          0.037           
#                               (0.028)          
                                               
# Constant                     -1.297**          
#                               (0.651)          
                                               
# -----------------------------------------------
# Observations                    88             
# R2                             0.643           
# Adjusted R2                    0.630           
# Residual Std. Error       0.185 (df = 84)      
# F Statistic           50.424*** (df = 3; 84)   
# ===============================================
# Note:               *p<0.1; **p<0.05; ***p<0.01
#               Estimate Std. Error t value  Pr(>|t|)    
# (Intercept)  -1.297042   0.781315 -1.6601 0.1006278    
# log(lotsize)  0.167967   0.041473  4.0500 0.0001136 ***
# log(sqrft)    0.700232   0.103829  6.7441 1.835e-09 ***
# bdrms         0.036958   0.030601  1.2077 0.2305340    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

coeftest(ch8.2)
# t test of coefficients:

#               Estimate Std. Error t value  Pr(>|t|)    
# (Intercept)  -1.297042   0.651284 -1.9915   0.04967 *  
# log(lotsize)  0.167967   0.038281  4.3877 3.307e-05 ***
# log(sqrft)    0.700232   0.092865  7.5403 5.006e-11 ***
# bdrms         0.036958   0.027531  1.3424   0.18308    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
