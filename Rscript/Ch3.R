#open the dataset
library(wooldridge)
data("ceosal2")
#model
C3 <- lm(log(salary)~log(sales)+log(mktval),data)
#report the result
library(stargazer)
stargazer(C3, type = "text", title = "Table 1: sales&mktval")

# Table 1: sales&mktval
# ===============================================
#                         Dependent variable:    
#                     ---------------------------
#                             log(salary)        
# -----------------------------------------------
# log(sales)                   0.162***          
#                               (0.040)          
                                               
# log(mktval)                   0.107**          
#                               (0.050)          
                                               
# Constant                     4.621***          
#                               (0.254)          
                                               
# -----------------------------------------------
# Observations                    177            
# R2                             0.299           
# Adjusted R2                    0.291           
# Residual Std. Error      0.510 (df = 174)      
# F Statistic           37.129*** (df = 2; 174)  
# ===============================================
# Note:               *p<0.1; **p<0.05; ***p<0.01

#model
C3.1 <- lm(log(salary)~log(sales)+log(mktval)+profits,data)
#report the result
stargazer(C3.1, type = "text", title = "Table 2: sales&mktval&profits")

# Table 2: sales&mktval&profits
# ===============================================
#                         Dependent variable:    
#                     ---------------------------
#                             log(salary)        
# -----------------------------------------------
# log(sales)                   0.161***          
#                               (0.040)          
                                               
# log(mktval)                    0.098           
#                               (0.064)          
                                               
# profits                       0.00004          
#                              (0.0002)          
                                               
# Constant                     4.687***          
#                               (0.380)          
                                               
# -----------------------------------------------
# Observations                    177            
# R2                             0.299           
# Adjusted R2                    0.287           
# Residual Std. Error      0.512 (df = 173)      
# F Statistic           24.636*** (df = 3; 173)  
# ===============================================
# Note:               *p<0.1; **p<0.05; ***p<0.01

#model: add ceoten to C3.1
C3.2 <- lm(log(salary)~log(sales)+log(mktval)+profits+ceoten,data)
#report the result
stargazer(C3.2, type = "text", title = "Table 3: sales&mktval&profits&ceoten")

# Table 3: sales&mktval&profits&ceoten
# ===============================================
#                         Dependent variable:    
#                     ---------------------------
#                             log(salary)        
# -----------------------------------------------
# log(sales)                   0.162***          
#                               (0.039)          
                                               
# log(mktval)                    0.102           
#                               (0.063)          
                                               
# profits                       0.00003          
#                              (0.0002)          
                                               
# ceoten                        0.012**          
#                               (0.005)          
                                               
# Constant                     4.558***          
#                               (0.380)          
                                               
# -----------------------------------------------
# Observations                    177            
# R2                             0.318           
# Adjusted R2                    0.302           
# Residual Std. Error      0.506 (df = 172)      
# F Statistic           20.077*** (df = 4; 172)  
# ===============================================
# Note:               *p<0.1; **p<0.05; ***p<0.01

#View the scattergram
plot(data$sales,data$profits, main="Correlation between sales and profits", xlab="sales", ylab="profits")
#calculate R
cor(data$sales,data$profits)
