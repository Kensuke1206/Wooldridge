#open the dataset
library(wooldridge)
data("k401ksubs")
#caliculate the number of fsize==1
k401ksubs[k401ksubs$fsize==1, ]
sum(k401ksubs$fsize==1)
#model
F<-k401ksubs[k401ksubs$fsize==1, ]
Ch4 <- lm(nettfa~inc+age,data=F)
#report the result
library(stargazer)
stargazer(Ch4, type = "text", title = "Table 1: inc&age")

# Table 1: inc&age
# ===============================================
#                         Dependent variable:    
#                     ---------------------------
#                               nettfa           
# -----------------------------------------------
# inc                          0.799***          
#                               (0.060)          
                                               
# age                          0.843***          
#                               (0.092)          
                                               
# Constant                    -43.040***         
#                               (4.080)          
                                               
# -----------------------------------------------
# Observations                   2,017           
# R2                             0.119           
# Adjusted R2                    0.118           
# Residual Std. Error     44.683 (df = 2014)     
# F Statistic          136.465*** (df = 2; 2014) 
# ===============================================
# Note:               *p<0.1; **p<0.05; ***p<0.01

#model
SL<-lm(nettfa~inc, data=F)
#report the result
library(stargazer)
stargazer(SL, type = "text", title = "Table 2: inc SL")

# Table 2: inc SL
# ===============================================
#                         Dependent variable:    
#                     ---------------------------
#                               nettfa           
# -----------------------------------------------
# inc                          0.821***          
#                               (0.061)          
                                               
# Constant                    -10.571***         
#                               (2.061)          
                                               
# -----------------------------------------------
# Observations                   2,017           
# R2                             0.083           
# Adjusted R2                    0.082           
# Residual Std. Error     45.592 (df = 2015)     
# F Statistic          181.599*** (df = 1; 2015) 
# ===============================================
# Note:               *p<0.1; **p<0.05; ***p<0.01

#caliculate correlation between inc and age
cor(F$inc,F$age)
# [1] 0.03905864
#convert t statistics into p-value
pt(-1.71,2016,lower.tail=t)
# [1] 0.04370987