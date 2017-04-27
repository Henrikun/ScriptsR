######################################################################
##
##  building a regression function without intercept.
##              Gujarati, Porter (2011)
##
#######################################################################
#
##    The function is Yi = beta2_estimated * Xi + ui_estimated
##                  Yi = b2Xi + ui        
##_____________________________________________________________________
##  Curva de Phillips Aumentada de Friedman (1968) e Phelps (1968).
##   (Pi_t - Pi_ex) = - beta (Un_t - Un_ex)
##    Inflation = - beta ( Unemployment Rate )


  Pi_t  <- c(101:110)
    Pi_ex  <- c(121:130)
      Un_t  <-  c(1:10)
        Un_ex <- c(11:20)

  Inflation <-  Pi_t-Pi_ex
    Unemployment <- Un_t-Un_ex
      
####    How to Calculate the Beta Estimated 
#      Xi <- Unemployment
#      Yi <- Inflation
#      Sum_XiYi <- sum(Unemployment*Inflation)
#      Sum_Xi2 <- sum(Unemployment)^2 
#      beta_estimated <- Sum_XiYi/Sum_Xi2
##    Or just use only command line:
    
  beta_estimated <- sum(Unemployment*Inflation)/sum(Unemployment)^2
    beta_estimated
    
############################################
##  Or Using one regression function without intecept.
##  lm(formula = Yi ~ 0 + Xi)
Inflation <- log(Inflation)
    lm(formula = Inflation ~ 0 + Unemployment )
        summary(lm(formula = Inflation ~ 0 + Unemployment ))
        
        
        
    
        
      