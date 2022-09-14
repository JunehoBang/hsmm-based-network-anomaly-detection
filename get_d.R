myget_d <- function(rd, J, M, param){   #J: the number of states
   d <- c()                          #M: The number of maximum length
   for (j in 1:J){ 
     # rd = "non.parametric" 
     if (rd == "nonp") 
       d <- c(d, param$np[,j]) 
     # rd = "geometric" 
     if (rd == "geom") 
       d <- c(d, dgeom(c(1:M), param$p[j])) 
     # rd = "negative.binomial" 
     if (rd == "nbinom") 
       d <- c(d, dnbinom(c(1:M), size = param$r[j], prob = param$pi[j])) 
     # rd = "logarithmic"                                                                                                                      
     if (rd == "log") 
       d <- c(d, dlog(c(1:M), param$p[j])) 
     # rd = "logarithmic.geometric" 
 #    if (rd == "logarithmic.geometric") 
 #      d <- c(d, dloggeom(c(0:(M - 1)), param$p.loggeom[j], param$theta[j])) 
     # rd = "Poisson" 
     if (rd == "pois") 
       d <- c(d, dpois(c(1:M), param$lambda[j])) 
     } 
   return(d) 
} 
