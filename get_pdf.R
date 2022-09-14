myget_pdf <- function(inputData_e, inputData_r, od_e, od_r, J, param_e, param_r){
	library("VGAM")
	tau <- myget_tau(inputData_r)
	pdf <- rep(0, times = J * tau)
	pdf_e <- rep(0, times = J * tau)
	pdf_r <- rep(0, times = J * tau)

	for (j in 1:J){

		if(od_e=="betabinom"){
			pdf_e[(1 + (j - 1) * tau):(tau + (j - 1) * tau)] <- dbetabinom.ab(inputData_e[1:tau],size=param_e$max_value[j], shape1=param_e$shape1[j],shape2=param_e$shape2[j])
		}
       
		if(od_r=="pois"){
			pdf_r[(1 + (j - 1) * tau):(tau + (j - 1) * tau)] <-dtpois(x=inputData_r[1:tau],max_value=param_r$max_value[j],lambda=param_r$lambda[j])
		}
       
		if(od_r=="binom"){
			pdf_r[(1 + (j - 1) * tau):(tau + (j - 1) * tau)] <-dbinom(inputData_r[1:tau],param_r$max_value[j],param_r$prob[j])
		}

		if(od_r == "truncnorm") {
			normfactor<-sum(dnorm(c(0:param_r$max_value[j]), mean=param_r$mean[j], sd=sqrt(param_r$var[j])))
			for (k in 1:tau){
				pdf_r[k + (j - 1) * tau] <- dnorm(inputData_r[k], mean=param_r$mean[j] , sd=sqrt(param_r$var[j]))/normfactor
			}
		}
		if(od_r== "hypergeom"){
			pdf_r[(1 + (j - 1) * tau):(tau + (j - 1) * tau)] <-dhyper(inputData_r[1:tau],param_r$m[j],param_r$n[j],param_r$k[j])
		}
	}
	pdf<-pdf_e * pdf_r
	#lower_bound <- 1e-300
	#pdf[pdf < lower_bound] <- lower_bound
	return(pdf)
}