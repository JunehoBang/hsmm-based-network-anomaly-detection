mysmooth <-function (x_e, x_r, od_e, od_r, rd, pi.par, tpm.par, od.par_e, od.par_r, rd.par, M = NA) 
{
    #x: observation
    #od: observation probability distribution (ex. "normal")                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
    #rd: runlength distribution (ex. "log")
    #pi.par: initial hidden state probability
    #tpm.par: hidden state transition probability
    #rd.par: runlength distribution parameters

    inputData_e <- as.vector(x_e) # get values as an vector
    inputData_r <- as.vector(x_r) # get values as an vector
    ObLength <- myget_tau(inputData_e) # length of observations
    J <- length(pi.par)	      # the number of states
    Para <- list()            # Input parameters for FB algorithm
    Para$pi <- pi.par         # Initial state probability dist.
    Para$tpm <- tpm.par       # Transition probability dist.
    Para$rd <- rd.par         # runlength probability dist.
    Para$od_e <- od.par_e         # observation probability dist.
    Para$od_r <- od.par_r         # observation probability dist.
    
    if (is.na(M)) {           # M: Maximum runlength
        if (rd == "nonp") {
            M <- as.integer(dim(rd.par$np)[1])
        }
        else {
            M <- as.integer(min(ObLength, 1000))
        }
    }

    run_d <- myget_d(rd, J,M, param = Para$rd)
    RunLengthProb=matrix(run_d,J,byrow=TRUE)
    ob_p <- myget_pdf(inputData_e=inputData_e, inputData_r=inputData_r, 
                      od_e=od_e, od_r=od_r, J=J, 
		      param_e = Para$od_e, param_r = Para$od_r)  
    ObProb=matrix(ob_p,J,byrow=TRUE) #This is real observation probability matrix
    Alpha_F <-array(0,c(ObLength,J,M)) #J: Number of states
    Beta_B <-array(0,c(ObLength,J,M))  #M: Maximum runlength

    for (j in 1:J)
    {
	Normfactor=sum(RunLengthProb[j,])
	RunLengthProb[j,] = RunLengthProb[j,]/Normfactor

    }
    
    #forward and backward variable initialization

    for (d in 1:M){
        Alpha_F[1,,d] = Para$pi *ObProb[,1]*RunLengthProb[,d]
    }

    
    
    #forward variable calculation
    for (ti in 2:ObLength){
       for (m in 1:J) {
            for (d in M:1) {
		if(d==M){
		   firstTerm=0 
		}
		else{
		   firstTerm= Alpha_F[ti-1,m,d+1]*ObProb[m,d+1]
		}
		
		#for (n in 1:J){
		#    if(n!=m){
		#           PrevClue = PrevClue + Alpha_F[ti-1,n,1] * Para$tpm[n,m]
		#       }
    		#}

		PrevClue = sum(Alpha_F[ti-1,,1]*Para$tpm[,m])-Alpha_F[ti-1,m,1] * Para$tpm[m,m]
    	secondTerm = PrevClue*ObProb[m,ti]*RunLengthProb[m,d]
    	Alpha_F[ti,m,d]=firstTerm+secondTerm
			}
        }
    }

    loglikelihood = log(sum(Alpha_F[ObLength,,]))
    out <-list(forward=Alpha_F, loglikelihood=loglikelihood)
    return(out)
}
