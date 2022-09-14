hsmmdetector <- function(obs_e, obs_r,  od_e, od_r, rd, pi.par, tpm.par, od.par_e, 
		od.par_r, rd.par, M, threshold)
{
	smoothout=mysmooth(x_e=obs_e, x_r=obs_r, od_e=od_e,od_r=od_r, rd=rd, 
				pi.par=pi.par, tpm.par=tpm.par, od.par_e=od.par_e, 
				od.par_r, rd.par=rd.par, M=M)
	if (smoothout$loglikelihood < threshold)
		return("positive")
	else
		return("negative")
}
