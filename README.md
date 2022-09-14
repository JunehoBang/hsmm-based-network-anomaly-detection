# hsmm-based-network-anomaly-detection

The repository contains the source code for validating the network anomaly detection algorithm using hsmm based network traffic model.
The details of the algorithm is specified in the ref. [^1]

Please  note that the source codes were written in reference to the hsmm package in R
Considering the R package's limitations in testing my algorithm, I re-developed the code referring to the package codes

The main code is contained in `hsmmdetector.r` which evaluates the normality measure of the observation 
by calling the code in `mysmooth.r` which implements the hsmm's forward-backward algorithm. 

In case the normality value is lower than the predefined value, it is judged as abnormal

The `mysmooth.r` incorportates some of the typical probability distributions as well as the uncommon ones. 
These probability distributions were taken from `get_d.r` and `get_pdf.r`. respectively.

[^1]: J. Bang, "Anomaly detection of network-initiated LTE signaling traffic in wireless sensor and actuator networks based on a Hidden semi-Markov Model," Computers & Security, vol. 65, pp. 108-120, Mar. 2017, URL: https://www.sciencedirect.com/science/article/pii/S0167404816301614
