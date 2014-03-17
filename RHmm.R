# Fit a 3 states 1D-gaussian model
data(n1d_3s)
HMMFit(obs_n1d_3s, nStates=3)
# Fit a 3 states gaussian HMM for obs_n1d_3s
# with iterations printing and kmeans initialization
Res_n1d_3s <- HMMFit(obs=obs_n1d_3s, nStates=3,
control=list(verbose=1, init="KMEANS"),
asymptCov=TRUE)
summary(Res_n1d_3s)
# Fit a 2 states 3D-gaussian model
data(n3d_2s)
summary(HMMFit(obs_n3d_2s, asymptCov=TRUE))
# Fit a 2 states mixture of 3 normal distributions HMM
# for data_mixture
data(data_mixture)
ResMixture <- HMMFit(data_mixture, nStates=2, nMixt=3,
dis="MIXTURE")
# Fit a 3 states discrete HMM for weather data
data(weather)
ResWeather <- HMMFit(weather, dis=’DISCRETE’, nStates=3)