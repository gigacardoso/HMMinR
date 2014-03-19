library(HMM)
#load data
d <- read.csv(file="C:\\Users\\Daniel\\Documents\\GitHub\\HMMinR\\data\\ALB.csv",head=TRUE,sep=",", stringsAsFactors=FALSE)

# Initialise HMM
hmm = initHMM(c("1","2","3","4"), c("H","N","L","VL"),
transProbs=matrix(c(
.5,.2,.1,.2,
.2,.5,.2,.1,
.1,.15,.55,.2,
.2,.15,.15,.5),4),
emissionProbs=matrix(c(.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25),4))

#print(hmm)
#train HMM

for (i in 1:nrow(d)) {
	print(i)
	observations <- vector()
	m = 1
	for (j in 2:ncol(d)) {
		observations[m] <- d[[i,j]]
		m = m + 1
	}
	print(observation)
	print("")
	if(i == 1){
		vt = baumWelch(hmm, observations, maxIterations=100, delta=1E-9, pseudoCount=0)
	}else{
		vt = baumWelch(vt$hmm, observations, maxIterations=100, delta=1E-9, pseudoCount=0)
	}
}
#print(observations)
print(vt$hmm)

# Sequence of observations
observations = c("L","L","L","R")
# Calculate forward probablities
logForwardProbabilities = forward(hmm,observations)

print(exp(logForwardProbabilities))