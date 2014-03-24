library(HMM)
#load data
d <- read.csv(file="C:\\Users\\Daniel\\Documents\\GitHub\\HMMinR\\data\\ALB.csv",head=TRUE,sep=",", stringsAsFactors=FALSE)
prob <- function (x) {x / sum (x)}  # Makes it a probability (it sums to 1)

# Initialise HMM
hmm = initHMM(c("1","2","3","4"), c("H","N","L","VL"), startProbs=(prob (runif (4))),
transProbs=apply (matrix (runif(16), 4), 1, prob),
emissionProbs=apply (matrix (runif(16), 4), 1, prob))
#matrix(c(.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25),4),
#matrix(c(
#.5,.2,.1,.2,
#.2,.5,.2,.1,
#.1,.15,.55,.2,
#.2,.15,.15,.5),4)

print(hmm)
print("yay")
#train HMM
#nrow(d)
m = 1
observations <- vector()
 for (i in 1:nrow(d)) {
	#print(i)
	for (j in 2:ncol(d)) {
		observations[m] <- d[[i,j]]
		m = m + 1
	}
}
#print(observations)
vt = baumWelch(hmm, observations, maxIterations=100, delta=1E-9, pseudoCount=0)
#vt = viterbiTraining(hmm, observations,10)
print(vt$hmm)

# Sequence of observations
observations = c("L","L","L","R")
# Calculate forward probablities
logForwardProbabilities = forward(hmm,observations)

print(exp(logForwardProbabilities))