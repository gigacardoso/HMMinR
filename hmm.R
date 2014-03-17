 local({pkg <- select.list(sort(.packages(all.available = TRUE)),graphics=TRUE)
 if(nchar(pkg)) library(pkg, character.only=TRUE)})

#load data
 d <- read.csv(file="C:\\Users\\Daniel\\Documents\\GitHub\\HMMinR\\ALB.csv",head=TRUE,sep=",")

# Initialise HMM
hmm = initHMM(c("A","B"), 
c("H","N","L","VL"),
transProbs=matrix(c(.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25),4),
emissionProbs=matrix(c(.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25),4))

print(hmm)
print("yay")
#train HMM
 for (i in 1:nrow(d)) {
	#print(d[i,2:ncol(d)])
	obs = as.vector(d[1,2:ncol(d)])
	obs <- vector()
	m = 1
	for (j in 2:ncol(d)) {
		obs[[m]] <- d[i,j]
		m = m + 1
	}
	baumWelch(hmm, obs, maxIterations=100, delta=1E-9, pseudoCount=0)
}
print(hmm)

# Sequence of observations
observations = c("L","L","L","R")
# Calculate forward probablities
logForwardProbabilities = forward(hmm,observations)

print(exp(logForwardProbabilities))