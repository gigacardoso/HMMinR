 local({pkg <- select.list(sort(.packages(all.available = TRUE)),graphics=TRUE)
 if(nchar(pkg)) library(pkg, character.only=TRUE)})

 d <- read.csv(file="C:\\Users\\Daniel\\Documents\\GitHub\\HMMinR\\ALB.csv",head=TRUE,sep=",")

 for (i in 1:nrow(d)) {
	print(d[i,])
	obs = d[i,]
}
# Initialise HMM
hmm = initHMM(c("A","B"), 
c("L","R"),
transProbs=matrix(c(.8,.2,.2,.8),2),
emissionProbs=matrix(c(.6,.4,.4,.6),2))

#train HMM
for each 
baumWelch(hmm, observation, maxIterations=100, delta=1E-9, pseudoCount=0)


emissionProbs=matrix(c(.6,.4,.4,.6),2))
print(hmm)
# Sequence of observations
observations = c("L","L","L","R")
# Calculate forward probablities
logForwardProbabilities = forward(hmm,observations)
print(exp(logForwardProbabilities))