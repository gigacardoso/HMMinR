local({pkg <- select.list(sort(.packages(all.available = TRUE)),graphics=TRUE) if(nchar(pkg)) library(pkg, character.only=TRUE)})

hmm = initHMM(c("A","B"),c("L","R"))
print(hmm)
# Sequence of observation
a = sample(c(rep("L",100),rep("R",300)))
b = sample(c(rep("L",300),rep("R",100)))
observation = c(a,b)
# Viterbi-training
vt = viterbiTraining(hmm,observation,10)
print(vt$hmm)