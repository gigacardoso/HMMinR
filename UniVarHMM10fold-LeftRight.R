library(doSNOW)

predict <- function(states, exam, iter, steps){
library(HMM)
print(paste(exam, "loading data"))
d <- read.csv(file=paste(c("C:\\Users\\Daniel\\Documents\\GitHub\\HMMinR\\data\\",exam,".csv"),collapse=""),head=TRUE,sep=",", stringsAsFactors=FALSE)
#d <- read.csv(file="C:\\Users\\Daniel\\Documents\\GitHub\\HMMinR\\data\\ALB.csv",head=TRUE,sep=",", stringsAsFactors=FALSE)

#exams <- c("CHE","T-CHO","TP","Type","Activity")

print(summary(d))
prob <- function (x) {x / sum (x)}  # Makes it a probability (it sums to 1)
# "P", "PP" and "PPP" sao fillers para que o num de simbolos seja multiplo do num de estados
vals <- getPossibleValues(exam)

vals <- c(vals, "$")

rows <- nrow(d)
fold <- floor(rows/10)

folds <- c(1,fold,fold*2,fold*3,fold*4,fold*5,fold*6,fold*7,fold*8,fold*9)
fileConn<-file(paste(c("C:\\hepat_data030704\\data\\predictionsHMM\\new\\",exam,"_Predictions.csv"),collapse=""))
#fileConn<-file("C:\\hepat_data030704\\data\\predictionsHMM\\ALB_Predictions.csv")
for(p in 1:9){
	print(p)
	if( p == 1){
	train <- d[folds[p+1]+1:nrow(d)-fold,]
	test <- d[folds[p]:folds[p+1],]
	print(paste("train ", folds[p+1]+1,"-" , nrow(d)))
	print(paste("test ", folds[p],"-" , folds[p+1]))
	}else{
		if(p == 9){
			train <- d[1:folds[p]-1,]
			test <- d[folds[p]:nrow(d),]
			print(paste("train ", 1,"-",folds[p]-1))
			print(paste("test ", folds[p],"-" , nrow(d)))
		}else {
			train1 <- d[1:folds[p]-1,]
			train2 <- d[folds[p+1]+1:nrow(d),]
			size <- nrow(d) - folds[p+1]
			test <- d[folds[p]:folds[p+1],]
			train <- rbind(train1, train2[1,])
			for(k in 2:size){
				train <- rbind(train, train2[k,])
			}
			print(paste("train ", 1,"-",folds[p]-1))
			print(paste("test ", folds[p],"-" , folds[p+1]))
			print(paste("train ", folds[p+1]+1,"-",nrow(d)))
		}
	}
	
#	hmm = initHMM(c("1","2","3","4"), vals,
#		transProbs=matrix(c(.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25),4),
#		emissionProbs=matrix(c(.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25,.25),4))	
	stat <- c("a","b")
	for(i in 1:(states-2)){
		stat <- c( stat , paste("s",i))
	}
	
	m <- matrix(0, states, states)
	for (i in 1:states){
		if (i < states){ 
			m[i,i+1] <- 1
		}else{
			m[i,1] <- 1
		}
	}

	print(paste(exam, "initialization"))
	hmm = initHMM(stat, vals, startProbs=(prob (runif (states))),
		transProbs=m,
		emissionProbs=apply (matrix (runif(states*length(vals)), states), 1, prob))	
	#print(hmm)
	#train hmm
	print(paste(exam, "Build training"))
	m = 1
	observations <- vector()
	for (i in 1:nrow(train)) {
		for (j in 2:ncol(train)) {
			observations[m] <- train[[i,j]]
			m = m + 1
		}
		observations[m] <- "$"
		m = m + 1
	}
	print(paste(exam, "BaumWelch", "iter ->", iter))
	vt = baumWelch(hmm, observations, maxIterations=iter, delta=1E-9, pseudoCount=0)
	#print(vt$hmm)
	#predict
	values <- getPossibleValues(exam)
	
	print(paste(exam, "Forward"))
	for (i in 1:nrow(test)) {
		m = 1
		observations <- vector()
		#get values of row
		for (j in seq(2, ncol(test)-1, by=1)) {
			observations[m] <- test[[i,j]]
			m = m + 1
		}
		#forward and save for every possible value
		
		probs <- vector()
		for(j in 1:length(values)){
			observations[m] <- values[j]
			observations[(m+1)] <- "$"
			f <- forward(vt$hmm, observations)
			#print(observations)
			#print(f)
			probs[j] <- f[1,ncol(f)]
			for(k in 2:states){
				if (f[k,ncol(f)] > probs[j]){
					probs[j] <- f[k,ncol(f)]
				}
			}
		}
		max <- (-2000000)
		for(j in 1:length(values)){
			if (probs[j] > max){
				index <- j
				max <- probs[j]
			}
		}
		#if( index != 2){
	#		print(observations)
	#		print(paste("chosen",vals[index]))
	#	}
		obs <- vector()
		obs[1] <- test[i,1]
		obs[2] <- values[index]
		#print(test[i,])
		#print(obs)
		if( p == 1 && i == 1){
			text <- paste(obs,collapse=",")
		}else{
			text <- c(text,paste(obs,collapse=","))
		}
	}
}
write(text,fileConn)
close(fileConn)
print(paste(exam, "<-----------------------------------   DONE"))
}

getPossibleValues <- function(exam){
	if(exam == "ALB"){
		vals <- c("H","N","L","VL")
	}		
	if(exam == "Type"){
	vals <- c("B","C")
	}
	if(exam == "CHE"){
	vals <- c("H","N","L","VL","VH")
	} 
	if(exam == "T-CHO") {
		vals <- c("H","N","L","VL","VH")
	}
	if(exam == "TP") {
		vals <- c("H","N","L","VL","VH")
	}
	if(exam == "T-BIL") {
		vals <- c("N","H","VH","UH")
	}
	if(exam == "Activity"){
		vals <- c("A1","A2","A3")
	}
	if(exam == "GPT"){
		vals <- c("N","H","VH","UH")
	}
	if(exam == "GOT"){
		vals <- c("N","H","VH","UH")
	}
	if(exam == "ZTT"){
		vals <- c("N","H","VH","UH")
	}
	if(exam == "TTT"){
		vals <- c("N","H","VH","UH")
	}
	if(exam == "D-BIL"){
		vals <- c("N","H","VH","UH")
	}
	if(exam == "I-BIL"){
		vals <- c("N","H","VH","UH")
	}
	print(vals)
	return(vals)
}

#
#				----------- RUN ------------------------
#


#"GPT","GOT","ZTT","TTT","D-BIL","I-BIL","ALB","T-CHO","T-BIL","TP","Type","CHE","Activity"
exams <- c("GPT","GOT","ZTT","TTT","D-BIL","I-BIL","ALB","T-CHO","T-BIL","TP","Type","CHE","Activity")

#cl <- makeCluster(3, type="SOCK")
#registerDoSNOW(cl)

#writeLines(c(""), "C:\\hepat_data030704\\data\\predictionsHMM\\__log.txt")

for(i in 1:length(exams) ) { # , .combine=rbind) %dopar% {
	#sink("C:\\hepat_data030704\\data\\predictionsHMM\\__log.txt", append=TRUE)
	print(exams)
	#predict(#states,exam, #iter, #steps)
	predict(3,exams[i], 5, 3)
	#sink()
}

#stopCluster(cl)
