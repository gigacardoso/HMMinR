library(HMM)

d <- read.csv(file="C:\\Users\\Daniel\\Documents\\GitHub\\HMMinR\\data\\ALB.csv",head=TRUE,sep=",", stringsAsFactors=FALSE)
prob <- function (x) {x / sum (x)}  # Makes it a probability (it sums to 1)
vals <- c("H","N","L","VL")
rows <- nrow(d)
fold <- floor(rows/10)

folds <- c(1,fold,fold*2,fold*3,fold*4,fold*5,fold*6,fold*7,fold*8,fold*9)
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
	hmm = initHMM(c("1","2","3","4"), c("H","N","L","VL"), startProbs=(prob (runif (4))),
		transProbs=apply (matrix (runif(16), 4), 1, prob),
		emissionProbs=apply (matrix (runif(16), 4), 1, prob))	
	
	#train hmm
	m = 1
	observations <- vector()
	for (i in 1:nrow(train)) {
		for (j in 2:ncol(train)) {
			observations[m] <- train[[i,j]]
			m = m + 1
		}
	}
	vt = baumWelch(hmm, observations, maxIterations=10, delta=1E-9, pseudoCount=0)

	#predict
	fileConn<-file(paste("C:\\Users\\Daniel\\Documents\\GitHub\\HMMinR\\ALB_Predictions.csv"))#,i,".csv"))
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
		for(j in 1:length(vals)){
			observations[m] <- vals[j]
			f <- forward(vt$hmm, observations)
			#print(observations)
			#print(f)
			probs[j] <- f[1,7]
			for(k in 2:4){
				if (f[k,7] > probs[j]){
					probs[j] <- f[k,7]
				}
			}
		}
		max <- (-200)
		for(j in 1:length(vals)){
			if (probs[j] > max){
				index <- j
				max <- probs[j]
			}
		}
		if( index != 2){
			print(observations)
			print(paste("chosen",vals[index]))
		}
		obs <- vector()
		obs <- test[i,]
		obs[8] <- vals[index]
		print(test[i,])
		print(obs)
		write(paste(obs,collapse=","),fileConn,append=TRUE)
	}
	close(fileConn)
}

