i <- nrow(d)
fold <- floor(i/10)

folds <- c(1,fold,fold*2,fold*3,fold*4,fold*5,fold*6,fold*7,fold*8,fold*9)
for(j in 1:9){
	print(j)
	if( j == 1){
	train <- d[folds[j+1]+1:nrow(d),]
	test <- d[folds[j]:folds[j+1],]
	print(paste("train ", folds[j+1]+1,"-" , nrow(d)))
	print(paste("test ", folds[j],"-" , folds[j+1]))
	}else{
		if(j == 9){
			train <- d[1:folds[j]-1,]
			test <- d[folds[j]:nrow(d),]
			print(paste("train ", 1,"-",folds[j]-1))
			print(paste("test ", folds[j],"-" , nrow(d)))
		}else {
			train1 <- d[1:folds[j]-1,]
			train2 <- d[folds[j+1]+1:nrow(d),]
			size <- nrow(d) - folds[j+1]
			test <- d[folds[j]:folds[j+1],]
			train <- rbind(train1, train2[1,])
			for(k in 2:size){
				train <- rbind(train, train2[k,])
			}
			print(paste("train ", 1,"-",folds[j]-1))
			print(paste("test ", folds[j],"-" , folds[j+1]))
			print(paste("train ", folds[j+1]+1,"-",nrow(d)))
		}
	}
}
