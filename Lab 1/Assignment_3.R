mylin = function(X, Y, Xpred) {
	Xpred1 = cbind(1, Xpred)
	X1 = cbind(1, X)
	beta = solve(t(X1) %*% X1) %*% t(X1) %*% Y # obtained using the "training" data matrix
	Res = Xpred1 %*% beta # y_hat for the "test" data
	return(Res)
}
myCV = function(X, Y, Nfolds) {
	n = length(Y) # number of observations (rows)
	p = ncol(X) # number of covariates (variables or columns)
	set.seed(12345)
	ind = sample(n, n) # indexes are randomized
	X1 = X[ind, ] # randomize the order of the observations
	Y1 = Y[ind]
	sF = floor(n / Nfolds) # number of observations inside each fold
	MSE = numeric(2 ^ p - 1) # vector of the length of 2^p-1 combinations
	Nfeat = numeric(2 ^ p - 1)
	Features = list() # features that will be selected
	curr = 0 # current
	folds_obs <- cut(1:n, breaks = Nfolds, labels = FALSE)
	#we assume 5 features.
	for (f1 in 0:1)
		for (f2 in 0:1)
			for (f3 in 0:1)
				for (f4 in 0:1)
					for (f5 in 0:1) {
						model = c(f1, f2, f3, f4, f5)
						if (sum(model) == 0)
							next()
						SSE = 0
						for (k in 1:Nfolds) {
							#MISSING:compute which indices should belong to current fold
							indices <-
								ind[which(folds_obs == k)] #indeces of the observations in fold k
							#MISSING:implement cross-validation for model with features in "model" and iteration i.
							X_mylin <-
								X[-indices, which(model == 1)]
							XPred_mylin <-
								X[indices, which(model == 1)]
							Y_mylin <- Y[-indices]
							#MISSING:Get the predicted values for fold k, Ypred, and the original values for fold 'k',
							Ypred <-
								mylin(X_mylin, Y_mylin, XPred_mylin) 
							Yp <- Y[indices]
							SSE = SSE + sum((Ypred - Yp) ^ 2)
						}
						curr = curr + 1
						MSE[curr] = SSE / n
						Nfeat[curr] = sum(model)
						Features[[curr]] =
							model
					}
	plot(Nfeat, MSE) #MISSING: plot MSE against number of features
	abline(h = MSE[which.min(MSE)], col = "red")
	i = which.min(MSE)
	return(list(CV = MSE[i], Features = Features[[i]]))
}

data("swiss")
myCV(as.matrix(swiss[, 2:6]), swiss[[1]], 5)