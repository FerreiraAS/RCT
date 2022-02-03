FIGURE.1 <- function(dataset, variables, covariate, bw.factor, wt.labels, missing, m.imputations, xlabs, ylab, alpha) {
	# LAST MODIFIED: NOV 30, 2021
	
	# required packages
	require("Rmisc")
	library("Rmisc")

	# This function outputs a plot for two-way mixed-models.
	# dataset: a 2D dataframe (rows: participants, columns: variables)
# variables: a 1D variable labels (within-group)
# bw.factor: a 1D between-group factor
# wt.labels: a 1D variable labels for each
# level missing: method for handling missing balues (mean inputation, last value carried forward)
# xlabs: a 1D vector of labels for the X axis (within-group factor)
# ylabs: a string label for the Y axis (outcome)

	# confirma a estrutura dos dados
	dataset <- data.frame(dataset)
	bw.factor <- factor(bw.factor, exclude = NULL)

	# remove variaveis nao usadas
	dataset <- dataset[, colnames(dataset) %in% variables]

	# preparação e análise do modelo misto
	ID_M <- rep(seq(1:length(bw.factor)), length(wt.labels))
	TIME_M <- as.factor(c(rep(seq(1, length(wt.labels)), each = length(bw.factor))))
	GROUP_M <- rep(bw.factor, length(wt.labels))
	OUTCOME_ORIG <- c(as.matrix(dataset))
	OUTCOME_M <- c(as.matrix(dataset))
	if (!is.null(covariate)) {
		COVARIATE_M <- rep(covariate, length(wt.labels))
	}

	# decide como lidar com os dados perdidos
	if (missing == "complete.cases") {
		include <- complete.cases(dataset)
		dataset <- dataset[include == TRUE, ]
		bw.factor <- bw.factor[include == TRUE]
		ID_M <- rep(seq(1:length(bw.factor)), length(wt.labels))
		TIME_M <- as.factor(c(rep(seq(1, length(wt.labels)), each = length(bw.factor))))
		GROUP_M <- rep(bw.factor, length(wt.labels))
		OUTCOME_ORIG <- c(as.matrix(dataset))
		OUTCOME_M <- c(as.matrix(dataset))
		if (!is.null(covariate)) {
			COVARIATE_M <- COVARIATE_M[include == TRUE]
		}
	}
	if (missing == "mean.imputation") {
		# calcula a média para imputação para cada grupo
		for (i in 1:length(wt.labels)) {
			temp.imp <- dataset[, i]
			for (j in 1:nlevels(bw.factor)) {
				temp.imp[which(is.na(temp.imp) & bw.factor == levels(bw.factor)[j])] <- mean(temp.imp[which(bw.factor == levels(bw.factor)[j])], na.rm = TRUE)
			}
			dataset[, i] <- temp.imp
		}
		OUTCOME_M <- c(as.matrix(dataset))
		if (!is.null(covariate)) {
			COVARIATE_M[is.na(COVARIATE_M)] <- mean(COVARIATE_M, na.rm = TRUE)
		}
	}

	# calculate and plot CI
	myCI <- group.CI(OUTCOME_M ~ GROUP_M * as.factor(TIME_M), data = cbind(GROUP_M, as.factor(TIME_M), OUTCOME_M), ci = 1 - alpha)
	myCI[, 2] <- rep(wt.labels, each = nlevels(bw.factor))
	symbols <- c(15, 16, 17, 18)

	jitter <- rep(seq(from = -0.25, to = 0.25, length.out = nlevels(bw.factor)), each = length(wt.labels))
	jitter <- matrix(jitter, nrow = nlevels(bw.factor), ncol = length(unique(wt.labels)), byrow = TRUE)

	# interaction plot with %CI
	# start empty area plot with main setup
	plot(NA, xaxt = "n", xlab = xlabs, ylab = ylab, xlim = c(min(wt.labels) - 1.5, max(wt.labels) + 1.5), ylim = c(min(myCI[, 5]) - min(myCI[, 5]) * 
		0.1, max(myCI[, 3]) + max(myCI[, 3]) * 0.1))
	# plot CI intervals (vertical lines)
	for (i in 1:dim(myCI)[1]) {
		lines(x = rep(myCI[i, 2], 2) + jitter[i], y = c(myCI[i, 5], myCI[i, 3]), lty = "solid", lwd = 1.5, col = "darkgrey")
		axis(side = 1, at = wt.labels, labels = wt.labels)
	}
	# plot CI intervals (symbols at end lines)
	for (i in 1:dim(myCI)[1]) {
		points(x = rep(myCI[i, 2], 2) + jitter[i], y = c(myCI[i, 5], myCI[i, 3]), pch = "−", cex = 1.5, lwd = 1.5, col = "darkgrey")
	}
	# plot point estimates
	for (i in 1:nlevels(bw.factor)) {
		lines(x = myCI[myCI[, 1] == i, 2] + jitter[i], y = myCI[myCI[, 1] == i, 4], type = "b", pch = symbols[i], lwd = 1, cex = 2)
	}
	# plot legend
	legend(x = "topleft", legend = levels(GROUP_M), pch = symbols, cex = 1.25, bty = "n", horiz = TRUE, x.intersp = 1)
}
