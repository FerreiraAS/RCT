# LAST MODIFIED: JUNE 18, 2021

# REINICIA TODAS AS VARIÁVEIS
rm(list = ls(all = TRUE))

# OCULTA TODOS OS AVISOS
options(warn = -1)
quartz.options(width = 9, height = 4, dpi = 150)

# INSTALA E ABRE OS PACOTES UTILIZADOS
require(readxl)
library(readxl)
require(foreign)
library(foreign)

# ANÁLISE COMPARATIVA
source("RCT_Figure1.R") # numeric variables, plot of descriptive analysis (mean and CI)
source("RCT_Table2a.R") # numeric variables, linear mixed model analysis, between- AND within-factor WITH baseline adjustment
source("RCT_Table2b.R") # numeric variables, linear mixed model analysis, between- AND within-factor WITHOUT baseline adjustment
source("RCT_Table3.R") #  ordinal variables, ridit analysis, ONLY within-factor

# CARREGA O BANCO
banco_Julia <- data.frame(read_excel("banco_julia_rct_final_III.xls", sheet = 1))
attach(banco_Julia)
GRUPOS <- banco_Julia[, 2]
GRUPOS <- factor(GRUPOS, levels = c("X", "Z"), labels = c("CORE-MT", "CFT"))
FASES <- c("BL", "8w", "26w", "52w")
FASES.N <- c(0, 8, 26, 52)
ALPHA <- 0.05

banco_Julia$BPA_1 <- as.numeric(factor(BPA_1, levels = c("sim", "não"), labels = c(1, 0)))
banco_Julia$BPA_2 <- as.numeric(factor(BPA_2, levels = c("sim", "não"), labels = c(1, 0)))
banco_Julia$BPA_3 <- as.numeric(factor(BPA_3, levels = c("sim", "não"), labels = c(1, 0)))

# DELINEMANENTO DO ESTUDO
DESFECHO.1 <- colnames(cbind(INTENS_DOR, INTENS_DOR_1, INTENS_DOR_2, INTENS_DOR_3))
DESFECHO.2 <- colnames(cbind(ODI_TOTAL, ODI_1_TOTAL, ODI_2, ODI_3))
DESFECHO.3 <- colnames(cbind(PERC_EF_1, PERC_EF_2, PERC_EF_3))
DESFECHO.4 <- colnames(cbind(SATISF_1, SATISF_2, SATISF_3))
DESFECHO.5 <- colnames(cbind(BPA_1, BPA_2, BPA_3))

# COVARIAVEL
COVARIATE <- NULL

# HANDLING MISSING VALUES: "complete.cases", "mean.imputation", "multiple.imputation"
MISSING <- "multiple.imputation"
M <- 50

dev.new()
layout(matrix(seq(1:3), nrow = 1, ncol = 3, byrow = TRUE), heights = rep(1), widths = rep(1, 1, 1))

TABLE.2a(dataset = banco_Julia, variables = DESFECHO.1, covariate = COVARIATE, bw.factor = GRUPOS, control.g = "CORE-MT", wt.labels = FASES, missing = MISSING, 
	m.imputations = M, alpha = ALPHA, n.digits = 2)
FIGURE.1(dataset = banco_Julia, variables = DESFECHO.1, bw.factor = GRUPOS, wt.labels = FASES.N, missing = MISSING, m.imputations = M, xlabs = "Weeks", 
	ylab = "Pain intensity", alpha = 0.05)

TABLE.2a(dataset = banco_Julia, variables = DESFECHO.2, covariate = COVARIATE, bw.factor = GRUPOS, control.g = "CORE-MT", wt.labels = FASES, missing = MISSING, 
	m.imputations = M, alpha = ALPHA, n.digits = 2)
FIGURE.1(dataset = banco_Julia, variables = DESFECHO.2, bw.factor = GRUPOS, wt.labels = FASES.N, missing = MISSING, m.imputations = M, xlabs = "Weeks", 
	ylab = "Incapacity", alpha = 0.05)

TABLE.2b(dataset = banco_Julia, variables = DESFECHO.3, covariate = COVARIATE, bw.factor = GRUPOS, control.g = "CORE-MT", wt.labels = FASES[-1], missing = MISSING, 
	m.imputations = M, alpha = ALPHA, n.digits = 2)
FIGURE.1(dataset = banco_Julia, variables = DESFECHO.3, bw.factor = GRUPOS, wt.labels = FASES.N[-1], missing = MISSING, m.imputations = M, xlabs = "Weeks", 
	ylab = "Perceived efficacy", alpha = 0.05)

TABLE.3(dataset = banco_Julia, variables = DESFECHO.4, bw.factor = GRUPOS, control.g = "CORE-MT", wt.labels = FASES[-1], alpha = ALPHA, n.digits = 2)

TABLE.3(dataset = banco_Julia, variables = DESFECHO.5, bw.factor = GRUPOS, control.g = "CORE-MT", wt.labels = FASES[-1], alpha = ALPHA, n.digits = 2)

detach(banco_Julia)