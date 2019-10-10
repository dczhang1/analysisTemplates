# CFA and SEM with lavaan

library(lavaan)
library(semTools)
order = c("df","chisq","rmsea","srmr","cfi","tli","bic","wrmr") #list of SEM fit indices

# Models
            # Simple Measurement Model
            model.1 <- '
                 latentA =~ x1 + x2 + x3
                 labentB =~ x4 + x5 + x6
                 latentC =~ x7 + x8 + x9
              '

            # Simple Latent path model
            model.1 <- '
                 latentA =~ x1 + x2 + x3
                 latentB =~ x4 + x5 + x6
                 latentC =~ x7 + x8 + x9
                 
                 latentA ~ latentB + latentC
              '
            

# Data Fitting Procedures
            # ML
                        fit.1 <- sem(model.1, data = df, missing = "ML", orthogonal = F, std.lv = TRUE)            
            # Bootstrapped estimates
                        fit.1 <- sem(model.1, data = df , se = "bootstrap", bootstrap = 5000, missing = "ML", orthogonal = F, std.lv = TRUE)

# Summary procedures
            # Overall summary
                        summary(fit.1, fit.measures=TRUE, standardized = T)
            # Modification indices
                        modindices(fit.1, sort. = T)
            # Fit measures only
                        fitMeasures(fit.1, fit.measures = order)
            # Bootstrapped CIs
                        parameterEstimates(fit.1, se = T, pvalue = T, boot.ci.type = "bca.simple")