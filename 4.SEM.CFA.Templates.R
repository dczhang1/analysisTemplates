# CFA and SEM with lavaan

library(lavaan)
library(semTools)
order = c("df","chisq","rmsea","srmr","cfi","tli","bic","wrmr") #list of SEM fit indices

# Models
        ### Naming convention
                # Models are named with: [model type].model.[modelID]
                # Model type = 'sem', 'path', 'cfa' 
        # Simple Measurement Model
            cfa.model.1 <- '
                 latentA =~ x1 + x2 + x3
                 labentB =~ x4 + x5 + x6
                 latentC =~ x7 + x8 + x9
              '
            # Simple Latent path model
            sem.model.1 <- '
                 latentA =~ x1 + x2 + x3
                 latentB =~ x4 + x5 + x6
                 latentC =~ x7 + x8 + x9
                 latentA ~ latentB + latentC
              '
            # Simple path model
            path.model.1 <- '
                 x1 ~ x2 + x3
              '
            

# Data Fitting Procedures
            # ML
                        sem.fit.1 <- sem(sem.model.1, data = df, missing = "ML", orthogonal = F, std.lv = TRUE)            
            # Bootstrapped estimates
                        sem.fit.1 <- sem(sem.model.1, data = df , se = "bootstrap", bootstrap = 5000, missing = "ML", orthogonal = F, std.lv = TRUE)

# Summary procedures
            # Overall summary
                        summary(sem.fit.1, fit.measures=TRUE, standardized = T)
            # Modification indices
                        modindices(sem.fit.1, sort. = T)
            # Fit measures only
                        fitMeasures(sem.fit.1, fit.measures = order)
            # Bootstrapped CIs
                        parameterEstimates(sem.fit.1, se = T, pvalue = T, boot.ci.type = "bca.simple")
                        

### Script for recreating lavaan with correlation matrix
                        ### Create correlation matrix
                        ### List of variable names
                        varList <- c("ccb", "pcb", "pcv", "cwb")
                        
                        ### Convert vector of correlations to matrix
                        example.cor <- lav_matrix_lower2full (
                                c(1,
                                  .46, 1,
                                  .52, .77, 1,
                                  .24, .26, .35, 1))
                        
                        ### name variables in matrix
                        colnames(example.cor) <- rownames(example.cor) <- varList
                        
                        ### Enter the SDs
                        example.sd <- c(.90, 1.01, 1.08, .79)
                        names(example.sd) <- varList
                        
                        ### Convert cor to cov
                        example.cov <- cor2cov(example.cor, example.sd)
                        
                        ### Analyze model with cov matrix
                        example.model <-'
                                        cwb ~ pcv
                                        pcv ~ pcb
                                        pcb ~ ccb
        '
                        ### Fit model
                        example.fit <- sem(example.model, sample.cov = example.cov, sample.nobs = 250, std.lv = F)
                        summary(example.fit)
                        fitmeasures(example.fit)
                        
                        