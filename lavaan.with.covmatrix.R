### Code for SEM with correlation matrix

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
        
        