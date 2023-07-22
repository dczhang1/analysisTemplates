### Template for MANOVA with ez package

### Library
        library(ez)
        
### Create aggreget DV
        agg.DV <- cbind(xData$DV1, xData$DV2)
        manova.model <- lm(agg.DV ~ factor.1 + factor.2, data = xData, 
                           contrasts=list(factor.1 = contr.sum, factor.2 = contr.sum))
        manova.out <- Manova(manova.model, type = "III")
        summary(manova.out, multivariate = T)