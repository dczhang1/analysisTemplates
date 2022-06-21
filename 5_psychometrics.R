### Basic Psychometrics Templates

### Internal Consistency
    ### Cronbach alpha
        psych::alpha(dplyr::select(df.clean, VALIDITY_1:VALIDITY_4), check.keys = T)



### EFA/PCA
    #Create data for efa
        efa.data <-  select(xData,var1:var5)
    # Parallel analysis
        pa.efa.1 = fa.parallel(efa.data, fm = "pa", fa = "fa")
        pa.efa.1$fa.values
    # Factor loadings
        fa(efa.data, nfactors = 3, rotate = "oblimin", fm = "pa")        
        
### Unidimensional IRT
        df.irt <- select(df.clean, item1:item10)      
        grm.validity <- grm(df.irt)
        summary(df.irt)
        coef(df.irt)
        plot(df.irt, type = "ICC")
        plot(df.irt, type = "IIC")