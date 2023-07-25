### Basic Psychometrics Templates

### Internal Consistency
    ### Cronbach alpha
        psych::alpha(dplyr::select(df.clean, item1:item5), check.keys = T)

### EFA/PCA
    #Create data for efa
        efa.data <-  select(xData,item1:item5)
    # Parallel analysis
        pa.efa.1 = fa.parallel(efa.data, fm = "pa", fa = "fa")
        pa.efa.1$fa.values
    # Factor loadings
        fa(efa.data, nfactors = 3, rotate = "oblimin", fm = "pa")        
        
