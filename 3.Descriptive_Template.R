### Template for descriptive table and correlation table

###### DESCRIPTIVE STATISTICS #######
        ### Summary Statistics
                #library(psych)
                psych::describe(descrip.data)
        
        ### Histograms of Specific Vars
                ggplot(descrip.data, aes(var1)) +
                        geom_histogram(bins = 20) 
        
        ### Frequency Tables
                #library(plyr)
                plyr::count(xData, 'var1')
                #Can be used to count marginal tables
                plyr::count(xData, c('sex','ethnicity'))
                
        ### Correlation Tables
                #Printable pearson's correlation matrix
                #library(apaTables)
                apa.cor.table(cor.data, filename = "cortable.rtf", show.conf.interval = F)
                
                #Create correlation object
                cor.table <- cor(na.omit(cor.data))
                
                        
### Calculating summary statistics split by groups 

                        df %>%
                                group_by(FactorA, FactorB) %>%
                                dplyr::summarise(mean.DV = mean(DV), sd.DV = sd(DV), n = length(DV))
                        
### Create a HTML document of variable summary
                        library(summarytools)
                        view(dfSummary(df))