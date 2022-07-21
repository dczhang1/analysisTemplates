### Template for descriptive table and correlation table

###### DESCRIPTIVE STATISTICS #######
        ### Summary Statistics
                #library(psych)
                psych::describe(xData)
        
        ### Histograms of Specific Vars
                ggplot(xData, aes(var1)) +
                        geom_histogram(bins = 20) 
        
        ### Frequency Tables
                # creates a frequency table of a single variable
                table(xData$var1)
                
                #this just counts number of observations of a variable
                plyr::count(xData, 'var1') 
                #Can be used to count marginal tables
                plyr::count(xData, c('sex','ethnicity'))
                
        ### Correlation Tables
                #Printable pearson's correlation matrix
                #library(apaTables)
                apa.cor.table(cor.data, filename = "Outputs/cortable.rtf", show.conf.interval = F)
                
                #Create correlation object
                cor.table <- cor(na.omit(cor.data))
                
                        
### Calculating summary statistics split by groups 

                        df %>%
                                group_by(FactorA, FactorB) %>%
                                dplyr::summarise(mean.DV = mean(DV), sd.DV = sd(DV), n = length(DV))
                        
### Create a HTML document of variable summary
                        library(summarytools)
                        view(dfSummary(df))
                        
