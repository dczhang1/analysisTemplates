### ANOVA and ANCOVA Template
        ### Set model parameters
                anova.model.1 <- aov(dv1 ~ factor1 * factor2 + covariate1, data = xData) #covariate for ANCOVA
        ### ANOVA Table        
                Anova(anova.model.1)
        ### Create APA ANOVA summary table
                apaTables::apa.aov.table(anova.model.1, filename = "anovaModel_1.doc")
        ### Create APA table of means
                apaTables::apa.2way.table(factor1, factor2, dv1, xData, filename = "anovaModelMeans_1.doc", show.marginal.means = T, show.conf.interval = T, landscape = T)
        ### Pairwise tests
                pairwise.t.test(dv1, factor1, p.adjust="holm", pool.sd = T)  
                
                