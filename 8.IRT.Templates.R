### Item response theory
library(ltm)

### Unidimensional Graded Response Model
            # df should contain only vars for the analysis
            grm.model.1 <- grm(df)
            summary(grm.model.1)
            coef(grm.model.1)
            plot(grm.model.1, type = "ICC")
            plot(grm.model.1, type = "IIC")

### Dichotomous data
            ### https://www.youtube.com/watch?v=L1S7o49r0nI
            
            ###Library
                        library(ltm)
                        library('mirt')
                        library('shiny')
            ###Load up sample data
            data <- expand.table(LSAT7)
            
            ###Look at summary statistics
            summary(data)
            
###Running a 2PL and 3PL models
            ### Parameters:
            ### Location/difficult parameter: location where item discriminates between top and bottom 50%
                        #Negative = too easy, measures lower end of distribution
                        #Zero = discriminating middle of the skill distribution
                        #Positive = more difficult, discriminating top people only
            ### Discrimination
                        #It is the slope. 
            ### Guess parameter: some items are easy to guess. 
                        #Could be higher if the alternatives are poor
                        #Could be lower
### 2PL
            #Specify model
                        model.2pl = ltm(data ~ z1, IRT.param = T)
            #See model coefficients
                        coef(model.2pl)
                        summary(model.2pl)
            #Plot 
            #Item charateristics curve
                        plot(model.2pl, type="ICC")
                        abline(.5,0)
            #one curve
                        plot(model.2pl, type="ICC", items = 5)
            #Item information curve
                        plot(model.2pl, type="IIC")
            #Test average. Addition of all items IIC
                        plot(model.2pl, type="IIC", item = 0)
            #Information
            #See individual scores and underlying latent scores
                        factor.scores(model.2pl)
            #Alternative hypothesis: inconsistent response pattern... want p-value to be non-sig
                        person.fit(model.2pl)
            #Item fit
                        item.fit(model.2pl)
### 3PL
            #Includes the guessing parameter
            #Specify model
                        model.3pl <- tpm(data, type = "latent.trait", IRT.param=T)
            #Coefficients
                        coef(model.3pl)
            #plots
                        plot(model.3pl, type = "ICC")
                        plot(model.3pl, type = "IIC")
            #Information
                        factor.scores(model.3pl)
                        person.fit(model.3pl)
                        item.fit(model.3pl)
            #Compare models
                        anova(model.2pl, model.3pl)
                        summary(model.3pl)
                        
            
            