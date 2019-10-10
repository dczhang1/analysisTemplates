### Template for Plotting two-way interactions
### Source: https://sakaluk.wordpress.com/2015/08/27/6-make-it-pretty-plotting-2-way-interactions-with-ggplot2/#2cat
### ToC
        # PART I. Two categorical vars
        # PART II. One catagorical and one continuous vars
        # PART III. Two cont vars

### PART I. Two Categorical Vars
        ### Create new data.frame just for plotting
                dat <- xData

        ### Make sure the grouping vars are facts:
                dat$fact1=as.factor(dat$factor1)
                dat$fact2=as.factor(dat$factor2)

        ### Create dataframe to store summary statistics: change dv accordingly
                dat2 = describeBy(dat$dv,list(dat$fact1,dat$fact2), mat=TRUE,digits=2)

        ### Create names for factors: rename fact1name and fact2name accordingly. Leave group1 and group2 alone.
                names(dat2)[names(dat2) == 'group1'] = "fact1name"
                names(dat2)[names(dat2) == 'group2'] = "fact2name"

        ### Give names to levels: rename the levels
                levels(dat2$fact1name)[levels(dat2$fact1name)=='0'] = 'Control1'
                levels(dat2$fact1name)[levels(dat2$fact1name)=='1'] = 'Experimental2'

                levels(dat2$fact2name)[levels(dat2$fact2name)=='0'] = 'Control1'
                levels(dat2$fact2name)[levels(dat2$fact@name)=='1'] = 'Experimental2'

        ### Create standard errors: do not touch
                dat2$se = dat2$sd/sqrt(dat2$n)

        ### Plotting:
                limits = aes(ymax = mean + (1.96*se), ymin=mean - (1.96*se))
                dodge = position_dodge(width=0.9)
                apatheme=theme_bw()+
                theme(panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                panel.border=element_blank(),
                axis.line=element_line(),
                text=element_text(family='Times'))
                
                # May need to rename the aes() input depending on level names
                p=ggplot(dat2, aes(x = Control, y = mean, fill = Experimental))+
                geom_bar(stat='identity', position=dodge)+
                geom_errorbar(limits, position=dodge, width=0.25)+
                apatheme+
                ylab('DV')+
                scale_fill_grey()
                print(p)

### PART II. One cat and one cont vars
        ### Create new data.frame just for plotting
                dat <- xData
        
        ### Make sure categorical var is a factor
                dat$fact1 = as.factor(dat$fact1)
        
        ### Run linear model
                plotModel <- lm(dv ~ iv1 * fact1, data = dat)
                summary(plotModel)
                dat$predicted = predict(plotModel)
                
                
        ### Store apa template
                apatheme=theme_bw()+
                        theme(panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank(),
                              panel.border=element_blank(),
                              axis.line=element_line(),
                              text=element_text(family='Times'))
        
        ### Give level names
                #Optional if levels of factor already labeled
                levels(dat$fact1)[levels(dat$fact1)=='0'] = 'Control'
                levels(dat$fact1)[levels(dat$fact1)=='1'] = 'Experimental'
                
        ### Create plot
                p=ggplot(dat, aes(x=iv1, y=dv, shape=fact1))+
                        geom_point()+
                        scale_shape_manual(values=c(1,16), name='Factor1', labels=c('Control','Experimental'))+
                        geom_line(aes(x = wt, y = predicted, linetype=am)) +
                        scale_linetype_discrete(name='Factor1', labels=c('Control','Experimental'))+
                        labs(x = 'IV Label', y = 'DV Lab')+
                        apatheme
                print(p)
        
                
### PART III. Two cont vars
        ### Create new data.frame just for plotting
                dat <- xData
                
        ### Run linear model
                plotModel <- lm(dv ~ iv1 * iv2, data = dat)
                vcov(plotModel)
                describe(dat)
        
        ### Creat APA theme        
                apatheme=theme_bw()+
                        theme(panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank(),
                              panel.border=element_blank(),
                              axis.line=element_line(),
                              text=element_text(family='Times'))
                
        ### Create Plot
                p=ggplot(dat, aes(x = iv1, y = dv, size = iv2))+
                        geom_point()+
                        labs(x = 'X Label', y = 'Y Label')+
                        scale_size_continuous(guide = FALSE)+
                        ### Add different slopes: can remove for less lines 
                        ### Make sure to change intercept and slope values based on data
                        geom_abline(aes(intercept=33.965, slope=-4.3985, linetype='-1SD IV2'))+
                        geom_abline(aes(intercept=38.1208, slope=-5.854, linetype='Mean IV2'))+
                        geom_abline(aes(intercept=42.2767, slope=-7.3095, linetype='+1SD IV2'))+
                        scale_linetype_manual(values=c('dotted','dashed','solid'),
                                              breaks=c('-1SD IV2','Mean IV2','+1SD IV2'),name='Simple\nSlope')+
                        apatheme
                p