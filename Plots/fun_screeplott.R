### APA Quality Parallel Analysis Scree Plot
### Custom function to creating APA quality Scree Plot for Parallel Analysis 
### Source: http://www.statmethods.net/management/userfunctions.html

fun.parallel <- function(parallelOut) {  
        obs = data.frame(parallelOut$fa.values)
        obs$type = c('Observed Data')
        obs$num = c(row.names(obs))
        obs$num = as.numeric(obs$num)
        colnames(obs) = c('eigenvalue', 'type', 'num')
        
        
        percentile = apply(parallelOut$values,2,function(x) quantile(x,.95))
        min = as.numeric(nrow(obs))
        min = (4*min) - (min-1)
        max = as.numeric(nrow(obs))
        max = 4*max
        percentile1 = percentile[min:max]
        
        #Create data frame called &amp;amp;amp;amp;amp;quot;sim&amp;amp;amp;amp;amp;quot; with simulated eigenvalue data
        sim = data.frame(percentile1)
        sim$type = c('Simulated Data (95th %ile)')
        sim$num = c(row.names(obs))
        sim$num = as.numeric(sim$num)
        colnames(sim) = c('eigenvalue', 'type', 'num')
        
        #Merge the two data frames (obs and sim) together into data frame called eigendat
        eigendat = rbind(obs,sim)
        
        apatheme=theme_bw()+
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      panel.border = element_blank(),
                      text=element_text(family='Arial'),
                      legend.title=element_blank(),
                      legend.position=c(.7,.8),
                      axis.line.x = element_line(color='black'),
                      axis.line.y = element_line(color='black'))
        
        #Use data from eigendat. Map number of factors to x-axis, eigenvalue to y-axis, and give different data point shapes depending on whether eigenvalue is observed or simulated
        p = ggplot(eigendat, aes(x=num, y=eigenvalue, shape=type)) +
                #Add lines connecting data points
                geom_line()+
                #Add the data points.
                geom_point(size=4)+
                #Label the y-axis 'Eigenvalue'
                scale_y_continuous(name='Eigenvalue')+
                #Label the x-axis 'Factor Number', and ensure that it ranges from 1-max # of factors, increasing by one with each 'tick' mark.
                scale_x_continuous(name='Factor Number', breaks=min(eigendat$num):max(eigendat$num))+
                #Manually specify the different shapes to use for actual and simulated data, in this case, white and black circles.
                scale_shape_manual(values=c(16,1)) +
                #Add vertical line indicating parallel analysis suggested max # of factors to retain
                geom_vline(xintercept = parallelOut$nfact, linetype = 'dashed')+
                #Apply our apa-formatting theme
                apatheme
        #Call the plot. Looks pretty!
        p
}