ppcFun <- function(mean.preT, mean.postT, mean.preC, mean.postC, sd.preT, sd.preC, n.C, n.T){
            # mean.preT = mean, pretest, treatment
            # mean.preC = mean, pretest, control
            # mean.postT = mean, posttest, treatment
            # mean.postC = mean, posttest, control
            # sd.preT = sd, pretest, treatment
            # sd.preC = sd, posttest, control
            # n.C = N of control group
            # n.T = N of treatment group
            
            #### comppute SDpre first (9)
            
            sdpre <- sqrt(
                        ((n.T - 1)*((sd.preT)^2) + (n.C - 1)*((sd.preC)^2))
                        /(n.T + n.C -2)
                        )
            
            #### Compute Cp next (10)
            
            cp <- 1 - (3/(4*(n.T + n.C - 2) - 1))
            
            #### Compute final effect size
            
            dppc <- cp * (((mean.postT - mean.preT) - (mean.postC - mean.preC))/(sdpre))
            dppc
}


ppcFun(mean.preT, mean.postT, mean.preC, mean.postC, sd.preT, sd.preC, n.C, n.T)

# dppc3 (Morris, 2008, ORM)