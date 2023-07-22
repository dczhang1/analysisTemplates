#Generate random data based on correlation matrix
#http://statistical-research.com/simulating-random-multivariate-correlated-data-continuous-variables/

R = matrix(cbind(1,.75,.25,  .75,1,0,  .25,0,1),nrow=3)

#Set Correlation Matrix
        # CogAbility,Consc,Level 1 Interview, J-Performance (Cortina et al. PPsych 2000)
                #R = matrix(cbind(1,.075,.055,  .45,.075,1,.092,.267,.055,.092,1,.2,.45,.267,.2,1),nrow=4)
        # CogAbility,Consc,Level 2 Interview, J-Performance (Cortina et al. PPsych 2000)
                #R = matrix(cbind(1,.075,.253,  .45,.075,1,.161,.267,.253,.161,1,.35,.45,.267,.35,1),nrow=4)
        # CogAbility,Consc,Level 3-4 Interview, J-Performance (Cortina et al. PPsych 2000)
                #R = matrix(cbind(1,.075,.270,  .45,.075,1,.258,.267,.27,.258,1,.56,.45,.267,.56,1),nrow=4)
U = t(chol(R))
nvars = dim(U)[1]
numobs = 100000
#set.seed(1)
random.normal = matrix(rnorm(nvars*numobs,0,1), nrow=nvars, ncol=numobs);
X = U %*% random.normal
newX = t(X)
raw = as.data.frame(newX)
orig.raw = as.data.frame(t(random.normal))

#Set names of variables
        #names(raw) = c("response","predictor1","predictor2")
        #names(raw) = c("GMA","Cons","Interv","Perform")
        names(raw) = c("Speed","Strength","Endurance")
cor(raw)

plot(head(raw, 10000))
plot(head(orig.raw,10000))

write.csv(head(raw,10000), file = "/Users/donzhang/Desktop/sim.cor.data.csv")
