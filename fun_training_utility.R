### Economic Utility of Training
# Source: Cascio & Aguinis, Applied Psychology in Talent Management, pg416

# Model Parameters

# Yrs = Number of years duraction of training effect on performance (Default = 2)
# N = Number of persons trained
# SDy = Variability in performance in dollars
            # For low-complexity jobs, SDy is estimated to equal 40 percent of the job’s salary;
            # For moderate complexity jobs, it is equal to 60 percent of salary;
            # For high complexity jobs, SDy is equal to 100 percent of salary.
# Cost = cost per training
# perfTrained = Avg performance of trained employees
# perfUntrained = Avg performance of untrained employees
# SDperformance = SD of performance in untrained population
# ryy = Reliability of performance measure

# Notes
function_training_utility <- function(Yrs = 3, N, SDy = 40000, cost, 
                                      perfTrained, perfUntrained, 
                                      SDperformance = 1, ryy = .85){
            
            dt = (perfTrained - perfUntrained)/SDperformance*sqrt(ryy)
            DeltaU = Yrs * N * dt * SDy - (cost * N)
            return(round(DeltaU, 0))
            
}

function_training_utility(N = 75, perfTrained = 4.9, perfUntrained = 4.5, cost = 6666)

50000/30
