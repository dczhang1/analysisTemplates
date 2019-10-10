### Load Packages ###



### Fresh Installs of essential packages
        # From github
        utils::install.packages(pkgs = "devtools")         
                devtools::install_github("neuropsychology/psycho.R")  # Install the newest version
                devtools::install_github("crsh/papaja")
                devtools::install_github(
                  repo = "IndrajeetPatil/ggstatsplot", # package path on GitHub
                  dependencies = TRUE,                 # installs packages which ggstatsplot depends on
                  upgrade_dependencies = TRUE          # updates any out of date dependencies
                )
        
        # From CRAN
        install.packages(c("conflict","psych", "Hmisc", "tidyverse", "plyr","apaTables","DataExplorer","psycho",
                           "ggstatsplot","GPArotation","lavaan","ltm","semTools","rmarkdown","afex", "pacman", "ez", "jmv", "haven"))

### Load multiple packages at once
            pacman::p_load(tidyverse, psych, lavaan, psych, readr, ltm, plyr, apaTables, jmv, haven) 
        
### Other useful Utilities ###
  ### CFA
    order = c("df","chisq","rmsea","srmr","cfi","tli","bic","wrmr") #list of SEM fit indices

### Override conflict functions
    alpha <- psych::alpha
    select <- dplyr::select
    filter <- dplyr::filter
    summarize <- dplyr::summarise
            