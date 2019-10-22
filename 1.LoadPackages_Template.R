### Load Packages ###

my_packages <- c("devtools","tidyverse", "broom", "coefplot", "cowplot",
                 "gapminder", "GGally", "ggrepel", "ggridges", "gridExtra",
                 "here", "interplot", "margins", "maps", "mapproj",
                 "mapdata", "MASS", "quantreg", "rlang", "scales",
                 "survey", "srvyr", "viridis", "viridisLite", "devtools", "psych",
                 "lavaan", "ltm", "apaTables","jmv", "ggstatsplot", "semTools", "ez",
                 "pacman", "papaja", "relimp","semproducible")
install.packages(my_packages, repos = "http://cran.rstudio.com")

### Load multiple packages at once
    pacman::p_load(readr, tidyverse, psych, lavaan, psych, readr, ltm, plyr, apaTables, jmv, haven, papaja, pacman, 
                   relimp) 
        
    
### Other useful Utilities ###
  ### CFA
    order = c("df","chisq","rmsea","srmr","cfi","tli","bic","wrmr") #list of SEM fit indices

### Override conflict functions
    alpha <- psych::alpha
    select <- dplyr::select
    filter <- dplyr::filter
    summarize <- dplyr::summarise
            