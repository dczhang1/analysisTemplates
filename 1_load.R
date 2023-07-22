### Install and Load Packages ###
        
        install.packages("ggplot2")

### Load packages
        library(ggplot2)

### Other remote installs
        remotes::install_github("dgrtwo/drlib") # for topic modeling and text analysis

### Other useful Utilities ###
        ### CFA
                order <- c("df", "chisq", "rmsea", "srmr", "cfi", "tli", "bic", "wrmr") # list of SEM fit indices
        
        ### Override conflict functions
                alpha <- psych::alpha
                select <- dplyr::select
                filter <- dplyr::filter
                summarize <- dplyr::summarise
