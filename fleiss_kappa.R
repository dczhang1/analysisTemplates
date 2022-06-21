library(irr)

### Copy the ratings into clipboard and use the function below to paste. 
        read.excel <- function(header=TRUE,...) {
                read.table("clipboard",sep="\t",header=header,...)
        }


df.kappa <- read_excel("Coding for ICA/Round2/Keyword v2 ICA_kappa.xlsx")
kappam.fleiss(df.kappa, exact = T, detail = T)

