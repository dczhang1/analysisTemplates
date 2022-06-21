##installing necessary packages
install.packages("ez")
install.packages("lsr")


# Read clipboard
read.excel <- function(header=TRUE,...) {
        read.table("clipboard",sep="\t",header=header,...)
}

dat=read.excel()

#loading required libraries
library(ez)
library(lsr)

#loading and examining example dataset
data(ANT)
head(ANT)

###############################
### BETWEEN SUBJECTS ANOVAS ###
###############################

b_anova <- ezANOVA(data = ANT,               #dataset
                         dv = rt,            #variable name for DV
                         wid = subnum,       #variable name with unique subject/sample identifier
                         between = group)    #between subject variables to include in the model
print(b_anova)


b_anova_full <- ezANOVA(data = ANT, 
                          dv = rt, 
                          wid = subnum, 
                          between = group, 
                          within_full = .(cue, flank), #include so that within cells are correctly averaged
                          type = 3,                    #default is 2, set to 3 to reproduce SPSS or SAS output
                          detailed = T,                #will give us more information (SS columns)
                          return_aov = T)              #will output an aov object, useful for other functions
print(b_anova_full)


#to get the partial eta squared for our design
etaSquared(x = b_anova_full$aov, type = 3)


################################
### REPEATED MEASURES ANOVAS ###
################################

#ANOVAs that include repeated measures factors
mixed_anova <- ezANOVA(data = ANT,                
                          dv = rt,                
                          wid = subnum,           
                          between = group,        
                          within = .(cue, flank),    #within subjects variables to include in the model         
                          detailed = T) 
print(mixed_anova)


################################
### PAIRWISE COMPARISONS ###
################################

pairwise_comparisons(
        data = xData,
        x = groupA,
        y = dv,
        type = "robust", 
        paired = F,
        p.adjust.method = "holm"
)

