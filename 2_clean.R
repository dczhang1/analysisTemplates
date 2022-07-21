### Data Manipulation and Management Codes
###

### Combining data frames to add observations with same variable names
        dfNew <- plyr::rbind.fill(df1,df2)
        
### Combine data by adding new columns/variables
        dfMerge <- merge(df.t1,df.t2,by="ID")
        
        ### Merge multiple dataframes by ID
        Reduce(function(x,y) merge(x = x, y = y, by = "ID"), 
               list(df.t1, df.t2, df.t3))
        
### Create list of variables to be called later
        varPredictors <- c(var1, var2, var3) #create the list of variable names
        select(df, varPredictors) #subset data with just those variables
        
### Create unique subject number
        df <- df %>%
                mutate(id = row_number())
        
### Calculating unweighted scale means (most are redundant)
        ### Preferred method: combine psych package and dplyr
        ### keys: -1 for reverse coded
        ### items: dataframe of all the items. Use dplyr::select
        df$meanScore <- as.vector(scoreItems(keys = c(1,1,1,1,1), items = select(df, var1:var5))$score)
        
        ### If you know item position -- not recommended because item positions may change
        df$meanScore <- rowMeans(df[,c(1,2,3,4,5)], na.rm=T)
        
        ### If you konw item names -- good method but requires reverse coding separately
        df$meanScore <- rowMeans(df[,c("IPIP1", "IPIP2", "IPIP3", "IPIP4", "IPIP5")], na.rm = T)
        
        
        ### More obscure
        ### Uses dplyr and dput to get names quickly
        df$meanScore <- rowMeans(df[,dput(names(select(df, var1:var10)))], na.rm=T)
        
        ### Use dplyr
        df %>% rowwise() %>% mutate(meanScore = mean(var1:var5))
        
        
### Multiple conditional for transformation
        df %>% mutate(g = case_when(a == 1 | a == 3 | a == 5 | (a == 1 & b == 4) ~ 2,
                                    a == 0 | a == 1 | a == 4 | a == 3 |  c == 4 ~ 3,
                                    TRUE ~ NA_real_))  
        
        ## Recode one item
        library(plyr)
        df$newcode <- as.numeric(mapvalues(df$scode, from = c("Four", "Six", "Eight"), to = c(4, 6, 8)))
        
        ## Recode multiple items to same coding scheme
        df_recode <- df %>% 
                mutate_at(c("ITEM_1","ITEM_2"), 
                          funs(recode(., "Strongly disagree"=1, 
                                      "Somewhat disagree"=2,
                                      "Neither agree nor disagree"=3,
                                      "Somewhat agree"=4,
                                      "Strongly agree" = 5,
                                      .default = NaN)))
        
       
        
### Create dummy variables
        df$DUMMY_A <- ifelse(df$FACTOR_A=="A", 1,0)
        
        
### Create data for MPlus 
        MplusAutomation::prepareMplusData(df, "df.dat", inpfile=TRUE)
        