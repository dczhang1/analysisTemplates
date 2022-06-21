### dplyr package ###
### Basic codes

    
### Main verbs ###########
# select: return a subset of columns of dataframe
# filter: extract a subset of rows
# arrange: reorder rows of data
# rename: rename variables in a data frame
# mutate: add new variables/columns or transform existing vars
# summarize: generate summary statistics of different vars in data frame
#############################

### Notes ###    
# All functions takes in data frame as first argument
# Subsequent argument describes the function. 
# Result is new dataframe
# Takes in tidy data (row for observation and column for vars)

library(dplyr)
library(ggplot2)
select <- dplyr::select # make sure not to use MASS package

# Describe basic data.frame
    dim(iris)
    names(iris)
    df <- as.data.frame(iris)

# Note: the head function just prints the first few rows of the data for illustrative purposes. 
# Will not need head() when using these functions
    
# Select #
    # Reorder variables such at species goes first
        head(select(df, Species, everything()))
    # Select only range of variables
        head(select(df, Sepal.Length:Petal.Length))
    # De-select a variable
        head(select(df, -Sepal.Length))
        
# Filter #
    # Take rows under a condition
        head(filter(df, Sepal.Length > 5))
    # Take rows under multiple conditions
        head(filter(df, Sepal.Length > 5, Petal.Width < .3))
    
# Arrange #
    # Order by a variable
        head(arrange(df, Sepal.Length))
    # Arrange by descending
        head(arrange(df, desc(Sepal.Length)))

# Rename #
    # Rename a variable
        head(rename(df, SepalLen = Sepal.Length))
        
# Mutate #
    # Create a new variable
        # e.g., Centered variable
            head(mutate(df, centered.Sepal = Sepal.Length - mean(Sepal.Length, na.rm=T)))
        # e.g., z-Standardize
            head(mutate(df, std.Sepal = Sepal.Length-mean(c(Sepal.Length)/sd(Sepal.Length))))
        # e.g., Row means (not necessarily better this way though)
            head(mutate(df, mean.length= rowMeans(data.frame(Sepal.Length, Petal.Length))))
        # e.g., Row sums    
            head(mutate(df, sum.length= rowSums(data.frame(Sepal.Length, Petal.Length))))
            
            
# Groupby #
    # Create a grouped data.frame so they are summarized by groups
        groupdf <- group_by(df, Species)
        
        # Now data summaries are produced by groups
            dplyr::summarize(groupdf, meansepal = mean(Sepal.Length), maxsepal = max(Sepal.Length), medianpetal = median(Petal.Length))
        
    # Create a grouping variable that splits data by median of Sepal Length
            mutate(df, med.sep.len = factor(Sepal.Length>median(Sepal.Length), labels = c("Short","long")))


            