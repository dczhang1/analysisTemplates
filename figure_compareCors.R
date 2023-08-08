### Compare correlations
### This figure creates bar plots comparing the magnitude of correlation between two variables to the remainder
### of the variables in the dataset. It is best used when there are just two variables. 

#install.packages(c("ggplot2", "tidyr"))
#library(ggplot2)
#library(tidyr)

plotPredictiveValidity <- function(data, predictor_vars, alpha = 0.05, colors = c("#1f77b4", "#ff7f0e"), 
                                   title = "Predictive Validity", xlab = "Correlation Coefficient") {
            
            # Check if predictor_vars are in the data
            if (!all(predictor_vars %in% colnames(data))) {
                        stop("Specified predictor_vars are not all in the input data.")
            }
            
            # Compute the significant correlation cutoff based on sample size
            n <- nrow(data)
            t_critical <- qt(1 - alpha/2, df = n - 2)
            sig_cutoff <- t_critical / sqrt(n + t_critical^2)
            
            # Compute correlations
            outcome_vars <- setdiff(colnames(data), predictor_vars)
            corr_data <- sapply(predictor_vars, function(pred) {
                        sapply(outcome_vars, function(outcome) {
                                    cor(data[[pred]], data[[outcome]], method = "pearson")
                        })
            })
            
            # Preparing the data for ggplot
            data_long <- as.data.frame(corr_data)
            data_long$Outcome <- factor(rownames(data_long), levels = outcome_vars)
            data_long <- tidyr::gather(data_long, key = "Predictor", value = "Correlation", -Outcome)
            
            # Plotting
            p <- ggplot(data_long, aes(y = Outcome, x = Correlation, fill = Predictor)) + 
                        geom_bar(stat = "identity", position = "dodge", width = 0.4) +
                        geom_vline(aes(xintercept = sig_cutoff), linetype = "dashed", color = "black") +
                        geom_vline(aes(xintercept = -sig_cutoff), linetype = "dashed", color = "black") +
                        scale_fill_manual(values = colors) +
                        labs(title = title, x = xlab, y = NULL, fill = NULL) +
                        theme_minimal() +
                        theme(axis.text.y = element_text(size = 12),
                              axis.text.x = element_text(size = 12),
                              axis.line = element_line(colour = "black"),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank())
            
            return(p)
}

# Example usage:
# data <- data.frame(X1 = rnorm(100), X2 = rnorm(100), X3 = rnorm(100), X4 = rnorm(100))
# plotPredictiveValidity(data, predictor_vars = c("X1", "X2"))
