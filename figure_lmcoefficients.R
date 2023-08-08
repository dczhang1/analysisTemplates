### Plots regression coefficients for multiple outcomes on rows, and two predictors

library(ggplot2)
library(broom)
library(dplyr)

figure_multi_regCoefs <- function(data, predictors, outcomes) {
        results <- list()
        
        # Loop through each outcome and fit a regression model
        for (outcome in outcomes) {
                formula_str <- paste(outcome, "~", paste(predictors, collapse = " + "))
                model <- lm(as.formula(formula_str), data = data)
                tidy_res <- broom::tidy(model, conf.int = TRUE)
                
                # Filter out the intercept and bind to results
                tidy_res <- tidy_res %>% filter(term != "(Intercept)")
                tidy_res$outcome <- outcome
                results[[outcome]] <- tidy_res
        }
        
        # Combine results, add a column indicating fill color
        df <- bind_rows(results)
        df$fill_color <- ifelse(df$p.value < 0.05, as.character(df$term), "grey")
        
        # Sort outcomes based on the magnitude of the coefficient for the first predictor
        sorting_vector <- df %>% 
                filter(term == predictors[1]) %>%
                arrange((abs(estimate))) %>%
                pull(outcome)
        
        # Create the plot
        plot <- ggplot(df, aes(x = estimate, y = factor(outcome, levels = sorting_vector), fill = fill_color, group = term)) +
                geom_col(aes(color = fill_color), position = position_dodge(width = 0.7), width = 0.6) +
                geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = fill_color), position = position_dodge(width = 0.7), height = 0.2) +
                scale_fill_manual(values = c("grey", setNames(palette("Set1")[1:length(predictors)], predictors))) +
                scale_color_manual(values = c("grey", setNames(palette("Set1")[1:length(predictors)], predictors))) +
                labs(x = "Regression Coefficient", y = "Outcome") +
                theme_minimal() +
                theme(
                        text = element_text(family = "Arial"),
                        legend.position = "top",
                        legend.title = element_blank(),
                        legend.text = element_text(size = 10),
                        axis.title = element_text(size = 12, face = "bold"),
                        axis.text = element_text(size = 10),
                        plot.title = element_text(size = 14, face = "bold")
                ) +
                guides(fill = guide_legend(title = "Predictor"), color = guide_legend(title = "Predictor"))
        
        return(plot)
}

# Example usage
data(mtcars)
plot <- figure_multi_regCoefs(mtcars, predictors = c("wt", "hp"), outcomes = c("mpg", "disp"))
print(plot)
