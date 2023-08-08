figure_regcoef <- function(formula, data, plot_title = "Regression Coefficients") {
            library(ggplot2)
            
            # Fit the model
            model <- lm(formula, data)
            
            # Get coefficients and confidence intervals
            coefficients <- coef(model)
            confidence_intervals <- confint(model)
            
            # Extract p-values
            p_values <- summary(model)$coefficients[, 4]
            
            # Determine significance based on p-values
            significant <- p_values < 0.05
            
            # Determine fill color for bars
            bar_color <- ifelse(significant & coefficients > 0, "cyan3", 
                                ifelse(significant & coefficients < 0, "pink3", "grey"))
            
            # Transform data for plotting
            plot_data <- data.frame(
                        predictor = names(coefficients),
                        coef = coefficients,
                        lower = confidence_intervals[, 1],
                        upper = confidence_intervals[, 2],
                        bar_color = bar_color
            )
            
            # Exclude the intercept
            plot_data <- plot_data[plot_data$predictor != "(Intercept)", ]
            
            # Plot
            ggplot(plot_data, aes(x = predictor, y = coef)) +
                        geom_col(aes(fill = bar_color), width = 0.6) +
                        geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "#555555") +
                        coord_flip() +
                        scale_fill_identity() +  # Use the provided colors directly
                        labs(title = plot_title,   # Use the plot_title argument here
                             x = "Predictors",
                             y = "Coefficient Value") +
                        theme_minimal(base_family = "sans") +
                        theme(
                                    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                                    axis.title = element_text(size = 14, face = "bold"),
                                    axis.text = element_text(size = 12),
                                    legend.position = "none",
                                    panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(),
                                    panel.border = element_rect(colour = "black", fill = NA, size = 1)
                        )
}

#Demonstration
#data(mtcars)
#figure_regcoef('mpg ~ cyl + disp + hp + drat + wt', data = mtcars)
