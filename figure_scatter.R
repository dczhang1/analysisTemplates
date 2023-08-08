# This function creates a scatter plot between two variables and displays a histogram of the X variable on top. 

figure_scatter <- function(data, x_var, y_var, plot_color = "dodgerblue3", x_label, y_label, cor_position = "top left") {
            # Calculate correlation value
            cor_val <- round(cor(data[[x_var]], data[[y_var]], method = "pearson"), 2)
            
            # Calculate mean of x variable
            x_mean <- mean(data[[x_var]], na.rm = TRUE)
            
            # Define common x-axis limits
            xlim_values <- range(data[[x_var]])
            
            # Determine position of the correlation annotation based on cor_position
            if(cor_position == "top right") {
                        x_cor <- max(data[[x_var]]) * 0.95
                        y_cor <- max(data[[y_var]]) * 0.95
                        h_just <- 1
            } else if(cor_position == "top left") {
                        x_cor <- min(data[[x_var]]) * 1.05
                        y_cor <- max(data[[y_var]]) * 0.95
                        h_just <- 0
            } else if(cor_position == "bottom left") {
                        x_cor <- min(data[[x_var]]) * 1.05
                        y_cor <- min(data[[y_var]]) * 1.05
                        h_just <- 0
            } else { # bottom right
                        x_cor <- max(data[[x_var]]) * 0.95
                        y_cor <- min(data[[y_var]]) * 1.05
                        h_just <- 1
            }
            
            # Main scatter plot with regression line
            scatter_plot <- ggplot(data, aes_string(x=x_var, y=y_var)) + 
                        geom_point(color = plot_color) + 
                        geom_smooth(method = "lm", se = FALSE, color = "black", aes(group = 1)) + 
                        geom_label(aes(x = x_cor, y = y_cor, label = paste("r = ", cor_val)), 
                                   hjust = h_just, vjust = 0.5, fill = "grey80", alpha = 0.2) +
                        labs(x = x_label, y = y_label) +
                        theme_minimal() +
                        theme(
                                    axis.title = element_text(face = "bold", size = 12),
                                    axis.text = element_text(face = "bold", size = 10),
                                    panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(),
                                    panel.border = element_rect(colour = "black", fill = NA, size = 1.5)
                        ) +
                        xlim(xlim_values)
            
            # Histogram for x variable with vertical line for mean and text annotation
            histogram_plot <- ggplot(data, aes_string(x=x_var)) +
                        geom_histogram(fill = plot_color, color = "white", bins = 30) + 
                        geom_vline(aes(xintercept = x_mean), color = "black", linetype = "dashed") + 
                        annotate("text", x = Inf, y = Inf, 
                                 label = paste("M = ", round(x_mean, 2)), hjust = 1, vjust = 1) +
                        theme_minimal() +
                        theme(
                                    axis.title = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(),
                                    axis.line = element_blank()
                        ) +
                        xlim(xlim_values)
            
            # Arrange the plots
            grid.arrange(
                        histogram_plot,
                        scatter_plot,
                        ncol = 1, 
                        heights = c(1, 4)
            )
}
