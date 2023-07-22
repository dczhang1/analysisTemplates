fun_bullet <- function(factor_name, q1, q2, q3, score){
            
            fig <- plot_ly(
                        type = "indicator",
                        mode = "number+gauge+delta",
                        value = score,
                        domain = list(x = c(0.15, 1), y= c(0, 1)),
                        title = list(text = factor_name),
                        delta = list(reference = q3),
                        gauge = list(
                                    shape = "bullet",
                                    axis = list(range = list(NULL, 100)),
                                    threshold = list(
                                                line = list(color = "green", width = 2),
                                                thickness = 0.75,
                                                value = q3),
                                    steps = list(
                                                list(range = c(0, q1), color = "gray"),
                                                list(range = c(q1, q2), color = "lightgray")),
                                    bar = list(color = "darkblue")),
                                    height = 115, width = 600) 
            
            fig <- fig %>%
                        layout(margin = list(l= 100, r= 10))
            
            fig
}

fun_bullet("Work Adversity",70,75,80,57)
fun_bullet("Job Demands",52, 74, 94, 58)
fun_bullet("Leader <br> Engagement",60, 74, 90, 72)
fun_bullet("Organizational <br> Safety", 60, 75, 80, 64)
fun_bullet("Organizational <br> Accountability", 60, 75, 80, 52)
fun_bullet("Organizational <br> Integrity", 60, 70, 87, 58)
