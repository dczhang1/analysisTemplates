### Plots for factorial designs

### For plotting typical 2x2 plots. 
apa_factorial_plot(
        data = df
        #data = dplyr::rename(df, "Factor Name" = "factor_A") ### Use this to rename factor names for legend
        , id = "subjid"
        , dv = "dv"
        , factors = c("factor1", "factor2")
        , las = 1
        , plot = c("error_bars", "points")
        , ylim = c(1, 5)
        , xlab = "Condition"
        , ylab = "dependent var"
        , na.rm = T
)
```