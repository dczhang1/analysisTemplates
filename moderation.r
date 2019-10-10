##Cort's R Utility for Interactively Plotting Continuous*Continuous Two-Way Interactions"

Moderation <- function(IV, MOD) {return(5.52 - 0.55 * IV - 0.8 * MOD + 0.35 * IV * MOD)} ##Specify Formula Here##

IV="Independent Variable" ## Name IV
DV="Dependent Variable" ## Name DV
MOD="Moderator" ## Name Mod

xbar.IV <- 3.83  ##Mean of IV
s.IV <- 1.37  ##Standard Deviation of IV
xbar.MOD <- 2.56 ##Mean of Moderator
s.MOD <- 0.88 ##Standard Deviation of Moderator


##Specify Range of IV & DV
curve(Moderation(x, xbar.MOD), xlim = c(1, 7), ylim = c(1, 7), xlab = IV, ylab = DV, col="green", lty = "dotted", lwd="1.5")

curve(Moderation(x, xbar.MOD + s.MOD), col="red", lty = "dashed", lwd="1.5", add = TRUE)

curve(Moderation(x, xbar.MOD - s.MOD), col="blue", lty = "dashed", lwd="1.5", add = TRUE)

legend(1,2.5, c("High (+1 S.D.) Moderator", "Mean Moderator", "Low (-1 S.D.) Moderator"), lty=c(2,3,2), lwd=c(1.5,1.5,1.5),col=c("red","green","blue"))
