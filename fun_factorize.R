factorize <- function(x) {
        case_when(x %in% c("Strongly disagree") ~ 1,
                  x %in% c("Disagree") ~ 2,
                  x %in% c("Neutral") ~ 3,
                  x %in% c("Agree") ~ 4,
                  x %in% c("Strongly agree") ~ 5)
}