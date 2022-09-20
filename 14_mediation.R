# NOT RUN {
# Examples with JOBS II Field Experiment

# **For illustration purposes a small number of simulations are used**

data(jobs)

####################################################
# Example 1: Linear Outcome and Mediator Models
####################################################
b <- lm(job_seek ~ treat + econ_hard + sex + age, data=jobs)
c <- lm(depress2 ~ treat + job_seek + econ_hard + sex + age, data=jobs)

# Estimation via quasi-Bayesian approximation
contcont <- mediate(b, c, sims=50, treat="treat", mediator="job_seek")
summary(contcont)
plot(contcont)

# }
# NOT RUN {
# Estimation via nonparametric bootstrap
contcont.boot <- mediate(b, c, boot=TRUE, sims=50, treat="treat", mediator="job_seek")
summary(contcont.boot)
# }
# NOT RUN {
# Allowing treatment-mediator interaction
d <- lm(depress2 ~ treat + job_seek + treat:job_seek + econ_hard + sex + age, data=jobs)
contcont.int <- mediate(b, d, sims=50, treat="treat", mediator="job_seek")
summary(contcont.int)

# Allowing ``moderated mediation'' with respect to age
b.int <- lm(job_seek ~ treat*age + econ_hard + sex, data=jobs)
d.int <- lm(depress2 ~ treat*job_seek*age + econ_hard + sex, data=jobs)
contcont.age20 <- mediate(b.int, d.int, sims=50, treat="treat", mediator="job_seek",
                          covariates = list(age = 20))
contcont.age70 <- mediate(b.int, d.int, sims=50, treat="treat", mediator="job_seek",
                          covariates = list(age = 70))
summary(contcont.age20)
summary(contcont.age70)

# Continuous treatment
jobs$treat_cont <- jobs$treat + rnorm(nrow(jobs))  # (hypothetical) continuous treatment
b.contT <- lm(job_seek ~ treat_cont + econ_hard + sex + age, data=jobs)
c.contT <- lm(depress2 ~ treat_cont + job_seek + econ_hard + sex + age, data=jobs)
contcont.cont <- mediate(b.contT, c.contT, sims=50, 
                         treat="treat_cont", mediator="job_seek",
                         treat.value = 4, control.value = -2)
summary(contcont.cont)

# Categorical treatment 
# }
# NOT RUN {
b <- lm(job_seek ~ educ + sex, data=jobs)
c <- lm(depress2 ~ educ + job_seek + sex, data=jobs)

# compare two categories of educ --- gradwk and somcol
model.cat <- mediate(b, c, treat="educ", mediator="job_seek", sims=50, 
                     control.value = "gradwk", treat.value = "somcol")
summary(model.cat)
# }
# NOT RUN {
######################################################
# Example 2: Binary Outcome and Ordered Mediator
######################################################
# }
# NOT RUN {
jobs$job_disc <- as.factor(jobs$job_disc)
b.ord <- polr(job_disc ~ treat + econ_hard + sex + age, data=jobs,
              method="probit", Hess=TRUE)
d.bin <- glm(work1 ~ treat + job_disc + econ_hard + sex + age, data=jobs,
             family=binomial(link="probit"))
ordbin <- mediate(b.ord, d.bin, sims=50, treat="treat", mediator="job_disc")
summary(ordbin)

# Using heteroskedasticity-consistent standard errors
ordbin.rb <- mediate(b.ord, d.bin, sims=50, treat="treat", mediator="job_disc",
                     robustSE=TRUE)
summary(ordbin.rb)

# Using non-parametric bootstrap
ordbin.boot <- mediate(b.ord, d.bin, sims=50, treat="treat", mediator="job_disc",
                       boot=TRUE)
summary(ordbin.boot)
# }
# NOT RUN {
######################################################
# Example 3: Quantile Causal Mediation Effect
######################################################
require(quantreg)
c.quan <- rq(depress2 ~ treat + job_seek + econ_hard + sex + age, data=jobs,
             tau = 0.5)  # median
contquan <- mediate(b, c.quan, sims=50, treat="treat", mediator="job_seek")
summary(contquan)

######################################################
# Example 4: GAM Outcome
######################################################
# }
# NOT RUN {
require(mgcv)
c.gam <- gam(depress2 ~ treat + s(job_seek, bs="cr") + 
                     econ_hard + sex + age, data=jobs)
contgam <- mediate(b, c.gam, sims=10, treat="treat", 
                   mediator="job_seek", boot=TRUE)
summary(contgam)

# With interaction
d.gam <- gam(depress2 ~ treat + s(job_seek, by = treat) + 
                     s(job_seek, by = control) + econ_hard + sex + age, data=jobs)
contgam.int <- mediate(b, d.gam, sims=10, treat="treat", mediator="job_seek",
                       control = "control", boot=TRUE)
summary(contgam.int)
# }
# NOT RUN {
######################################################
# Example 5: Multilevel Outcome and Mediator Models
######################################################
# }
# NOT RUN {
require(lme4)

# educ: mediator group
# occp: outcome group

# Varying intercept for mediator 
model.m <- glmer(job_dich ~ treat + econ_hard + (1 | educ), 
                 family = binomial(link = "probit"), data = jobs)

# Varying intercept and slope for outcome
model.y <- glmer(work1 ~ treat + job_dich + econ_hard + (1 + treat | occp), 
                 family = binomial(link = "probit"), data = jobs)

# Output based on mediator group ("educ")
multilevel <- mediate(model.m, model.y, treat = "treat", 
                      mediator = "job_dich", sims=50, group.out="educ")

# Output based on outcome group ("occp")
# multilevel <- mediate(model.m, model.y, treat = "treat", 
mediator = "job_dich", sims=50) 

# Group-average effects  
summary(multilevel)

# Group-specific effects organized by effect
summary(multilevel, output="byeffect")
# plot(multilevel, group.plots=TRUE)
# See summary.mediate.mer and plot.mediate.mer for detailed explanations 

# Group-specific effects organized by group
summary(multilevel, output="bygroup")
# See summary.mediate.mer for detailed explanations 
# }