# Define the numeric vector with censored values indicated by '*'
y <- c(2, 2, 21, 21, 4, 3, 9, 38, 5, 3, 3, 20, 6, 13, 3, 31, 6, 14, 5, 22, 17, 13, 10, 9, 9)

# Define the censoring indicator vector (1 for uncensored, 0 for censored)
cens <- c(1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1)

library(survival)

# Kaplan-Meier estimator
km_fit <- survfit(Surv(y, cens) ~ 1)
summary(km_fit)
# Nelson-Aalen estimator
na_fit1 <- survfit(Surv(y, cens) ~ 1, type = "fh",conf.type='log')  # Fleming-Harrington estimator (equivalent to NA)
summary(na_fit1)

na_fit2 <- survfit(Surv(y, cens) ~ 1, type = "fh",conf.type='plain')  # Fleming-Harrington estimator (equivalent to NA)
summary(na_fit2)


# R Code to Extract Internal Calculations

library(survival)

# Define the numeric vector and censoring
y <- c(2, 2, 21, 21, 4, 3, 9, 38, 5, 3, 3, 20, 6, 13, 3, 31, 6, 14, 5, 22, 17, 13, 10, 9, 9)
cens <- c(1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1)

# Kaplan-Meier or Nelson-Aalen
km_fit <- survfit(Surv(y, cens) ~ 1)

# View detailed results
summary(km_fit)

# Extract survival probabilities, standard errors, and variance
km_fit$surv       # Survival probabilities
km_fit$std.err    # Standard errors
km_fit$n.risk     # Number at risk at each time
km_fit$n.event    # Number of events at each time

# Calculate cumulative hazard and compare
cumhaz <- -log(km_fit$surv)
cumvar <- (km_fit$std.err / km_fit$surv)^2  # Based on variance relation
cbind(cumhaz, cumvar)

