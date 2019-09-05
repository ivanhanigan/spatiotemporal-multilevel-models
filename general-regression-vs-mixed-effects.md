---
layout: default
title: General regression versus mixed-effects models
---

We will cover what is a Random Effect and how it differs from a Fixed effect. Some example syntax in R (on a secure website portal we have configured) and Stata will show how to get a handle on models that have random intercepts and additionally, random slopes.

We will spend a little time talking about how to partition variation and get estimates of the random and fixed effects. An important element will be our discussion of similarities between mixed-effects models with basic regression. There will also be brief discussion of extending the regression to non-gaussian responses (GLMERs).

## Suggested workflow (run these scripts in this order)

- Get the data file [simulated_data.csv](./2_general_regression_vs_mixed_effects/data/simulated_data.csv)
- [intercepts_and_slopes_in_general_regression.R](./2_general_regression_vs_mixed_effects/intercepts_and_slopes_in_general_regression.R)
- [intercepts_and_slopes_in_mixed_effects.R](./2_general_regression_vs_mixed_effects/intercepts_and_slopes_in_mixed_effects.R)
- [general_regression_vs_mixed_effects.R](./2_general_regression_vs_mixed_effects/general_regression_vs_mixed_effects.R)
- Check out [make_simulated_data.R](./2_general_regression_vs_mixed_effects/make_simulated_data.R) to see how the data were constructed
