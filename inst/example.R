library(simpr)
library(dplyr)
library(ggplot2)
library(brms)

## Regression example --------------

simpr_spec = variables(x1 = ~ 2 + rnorm(n),
                       x2 = ~ 3 + 2*x1 + rnorm(n, 0, sd = 0.5),
                       y = ~ 5 + b1*x1 + b2*x2 + g1*x1*x2 + rnorm(n, 0, sd = s)) %>%
  meta(n = seq(100, 300, by = 20),
       b1 = 1,
       b2 = 1,
       g1 = seq(-1, 1, by = 0.5),
       s = seq(0.2, 50, length.out = 6))


simpr_gen = simpr_spec %>%
  gen(20) %>%
  fit(lm = ~lm(y ~ x1*x2, data = .))

simpr_calc = simpr_gen %>%
  tidy_all

simpr_calc %>%
  filter(term %in% "x1:x2",
         n == 100) %>%
  group_by(g1, s) %>%
  summarize(power = mean(p.value < 0.05)) %>%
  ggplot(aes(s, power)) +
  geom_line() +
  facet_wrap(~g1)

## Chisq example: number of categories, compare fisher and chisq ------------

chisq_spec = variables(x1 = ~rnorm(n),
                       x2 = ~x1 + rnorm(n, 0, sd = 2),
                       c1 = ~ cut(x1, breaks = b) %>% as.numeric,
                       c2 = ~ cut(x2, breaks = b) %>% as.numeric) %>%
  meta(n = seq(50, 200, by = 50),
       b = 2:10)

chisq_gen = chisq_spec %>%
  gen(20) %>%
  fit(ChiSq = ~chisq.test(.$c1, .$c2),
      Unknown_Continuous_Correlation = ~cor.test(.$x1, .$x2),
      Pearson_Correlation = ~cor.test(.$c1, .$c2))


all_tidy = chisq_gen %>%
  tidy_all

all_tidy %>%
  group_by(n, b, Source) %>%
  summarize(power = mean(p.value < 0.05)) %>%
  ggplot(aes(n, power, group = Source, linetype = Source, color = Source)) +
  geom_line() +
  facet_wrap(~b)


# Independent t-test example ----------------------------------------------

ind_t_spec = variables(y1 = ~ rnorm(n, mean = m + d*s, sd = s),
                       y2 = ~ rnorm(n, mean = m, sd = s)) %>%
  meta(n = seq(20, 100, by = 10), # n per grp
       m = 70, # ctrl grp mean
       d = seq(0, 1, by = .2), # exp - ctrl / sd (cohen's d)
       s = 10) # sd (both grps)

ind_t_gen = ind_t_spec %>%
  gen(100) %>%
  fit(ind_t_test = ~t.test(.$y1, .$y2, paired = FALSE, alternative = "two.sided"))
# note the above usage of .$colname notation is equivalent to providing data=.

ind_t_tidy = ind_t_gen %>%
  tidy_all()

# plot the power curves
ind_t_tidy %>%
  group_by(n, d) %>%
  filter(d > 0) %>% # only cases where null is false
  summarize(power = mean(p.value < 0.05)) %>%
  ggplot(aes(x = n, y = power,
                               group = factor(d), color = factor(d))) +
  geom_line() +
  scale_color_discrete() +
  labs(x = "n per group", group = "Cohen's d", color = "Cohen's d",
                y = "Power", title = "Power curves for independent t-test")

# plot the power curves another way
ind_t_tidy %>%
  group_by(n, d) %>%
  filter(d > 0) %>% # only cases where null is false
  summarize(power = mean(p.value < 0.05)) %>%
  ggplot(aes(x = d, y = power,
                               group = factor(n), color = factor(n))) +
  geom_line() +
  scale_color_discrete() +
  labs(x = "Cohen's d", group = "n per group", color = "n per group",
                y = "Power", title = "Power curves for independent t-test")

# plot p value distribution when null is false (i.e., d = 0)
ind_t_tidy %>%
  group_by(n) %>%
  filter(d == 0) %>% # only cases where null is true
  ggplot(aes(x = p.value)) +
  geom_histogram(breaks = seq(0, 1, by = .05),
                          color = "black", fill = "#1b9e77") +
  geom_vline(xintercept = .05, color = "#d95f02") +
  labs(x = "p value", y = "Frequency",
                title = "Distribution of p-values under the null (d=0)")



# Dependent t-test example ----------------------------------------------

dep_t_spec = variables(y1 = ~ rnorm(n, mean = m, sd = s),
                       y2 = ~ y1 + rnorm(n, mean = d*s, sd = s)) %>%
  meta(n = seq(20, 100, by = 10), # overall n (2n observations)
       m = 70, # y1 mean
       d = seq(0, 1, by = .2), # exp - ctrl / sd (cohen's d)
       s = 10) # sd (both vars)

dep_t_gen = dep_t_spec %>%
  gen(100) %>%
  fit(dep_t_test = ~t.test(.$y1, .$y2, paired = TRUE, alternative = "two.sided"))

dep_t_tidy = dep_t_gen %>%
  tidy_all()

# plot the power curves
dep_t_tidy %>%
  filter(d > 0) %>% # only cases where null is false
  group_by(n, d) %>%
  summarize(power = mean(p.value < 0.05)) %>%
  ggplot(aes(x = n, y = power,
                               group = factor(d), color = factor(d))) +
  geom_line() +
  scale_color_discrete() +
  labs(x = "n per group", group = "Cohen's d", color = "Cohen's d",
       y = "Power", title = "Power curves for Dependent t-test")

# plot the power curves another way
dep_t_tidy %>%
  group_by(n, d) %>%
  filter(d > 0) %>% # only cases where null is false
  summarize(power = mean(p.value < 0.05)) %>%
  ggplot(aes(x = d, y = power,
                               group = factor(n), color = factor(n))) +
  geom_line() +
  scale_color_discrete() +
  labs(x = "Cohen's d", group = "n per group", color = "n per group",
       y = "Power", title = "Power curves for Dependent t-test")

# plot p value distribution when null is false (i.e., d = 0)
dep_t_tidy %>%
  group_by(n) %>%
  filter(d == 0) %>% # only cases where null is true
  ggplot(aes(x = p.value)) +
  geom_histogram(breaks = seq(0, 1, by = .05),
                          color = "black", fill = "#1b9e77") +
  geom_vline(xintercept = .05, color = "#d95f02") +
  labs(x = "p value", y = "Frequency",
                title = "Distribution of p-values under the null (d=0)")



# Power comparison: independent vs. dependent t-test ---------------------

# Let's simulate data to be dependent,
# and then compare dependent vs. independent t-tests on the same data,
# to simulate power of dependent designs vs. independent (where applicable)
t_comp_spec = variables(y1 = ~ rnorm(n, mean = m, sd = s),
                   y2 = ~ y1 + rnorm(n, mean = d*s, sd = s)) %>%
  meta(n = seq(20, 100, by = 10), # overall n (2n observations)
       m = 70, # y1 mean
       d = seq(0, 1, by = .2), # exp - ctrl / sd (cohen's d)
       s = 10) # sd (both vars)

# generate the data (100 replications)
t_comp_gen = t_comp_spec %>%
  gen(100)

# fit generated data using an INDEPENDENT model (incorrectly specified model)
#   as well as a DEPENDENT model(correctly specified model)
t_comp_fit = t_comp_gen %>%
  fit(ind_t_test = ~t.test(.$y1, .$y2, paired = FALSE, alternative = "two.sided"),
      dep_t_test = ~t.test(.$y1, .$y2, paired = TRUE, alternative = "two.sided"))
# tidy
t_comp_tidy = t_comp_fit %>%
  tidy_all()

# plot the power curves against each other (FACET by METHOD)
t_comp_tidy %>%
  filter(d > 0) %>% # only cases where null is false
  group_by(n, d, method) %>%
  summarize(power = mean(p.value < 0.05)) %>%
  ggplot(aes(x = n, y = power,
             group = method, color = method)) +
  geom_line() +
  facet_wrap(vars(d)) +
  scale_color_discrete() +
  labs(x = "n per group", group = "Cohen's d", color = "Cohen's d",
       y = "Power", title = "Power curves for independent vs. dependent t-test")


# Binomial example --------------------------------------------------------

# specify a binomial data-generating process
binom_spec = variables(s = ~ rbinom(1, size = n, prob = p), # successes
                       f = ~ n - s) %>% # failures
  meta(n = seq(100, 200, by = 10), # number of trials
       p = seq(.5, .75, by = .05)) # probability of success (1)

# generate the data (100 replications)
binom_gen = binom_spec %>%
  gen(100)

# fit the data using the binomial test (null = .5)
binom_fit = binom_gen %>%
  fit(bin_test = ~ binom.test(c(.$s, .$f), # (vector of #success and #failures)
                              p = 0.5, # null hypothesis
                              alternative = "two.sided"))
# tidy
binom_tidy = binom_fit %>%
  tidy_all()

# plot the power curves
binom_tidy %>%
  group_by(n, p) %>%
  filter(p > 0.5) %>% #
  summarize(power = mean(p.value < 0.05)) %>%
  ggplot(aes(x = n, y = power,
             group = p, color = p)) +
  geom_line() +
  labs(x = "Number of trials (n)", group = "Prob. of Success", color = "Prob. of Success",
       y = "Power", title = "Power curves for Binomial test")

# plot p value distribution when null is false (i.e., p = 0.5)
binom_tidy %>%
  group_by(n) %>%
  filter(p == 0.5) %>% # only cases where null is true
  ggplot(aes(x = p.value)) +
  #geom_st() +
  # geom_histogram(breaks = seq(0, 1, by = .05),
  #                color = "black", fill = "#1b9e77") +
  geom_vline(xintercept = .05, color = "#d95f02") +
  labs(x = "p value", y = "Frequency",
       title = "Distribution of p-values under the null (p=0.5)")


binom_tidy %>%
  group_by(n) %>%
  filter(p == 0.5) %>%
  pull(p.value) %>%
  stem()



# Reaction time power analysis --------------------------------------------

# specify a shifted log-normal data-generating process for 2 conditions (within)
rt_spec = variables(rt1 = ~ rshifted_lnorm(n, # sample size
                                           meanlog = mu, # difficulty (mean)
                                           sdlog = sigma, # scale (sd)
                                           shift = sh), # earliest possible RT
                    rt2 = ~ rshifted_lnorm(n, # sample size
                                           meanlog = mu + mu_diff, # difficulty (mean)
                                           sdlog = sigma + sig_diff, # scale (sd)
                                           shift = sh) # earliest possible RT
                       ) %>%
  meta(n = seq(10, 100, by = 10), # number of trials
       mu = c(-.5, 1, 1.5), # note: exp(mu) + shift is the median RT
       sigma = seq(.4, .8, 1.2),
       sh = .2, # earliest possible RT (200 ms), shifts log-normal rightward
       mu_diff = c(.1, .3, .5),
       sig_diff = c(.2, .4)
       )

# generate the data (100 replications)
rt_gen = rt_spec %>%
  gen(100)

