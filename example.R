#source("functions.R")

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
  calc_tidy

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
  calc_tidy

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

ind_t_tidy = ind_t_gen %>%
  calc_tidy()

# plot the power curves
ind_t_tidy %>%
  dplyr::group_by(n, d) %>%
  dplyr::filter(d > 0) %>% # only cases where null is false
  dplyr::summarize(power = mean(p.value < 0.05)) %>%
  ggplot2::ggplot(ggplot2::aes(x = n, y = power,
                               group = factor(d), color = factor(d))) +
  ggplot2::geom_line() +
  ggplot2::scale_color_discrete() +
  ggplot2::labs(x = "n per group", group = "Cohen's d", color = "Cohen's d",
                y = "Power", title = "Power curves for independent t-test")

# plot the power curves another way
ind_t_tidy %>%
  dplyr::group_by(n, d) %>%
  dplyr::filter(d > 0) %>% # only cases where null is false
  dplyr::summarize(power = mean(p.value < 0.05)) %>%
  ggplot2::ggplot(ggplot2::aes(x = d, y = power,
                               group = factor(n), color = factor(n))) +
  ggplot2::geom_line() +
  ggplot2::scale_color_discrete() +
  ggplot2::labs(x = "Cohen's d", group = "n per group", color = "n per group",
                y = "Power", title = "Power curves for independent t-test")

# plot p value distribution when null is fasle (i.e., d = 0)
ind_t_tidy %>%
  dplyr::group_by(n) %>%
  dplyr::filter(d == 0) %>% # only cases where null is true
  ggplot2::ggplot(ggplot2::aes(x = p.value)) +
  ggplot2::geom_histogram(breaks = seq(0, 1, by = .05),
                          color = "black", fill = "#1b9e77") +
  ggplot2::geom_vline(xintercept = .05, color = "#d95f02") +
  ggplot2::labs(x = "p value", y = "Frequency",
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
  calc_tidy()

# plot the power curves
dep_t_tidy %>%
  dplyr::filter(d > 0) %>% # only cases where null is false
  dplyr::group_by(n, d) %>%
  dplyr::summarize(power = mean(p.value < 0.05)) %>%
  ggplot2::ggplot(ggplot2::aes(x = n, y = power,
                               group = factor(d), color = factor(d))) +
  ggplot2::geom_line() +
  ggplot2::scale_color_discrete() +
  ggplot2::labs(x = "n per group", group = "Cohen's d", color = "Cohen's d",
                y = "Power", title = "Power curves for Dependent t-test")

# plot the power curves another way
dep_t_tidy %>%
  dplyr::group_by(n, d) %>%
  dplyr::filter(d > 0) %>% # only cases where null is false
  dplyr::summarize(power = mean(p.value < 0.05)) %>%
  ggplot2::ggplot(ggplot2::aes(x = d, y = power,
                               group = factor(n), color = factor(n))) +
  ggplot2::geom_line() +
  ggplot2::scale_color_discrete() +
  ggplot2::labs(x = "Cohen's d", group = "n per group", color = "n per group",
                y = "Power", title = "Power curves for Dependent t-test")

# plot p value distribution when null is fasle (i.e., d = 0)
dep_t_tidy %>%
  dplyr::group_by(n) %>%
  dplyr::filter(d == 0) %>% # only cases where null is true
  ggplot2::ggplot(ggplot2::aes(x = p.value)) +
  ggplot2::geom_histogram(breaks = seq(0, 1, by = .05),
                          color = "black", fill = "#1b9e77") +
  ggplot2::geom_vline(xintercept = .05, color = "#d95f02") +
  ggplot2::labs(x = "p value", y = "Frequency",
                title = "Distribution of p-values under the null (d=0)")
