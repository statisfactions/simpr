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

