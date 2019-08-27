source("functions.R")

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
  gen(20)
  

simpr_mod = simpr_gen %>% 
  fit(~lm(y ~ x1*x2, data = .))
  
simpr_calc = simpr_mod %>% 
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
                       c1 = ~ cut(x1, breaks = b),
                       c2 = ~ cut(x2, breaks = b)) %>% 
  meta(n = seq(50, 200, by = 50),
       b = 2:10)

chisq_gen = chisq_spec %>% 
  gen(50) 

## This is a bit clumsy here; would be easy to do these in succession if I kept
## everything in list-columns rather than going to long format, then each model
## object would be in its own column and they could all be chained and named; or
## called within a single fit() call. Then they could all be tidied at the same time.
chisq_mod = chisq_gen %>% 
  fit(~chisq.test(.$c1, .$c2))

## Something weird happened in model fitting for cor and fisher; we only have
## 50, somehow rep was grouped by

cor_mod = chisq_gen %>% 
  fit(~cor.test(.$x1, .$x2))

fisher_mod = chisq_gen %>% 
  fit(~fisher.test(.$c1, .$c2, simulate.p.value = TRUE))

all_mods = list(chisq_mod = chisq_mod,
                cor_mod = cor_mod,
                fisher_mod = fisher_mod)

all_tidy = map(all_mods, calc_tidy)



pvalues = map(all_tidy, ~ select(., n, b, p.value))

save.image("troubleshooting.Rdata")
