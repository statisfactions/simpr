source("functions.R")

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
