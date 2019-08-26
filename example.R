source("functions.R")

simpr_spec = variables(x1 = ~ 2 + rnorm(n),
                       x2 = ~ 3 + 2*x1 + rnorm(n, 0, sd = 0.5),
                       y = ~ 5 + b1*x1 + b2*x2 + g1*x1*x2 + rnorm(n, 0, sd = s)) %>% 
  meta(n = seq(100, 300, by = 20),
       b1 = seq(-1, 1, by = 0.5),
       b2 = seq(-1, 1, by = 0.5),
       g1 = seq(-1, 1, by = 0.5),
       s = seq(0.2, 3, length.out = 5))


simpr_gen = simpr_spec %>% 
  gen(20)
  

simpr_mod = simpr_gen %>% 
  fit(lm, y ~ x1*x2) 
  


simpr_calc = simpr_mod %>% 
  calc_tidy


simpr_calc %>% 
  filter(term %in% "x1") %>% 
  group_by(.n, .sd1) %>% 
  summarize(power = mean(p.value < 0.05)) %>% 
  ggplot(aes(.sd1, power)) +
  geom_line()

replace_function = function(x, args) {
 substitute(x, env = args)
}

args_char = args %>% 
  as.character %>% 
  set_names(names(args))
eval_fn = x %>% deparse %>% 
  str_replace_all(args_char)
replace_function(n, args)


a <- 2
bquote(a == a)
quote(a == a)
bquote(a == .(a))
