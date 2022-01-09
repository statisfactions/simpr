library(simpr)
library(lavaan)
library(glue)

sim_results = specify(out = ~ simulateData(glue("
   Y ~ {x_y}*X + {m_y}*M
   M ~ {x_m}*X

   Y =~ {y_i}*Y_1 + {y_i}*Y_2 + {y_i}*Y_3
   X =~ {pred_i}*X_1 + {pred_i}*X_2 + {pred_i}*X_3 + {pred_i}*X_4 + {pred_i}*X_5
   M =~ {pred_i}*M_1 + {pred_i}*M_2 + {pred_i}*M_3 + {pred_i}*M_4 + {pred_i}*M_5
"), sample.nobs = n)) %>%
  define(x_y = 0.5,
         m_y = seq(-1, 1, by = .5),
         x_m = seq(-1, 1, by = .5),
         y_i = c(.2, .8),
         pred_i = .8,
         n = seq(200)) %>%
  generate(1) %>%
  fit(sem_fit = ~ sem("
    Y ~ a*X + b*M
    M ~ c*X

    Y =~ Y_1 + Y_2 + Y_3
    X =~ X_1 + X_2 + X_3 + X_4 + X_5
    M =~ M_1 + M_2 + M_3 + M_4 + M_5

    ie := b*c # indirect effect
", data = .)) %>%
  tidy_fits
