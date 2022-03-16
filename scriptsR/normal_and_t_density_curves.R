ggplot(data.frame(x = seq(-4,4, l = 100)), aes(x = x)) +
  stat_function(fun = dnorm, size = 3) +
  labs(x = 'X', y = 'Densidade de probabilidade',
       title = 'Distr. Normal Padronizada') +
  scale_x_continuous(breaks = -4:4) +
  theme_classic(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5, size = 30))
  
ggplot(data.frame(x = seq(-4,4, l = 100)), aes(x = x)) +
  stat_function(fun = dt, args = list(df = 3), size = 3) +
  labs(x = 'X', y = 'Densidade de probabilidade',
       title = 'Distr. t de Student') +
  scale_x_continuous(breaks = -4:4) +
  theme_classic(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5, size = 30))
