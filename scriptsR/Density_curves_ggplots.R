library(tidyverse)
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

glnon <- 5
glden <- 50

params = list(df1 = glnon, df2 = glden)
ylim = c(0,10)
pF = 0.90
lim <- qf(pF, df1 = glnon, df2 = glden)
dfF <- data.frame(x = seq(0.1,4, l = 100)) %>% 
  mutate(df = stats::df(x, df1 = glnon, df2 = glden))
Fcurve = ggplot(data = dfF, mapping = aes(x = x)) +
  stat_function(fun = stats::df, args = list(df1 = glnon, df2 = glden)) +
  geom_area(stat = "function", fun = stats::df, color = 1,
            args = params,
            fill = '#d14143',
            xlim = c(lim, ylim[2])) +
  theme_classic(base_size = 15) +
  xlab('X') + #ylab('Densidade de probabilidade') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = 'black')) +
  scale_x_continuous(name = 'F',
                     limits = range(dfF$x), labels = NULL, breaks = NULL) +
  scale_y_continuous(name = 'Densidade de probabilidade',
                     limits = c(0,0.8), labels = NULL, breaks = NULL) +
  annotate(geom = 'segment', x = lim - 0.0, xend = lim - 0.0,
           y = 0.5, yend = 0.2, color = 'gray', size = 2,
           arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = 'text', x = lim - 0.0, y = 0.55, size = 8,
           color = 'gray', label = bquote("F"["calculado"]))

Fcurve
