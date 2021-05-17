tab_sex = table(cet$Sex)
tab_sex_df = as.data.frame(tab_sex)
colnames(tab_sex_df) = c("Frequencia", "Sex")

tab_sex_df_rel = tab_sex_df
tab_sex_df_rel[,2] = round(tab_sex_df_rel[,2]/nrow(cet),2)

bar_freq = ggplot(data=tab_sex_df , aes(x=Frequencia, y=Sex)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Sex), vjust=1.6, color="white", size=3.5)+
  labs(x = "", y = "Frequencia") +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))

bar_freq_rel = ggplot(data=tab_sex_df_rel , aes(x=Frequencia, y=Sex)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Sex), vjust=1.6, color="white", size=3.5)+
  labs(x = "", y = "Frequencia relativa") +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))

grid.arrange(bar_freq, bar_freq_rel, nrow = 1)

ar1 <- grid.arrange(bar_freq, bar_freq_rel, nrow = 1)
ggsave("figs/bar_freq_cet.png", ar1,
       width = 10,
       height = 5,
       units = c("in", "cm", "mm"),)

tb_sex_loc_wide = table(cet$Sex, cet$Location)

data_long = melt(tb_sex_loc_wide,
                 # The source columns
                 measure.vars=levels(factor(cet$Location)),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="Location",
                 value.name="Frequencia"
)
colnames(data_long) = c("Sex", "Location", "Frequência")

bar_sex_loc = ggplot(data=data_long , aes(x=Sex, y=Frequência, fill = Location)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  labs(x = "", y = "Frequência") +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))

bar_loc_sex = ggplot(data=data_long , aes(x=Location, y=Frequência, fill = Sex)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  labs(x = "", y = "Frequência") +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))



ar1 <- grid.arrange(bar_sex_loc, bar_loc_sex, nrow = 1)
ggsave("figs/bar_sex_loc_cet.png", ar1,
       width = 10,
       height = 4,
       units = c("in", "cm", "mm"),)

ht1 = ggplot(cet, aes(x=Age))+
  geom_histogram(color="darkblue", fill="lightblue", breaks=seq(0, 30, by = 10)) +
  ggtitle( "Intervalo de classe = 10 anos") +
  scale_x_continuous(name = "Age \n(years)") +
  scale_y_continuous(name = "Frequencia") +
  theme(plot.title = element_text(hjust = 0.5))

ht2 = ggplot(cet, aes(x=Age))+
  geom_histogram(color="darkblue", fill="lightblue", breaks=seq(0, 30, by = 5)) +
  ggtitle("Intervalo de classe = 5 anos") +
  scale_x_continuous(name = "Age \n(years)") +
  scale_y_continuous(name = "Frequencia") +
  theme(plot.title = element_text(hjust = 0.5))


ht3 = ggplot(cet, aes(x=Age))+
  geom_histogram(color="darkblue", fill="lightblue", breaks=seq(0, 30, by = 1)) +
  ggtitle( "Intervalo de classe = 1 ano") +
  scale_x_continuous(name = "Age \n(years)") +
  scale_y_continuous(name = "Frequencia") +
  theme(plot.title = element_text(hjust = 0.5))



ar1 <- grid.arrange(ht1, ht2, ht3, nrow = 1)
ggsave("figs/ht_cet.png", ar1,
       width = 15,
       height = 4,
       units = c("in", "cm", "mm"),)

volano_cant <- vol_ano %>%
  filter(Sistema == "Cantareira") %>%
  ggplot(aes(x=Ano, y = Volume_medio)) +
  geom_line() +
  geom_point() +
  ylab("Volume médio") +
  theme_bw()

ar1 <- grid.arrange(volano_cant, nrow = 1)
ggsave("figs/volano_cant.png", ar1,
       width = 8,
       height = 4,
       units = c("in", "cm", "mm"),)

volano_cantrio <- vol_ano %>%
  filter(Sistema == "Cantareira" | Sistema == "RioGrande") %>%
  ggplot(aes(x=Ano, y = Volume_medio, colour = Sistema)) +
  geom_line() +
  geom_point() +
  ylab("Volume médio") +
  theme_bw() +
  theme(legend.title=element_blank())

ar1 <- grid.arrange(volano_cantrio, nrow = 1)
ggsave("figs/volano_cantrio.png", ar1,
       width = 8,
       height = 4,
       units = c("in", "cm", "mm"),)

volano_all <- ggplot(vol_ano, aes(x=Ano, y = Volume_medio, colour = Sistema)) +
  geom_line() +
  geom_point() +
  ylab("Volume médio") +
  theme_bw() +
  theme(legend.title=element_blank())

ar1 <- grid.arrange(volano_all, nrow = 1)
ggsave("figs/volano_all.png", ar1,
       width = 8,
       height = 4,
       units = c("in", "cm", "mm"),)


gl1 = vol_ano %>%
  filter(Sistema == "Cantareira" | Sistema == "AltoTiete") %>%
  ggplot(aes(x=Ano, y = Volume_medio, colour = Sistema)) +
  geom_line() +
  geom_point() +
  ylab("Volume médio") +
  ylim(00,110) +
  theme_bw() +
  theme(legend.title=element_blank())

gl2 = vol_ano %>%
  filter(Sistema == "Cotia" | Sistema == "Guarapiranga") %>%
  ggplot(aes(x=Ano, y = Volume_medio, colour = Sistema)) +
  geom_line() +
  geom_point() +
  ylab("Volume médio") +
  ylim(00,110) +
  theme_bw() +
  theme(legend.title=element_blank())

gl3 = vol_ano %>%
  filter(Sistema == "RioClaro" | Sistema == "RioGrande") %>%
  ggplot(aes(x=Ano, y = Volume_medio, colour = Sistema)) +
  geom_line() +
  geom_point() +
  ylab("Volume médio") +
  ylim(00,110) +
  theme_bw() +
  theme(legend.title=element_blank())



ar1 <- grid.arrange(gl1, gl2, gl3, nrow = 3, ncol = 1)
ggsave("figs/volano_gl.png", ar1,
       width = 6,
       height = 12,
       units = c("in", "cm", "mm"),)


chuvol <- ggplot(vol_ano, aes(x=Chuva_media, y = Volume_medio)) +
  geom_point() +
  ylab("Volume médio") +
  xlab("Chuva média (mm)") +
  theme_bw()

ar1 <- grid.arrange(chuvol, nrow = 1, ncol = 1)
ggsave("figs/chuvol.png", ar1,
       width = 3.5,
       height = 3,
       units = c("in", "cm", "mm"),)

chuvol_sis <- ggplot(vol_ano, aes(x=Chuva_media, y = Volume_medio, color = Sistema)) +
  geom_point() +
  ylab("Volume médio") +
  xlab("Chuva média (mm)") +
  theme_bw()

ar1 <- grid.arrange(chuvol_sis, nrow = 1, ncol = 1)
ggsave("figs/chuvol_sis.png", ar1,
       width = 5,
       height = 3,
       units = c("in", "cm", "mm"),)


sc1 = vol_ano %>%
  filter(Sistema == "AltoTiete") %>%
  ggplot(aes(x=Chuva_media, y = Volume_medio)) +
  geom_point() +
  ylab("Volume médio") +
  xlab("Chuva média (mm)") +
  ggtitle("Sistema Alto Tiete") +
  theme_bw()

sc2 = vol_ano %>%
  filter(Sistema == "Cantareira") %>%
  ggplot(aes(x=Chuva_media, y = Volume_medio)) +
  geom_point() +
  ylab("Volume médio") +
  xlab("Chuva média (mm)") +
  ggtitle("Sistema Cantareira") +
  theme_bw()

sc3 = vol_ano %>%
  filter(Sistema == "Cotia") %>%
  ggplot(aes(x=Chuva_media, y = Volume_medio)) +
  geom_point() +
  ylab("Volume médio") +
  xlab("Chuva média (mm)") +
  ggtitle("Sistema Cotia") +
  theme_bw()

sc4 = vol_ano %>%
  filter(Sistema == "Guarapiranga") %>%
  ggplot(aes(x=Chuva_media, y = Volume_medio)) +
  geom_point() +
  ylab("Volume médio") +
  xlab("Chuva média (mm)") +
  ggtitle("Sistema Guarapiranga") +
  theme_bw()

sc5 = vol_ano %>%
  filter(Sistema == "RioGrande") %>%
  ggplot(aes(x=Chuva_media, y = Volume_medio)) +
  geom_point() +
  ylab("Volume médio") +
  xlab("Chuva média (mm)") +
  ggtitle("Sistema Rio Grande") +
  theme_bw()

sc6 = vol_ano %>%
  filter(Sistema == "RioClaro") %>%
  ggplot(aes(x=Chuva_media, y = Volume_medio)) +
  geom_point() +
  ylab("Volume médio") +
  xlab("Chuva média (mm)") +
  ggtitle("Sistema Rio Claro") +
  theme_bw()

grid.arrange(sc1, sc2, sc3, sc4, sc5, sc6, nrow = 3, ncol = 2)

ar1 <- grid.arrange(sc1, sc2, sc3, sc4, sc5, sc6, nrow = 3, ncol = 2)
ggsave("figs/chuvol_sis2.png", ar1,
       width = 5,
       height = 7,
       units = c("in", "cm", "mm"),)

sc1 = vol_ano %>%
  filter(Sistema == "AltoTiete") %>%
  ggplot(aes(x=Chuva_media, y = Volume_medio)) +
  geom_point() +
  ylab("Volume médio") +
  xlab("Chuva média (mm)") +
  ggtitle("Sistema Alto Tiete") +
  geom_smooth(method=lm, se=FALSE) +
  theme_bw()

sc2 = vol_ano %>%
  filter(Sistema == "Cantareira") %>%
  ggplot(aes(x=Chuva_media, y = Volume_medio)) +
  geom_point() +
  ylab("Volume médio") +
  xlab("Chuva média (mm)") +
  ggtitle("Sistema Cantareira") +
  geom_smooth(method=lm, se=FALSE) +
  theme_bw()

sc3 = vol_ano %>%
  filter(Sistema == "Cotia") %>%
  ggplot(aes(x=Chuva_media, y = Volume_medio)) +
  geom_point() +
  ylab("Volume médio") +
  xlab("Chuva média (mm)") +
  ggtitle("Sistema Cotia") +
  geom_smooth(method=lm, se=FALSE) +
  theme_bw()

sc4 = vol_ano %>%
  filter(Sistema == "Guarapiranga") %>%
  ggplot(aes(x=Chuva_media, y = Volume_medio)) +
  geom_point() +
  ylab("Volume médio") +
  xlab("Chuva média (mm)") +
  ggtitle("Sistema Guarapiranga") +
  geom_smooth(method=lm, se=FALSE) +
  theme_bw()

sc5 = vol_ano %>%
  filter(Sistema == "RioGrande") %>%
  ggplot(aes(x=Chuva_media, y = Volume_medio)) +
  geom_point() +
  ylab("Volume médio") +
  xlab("Chuva média (mm)") +
  ggtitle("Sistema Rio Grande") +
  geom_smooth(method=lm, se=FALSE) +
  theme_bw()

sc6 = vol_ano %>%
  filter(Sistema == "RioClaro") %>%
  ggplot(aes(x=Chuva_media, y = Volume_medio)) +
  geom_point() +
  ylab("Volume médio") +
  xlab("Chuva média (mm)") +
  ggtitle("Sistema Rio Claro") +
  geom_smooth(method=lm, se=FALSE) +
  theme_bw()



ar1 <- grid.arrange(sc1, sc2, sc3, sc4, sc5, sc6, nrow = 3, ncol = 2)
ggsave("figs/chuvol_sis3.png", ar1,
       width = 5,
       height = 7,
       units = c("in", "cm", "mm"),)


