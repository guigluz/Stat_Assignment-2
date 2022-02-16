library(tidyverse)

dados = read_csv2("seguranca_por_delegacia.csv", 
                  locale = locale(encoding = "latin1"))

df1 = dados %>%
  group_by(Regiao, ano) %>%
  summarise(hom_doloso_reg = sum(hom_doloso))
df1 %>%
  ungroup() %>%
  #mutate(ano.mes = paste(ano, mes, sep = "." )) %>%
  #filter(Regiao == "Capital") %>%
  ggplot(aes(x = ano, y = hom_doloso_reg, color = Regiao))+
  geom_point()+
  geom_line()+
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", 
                               "#984EA3"))+
  theme_bw()+
  labs(x= "Ano" , y = "Homicídios dolosos" , color = "Região")+
  ggtitle("Homicídios dolosos por região do RJ")
ggsave("Homicidios_por_regiao.png")

df2 = dados %>%
  filter(ano == 2020)%>%
  select(-c(1:8)) %>%
  select(Regiao, hom_doloso, lesao_corp_morte, latrocinio, 
         hom_por_interv_policial)%>%
  gather( key = crime, value = freq, 
          hom_doloso, lesao_corp_morte, 
          latrocinio, hom_por_interv_policial)%>%
  group_by(Regiao, crime) %>%
  summarise(freq = sum(freq))

df2 %>%
  ggplot(aes(x=Regiao,y=freq, fill = crime))+
  geom_bar(stat = 'identity', position = "dodge")+
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", 
                               "#984EA3"), 
                    labels = c("Homicídio doloso", 
                               "Homicídio por intervenção policial",
                               "Latrocínio",
                               "Lesão Corporal Seguida de Morte") )+
  theme_classic()+
  theme(legend.position = "bottom",
        legend.box.background = element_rect(),
        legend.title = element_text(face = "bold"))+
  guides(fill = guide_legend(nrow = 2))+
  labs(x= "Região" , y = "Frequência" , fill = "Legenda")+
  ggtitle("Letalidade violenta no Rio de Janeiro (2020)")
ggsave("Letalidade_violenta.png")
