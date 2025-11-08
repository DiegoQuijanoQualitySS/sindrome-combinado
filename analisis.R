library(tidyverse)
load("rdata/base_sc.rda")
options(tibble.width = Inf)
base_sc

## Descriptivos demograficos
## Sexo
table(base_sc$sexo,base_sc$grupo)
chisq.test(table(base_sc$sexo,base_sc$grupo))
## Edad
shapiro.test(base_sc$edad)
base_sc|> 
  filter(!is.na(edad))|>
  group_by(grupo)|>
  summarise(mediana = median(edad), rango_intercuartil = IQR(edad))
grupo_sxc <- base_sc|> filter(grupo == "SxC")
grupo_ca <- base_sc|> filter(!grupo == "SxC")
wilcox.test(grupo_sxc$edad,grupo_ca$edad)
## Tabaquismo
table(base_sc$tabaquismo,base_sc$grupo)
chisq.test(table(base_sc$tabaquismo,base_sc$grupo))
## Indice tabaquico
shapiro.test(base_sc$indice_tabaquico)
base_sc|> 
  filter(!indice_tabaquico == 0)|>
  group_by(grupo)|>
  summarise(mediana = median(indice_tabaquico), rango_intercuartil = IQR(indice_tabaquico), minimo = min(indice_tabaquico),maximo = max(indice_tabaquico))
sxc_fumadores <- grupo_sxc|> filter(!indice_tabaquico == 0)
ca_fumadores <- grupo_ca|> filter(!indice_tabaquico == 0)
wilcox.test(sxc_fumadores$indice_tabaquico,ca_fumadores$indice_tabaquico)

##Exposicion a humo de leña
table(base_sc$uso_de_leña,base_sc$grupo)
chisq.test(table(base_sc$uso_de_leña,base_sc$grupo))
##Indice de exposicion a humo de leña
shapiro.test(base_sc$indice_tabaquico)
base_sc|> 
  filter(!iehl == 0)|>
  group_by(grupo)|>
  summarise(mediana = median(iehl), rango_intercuartil = IQR(iehl), minimo = min(iehl),maximo = max(iehl))
sxc_leña <- grupo_sxc|> filter(!iehl == 0)
ca_leña <- grupo_ca|> filter(!iehl == 0)
wilcox.test(sxc_leña$iehl,ca_leña$iehl)

## Exposicion ocupacional
table(base_sc$exposicion_ocupacional,base_sc$grupo)
chisq.test(table(base_sc$exposicion_ocupacional,base_sc$grupo))
## Frecuencias de exposicion ocupacional
table(base_sc$producto_exposicion_ocupacional,base_sc$grupo)
fisher.test(base_sc$producto_exposicion_ocupacional,base_sc$grupo)

## Saturacion de oxigeno
shapiro.test(base_sc$saturacion_oxigeno)
base_sc|>filter(!is.na(saturacion_oxigeno) & !saturacion_oxigeno == 0)|>
  group_by(grupo)|>
  summarise(mediana = median(saturacion_oxigeno), rango_intercuartil = IQR(saturacion_oxigeno), minimo = min(saturacion_oxigeno),maximo = max(saturacion_oxigeno))
wilcox.test(grupo_sxc$saturacion_oxigeno,grupo_ca$saturacion_oxigeno)

## Genetica
##drb_alelo1
base_sc|>group_by(drb_alelo1,grupo)|>
  summarise(n())|>
  print(n=Inf)
## drb_alelo2
base_sc|>group_by(drb_alelo2,grupo)|>
  summarise(n())|>
  print(n=Inf)
## dqb1_ alelo 1
base_sc|>group_by(dqb1_alelo1,grupo)|>
  summarise(n())|>
  print(n=Inf)
## dqb1_alelo2
base_sc|>group_by(dqb1_alelo2,grupo)|>
  summarise(n())|>
  print(n=Inf)

## Analisis exploratorio grafico

## Indice tabaquico/ indice humo de leña / grupo
base_sc|>ggplot(aes(iehl,indice_tabaquico, color = producto_exposicion_ocupacional))+
  geom_point()+
  scale_x_continuous(transform = "log10")+
  scale_y_continuous(transform = "log10")+
  facet_grid(.~grupo)

## no fumadores / producto de exposicion
base_sc|> filter(tabaquismo == "negado")|>
  ggplot(aes(producto_exposicion_ocupacional,iehl,color = grupo, label=clave_muestra))+
  geom_point()+
  scale_y_continuous(transform = "log10")+
  facet_grid(.~contacto_aves)

base_sc|>ggplot(aes(grupo))+
  geom_bar()+
  facet_grid(tabaquismo~uso_de_leña)

base_sc|>filter(tabaquismo %in% c("fumador","exfumador"))|>
  ggplot(aes(grupo))+
  geom_bar()

base_sc|>filter(tipo_histologico %in% c(0,1))|>
  ggplot(aes(indice_tabaquico))+
  geom_histogram()+
  facet_grid(tipo_histologico~.)
table(base_sc$tipo_histologico,base_sc$indice_tabaquico)