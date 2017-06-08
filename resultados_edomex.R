library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

## abrir base de datos del prep, definir tipo de columnas
prep_edomex <- read_csv("MEX_GOB_2017/MEX_GOB_2017.csv", skip = 6, col_types = "iciciiciiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiccDDD")

## la base de datos trae columnas que identifican las casillas, y los resultados por partidos o coalición. 
## selecciono identificadores de casillas, resultados por partido o calición
##también selecciono el total de votos por casilla y el total de personas en la lista nominal
prep_relevante <- prep_edomex %>% 
  select(seccion,id_casilla, tipo_casilla,
         pan, pri, prd, pt, pvem, na, morena, pes, # para seleccionar coaliciones selecciono todo o que empieza que "c"
         starts_with("c"),  -contabilizada, total_votos, lista_nominal) %>%  #y quito la columna "contabilizada"
  group_by(seccion) %>% #agrupo los resultados por sección electoral
  summarise_at(vars(pri, pvem, na, pes, pan, prd, pt,  morena, starts_with("c"), total_votos, lista_nominal), sum, na.rm = TRUE) %>% #sumo los resultads de casillas por seccion
  mutate(adm = rowSums(.[,c(2:5, 10:20)], na.rm = TRUE)) %>% # sumo los votos por partidos en coalición y por coalición de Del Mazo
  select(seccion, jvm = pan, adm,  jzh = prd, ogy = pt, dga = morena, tco = cand_ind_1, total_votos, lista_nominal) # cambio nombres a iniciales de candidatos

## modificar el marco para hacer operaciones más rápido
resultados <- prep_relevante %>% 
  gather(candidato, votos, jvm:tco) %>% # paso el marco de horizontal a vertical
  group_by(candidato) %>% 
  summarise(votos = sum(votos), total_votos = sum(total_votos)) %>% 
  mutate(resultados = votos/total_votos*100) %>% # calcular porcentaje de votos
  arrange(desc(resultados)) %>% 
  mutate(candidato = factor(candidato, candidato))

##gráfica con los resultados electorales
ggplot(resultados, aes(candidato, resultados)) + geom_bar(stat = "identity", fill = "#F8766D") +
  ggtitle("Elecciones EdoMex 2017: porcentaje de votación recibido por candidata/o.")
