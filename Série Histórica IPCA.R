# Exercício de automação de coleta do IPCA gerando base de dados em planilha e gráfico para apresentação

# pacotes usados para a rotina
library(sidrar)
library(dplyr)
library(tidyverse)
library(writexl)

# criando objeto para automação da coleta do número índice do IPCA
ipca_indice <- get_sidra(api = '/t/1737/n1/all/v/2266/p/all/d/v2266%2013') %>%
  mutate(Data = parse_date(`Mês (Código)`, format = '%Y%m')) %>%
  dplyr::select(Data, Valor) %>% #selecionando variáveis necessárias para o gráfico
  filter(Data >= '2002-01-01') %>%
  as_tibble()

# criando objeto para automação da coleta da variação mensal do IPCA
ipca_vm <- get_sidra(api = '/t/1737/n1/all/v/63/p/all/d/v63%202') %>%
  mutate(Data = parse_date(`Mês (Código)`, format = '%Y%m')) %>%
  dplyr::select(Data, Valor) %>% #selecionando variáveis necessárias para o gráfico
  filter(Data >= '2002-01-01') %>% #filtrando data a partir de 2002 para não pegar a alta variação dos anos 80-90
  as_tibble()

# criando e formatando o gráfico para a série histórica do Número Índice
ipca_indice %>%
  ggplot(aes(x = Data, y = Valor))+
  geom_line(size = .7, colour = 'black')+
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_line(colour = 'lightgray'),
        panel.background = element_rect(fill = 'white', colour = 'white'))+
  labs(title = 'Série Histórica IPCA - Índice',
       x = '',
       y = 'Valor Índice',
       caption = 'Fonte: SIDRA - IBGE')

# criando e formatando o gráfico para a série histórica da variação mensal
ipca_vm %>%
  ggplot(aes(x = Data, y = Valor))+
  geom_line(size = .7, colour = 'black')+
  geom_hline(yintercept = 0, colour = 'red', linetype = 'dashed')+
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_line(colour = 'lightgray'),
        panel.background = element_rect(fill = 'white', colour = 'white'))+
  labs(title = 'Série Histórica IPCA - Variação Mensal',
       x = '',
       y = '% a.m.',
       caption = 'Fonte: SIDRA - IBGE')

# exportando os arquivos para uma planilha em excel
write_xlsx(ipca_indice, 'série histórica - índice.xlsx')
write_xlsx(ipca_vm, 'série histórica - variação mensal.xlsx')
