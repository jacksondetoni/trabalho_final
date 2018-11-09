# carregamento dos dados
dados<-read.table("E:\\D6 - Coleta e Analise de Dados Secundarios\\Trabalho Final\\Jackson_dados.txt",head= TRUE, sep=";")

#identificação das estruturas das variáveis

str(dados)
summary(dados)
library(dplyr)
glimpse(dados)
names(dados)

# criação de uma coluna de saldo comercial exportações - importações

dados <- dados %>%
  mutate(saldo = Vlr_export - Vlr_import)

head(dados)

# ordenar os saldos do maior para o menor

dados <-  dados %>%
  arrange(saldo)
head(dados)

# identificar os seis maiores saldo de exportação em 2017

dados_2017 <-  dados %>%
  filter(Ano==2017) %>%
  arrange(desc(saldo))

head(dados_2017)

# Produzir gráficos

# Saldo comercial total ano a ano

grafico_saldo_ano <- dados %>%
  group_by(Ano) %>%
  summarise(saldo_comercial = sum (saldo))

head(grafico_saldo_ano)

library(ggplot2)

# melhoramento da apresentação do grafico

ggplot(aes(x=Ano, y=saldo_comercial), data=grafico_saldo_ano) + geom_line() + 
  scale_x_continuous(breaks=2007:2017, labels = as.character (2007:2017))

# adequação da escala da ordenada, visibilidade da linha e alteração de cor e legendas


ggplot(aes(x=Ano, y=saldo_comercial/1000000), data=grafico_saldo_ano) + geom_line(colour="red", size = 2) + 
  scale_x_continuous(breaks=2007:2017, labels = as.character (2007:2017)) +
  labs (title="Saldo Comercial anual, em US$ milhões", y="Saldo Comercial") + theme_minimal()



# Grafico com setores de maiores saldos comerciais em 2017

head(dados_2017)

grafico_maioressaldos2017 <- dados_2017 [1:5, ]
ggplot(data=grafico_maioressaldos2017, aes(x=Desc_Cnae, y=saldo)) + geom_bar (stat="identity") 


# adequação da escala da ordenada, visibilidade da linha e alteração de cor e legendas


ggplot(data=grafico_maioressaldos2017, aes(x=Desc_Cnae, y=saldo/1000000, fill=Desc_Cnae)) + geom_bar (stat="identity") +
  theme(axis.text.x = element_blank()) + labs (title="Maiores Saldos Comercias em 2017, em US$ milhões", x="", fill="CNAE", y="Saldo")



# Grafico com setores de Menores saldo comercial em 2017

dados_2017_menorsaldo <-  dados %>%
  filter(Ano==2017) %>%
  arrange((saldo))

head (dados_2017_menorsaldo)

ggplot(data=dados_2017_menorsaldo [1:5, ], aes(x=Desc_Cnae, y=saldo)) + geom_bar (stat="identity") 


# adequação da escala da ordenada, visibilidade da linha e alteração de cor e legendas


ggplot(data=dados_2017_menorsaldo [1:5, ], aes(x=reorder (Desc_Cnae, saldo), y=saldo/1000000, fill=Desc_Cnae)) + geom_bar (stat="identity") +
theme(axis.text.x = element_blank()) + labs (title="Menores Saldos Comercias em 2017, em US$ milhões", x="", fill="CNAE", y="Saldo")


