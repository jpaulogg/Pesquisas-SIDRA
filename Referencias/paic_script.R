pacotes <- c("data.table", "dplyr", "stringr", "ggplot2")

lapply(pacotes, library, character.only = TRUE)

paic_tab1757 <- fread(#"https://sidra.ibge.gov.br/geratabela?format=xlsx&name=tabela1757.xlsx&terr=N&rank=-&query=t/1757/n1/all/n2/all/n3/all/v/410,630,631,634,637,673,1239,1240,1242/p/all/c319/104030/l/,,t%2Bc319%2Bp%2Bv",
  "tabela1757.csv", 
  col.names = c("Unid_territorial","faixa_pessoal","ano","variavel", "valor"),
  integer64 = "numeric",
  na.strings = c('"-"', '"X"', '"..."'),
  colClasses = c(list("factor" = c(1:4)), "numeric"=5),
  encoding = "UTF-8")

pas_tab2715$valor <- pas_tab2715$valor/1000

#Consumo intermediário das empresas do BR 

dados11 <- paic_tab1757 %>%
  filter(
    #Unid_territorial %in% c('Minas Gerais', 'Brasil'),
    Unid_territorial == "Brasil",
    str_detect(variavel, "Consumo"),
    #variavel == "Consumo"
  )

grafico11 <- dados11 %>%
  ggplot(aes(x = ano, y = valor)) +
  geom_line(aes(color = Unid_territorial, group = "Unid_territorial")) +
  ylab("Consumo intermediário - total (Milhões de reais)")

grafico11

ggplotly(grafico11)


