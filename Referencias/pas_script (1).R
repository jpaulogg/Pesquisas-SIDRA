#------------------INSTALANDO OS PACOTES

pacotes <- c("data.table", "dplyr", "stringr", "ggplot2")

lapply(pacotes, library, character.only = TRUE)

#------------------IMPORTAÇÃO

pas_tab2715 <- fread(#"https://sidra.ibge.gov.br/geratabela?format=br.csv&name=tabela2715.csv&terr=NC&rank=-&query=t/2715/n1/all/v/all/p/all/c12354/all/c12355/all/l/,,c12355%2Bc12354%2Bv%2Bp%2Bt",
                    "tabela2715.csv", 
                    col.names = c("regioes","cod","pais","atv_de_servicos", "variavel", "ano",  "Valor"),
                     integer64 = "numeric",
                     na.strings = c('"-"', '"X"'),
                     colClasses = c(list("factor" = c(1:6)), "numeric"=7),
                     encoding = "UTF-8")

pas_tab2715$Valor<-pas_tab2715$Valor/1000

#------------------primeiro exemplo
#1 UF (Minas), 1 variavel (Pessoal ocupado), N serviços (no caso os serviços prestados as familias)

dados1 <- pas_tab2715 %>%
  filter(
    regioes == "Minas Gerais",
    str_detect(variavel, "Pessoal"),
    str_detect(atv_de_servicos, "1.")
  )
#excluir variavel 4.1

grafico1 <- dados1 %>%
  ggplot(aes(x = ano, y = Valor)) +
  geom_line(aes(color = atv_de_servicos, group = atv_de_servicos)) +
  ylab("Pessoal ocupado em 31/12 (Pessoas)")

grafico1

#segundo exemplo
#1 UF (Minas), 1 variavel (receita bruta de serviços), N serviços (no caso Transportes, serviços auxiliares aos transportes e correio)

dados2 <- pas_tab2715 %>%
  filter(
    regioes == "Minas Gerais",
    str_detect(variavel, "Receita"),
    str_detect(atv_de_servicos, "4.")
  )

grafico2 <- dados2 %>%
  ggplot(aes(x = ano, y = Valor)) +
  geom_line(aes(color = atv_de_servicos, group = atv_de_servicos)) +
  ylab("Receita bruta de serviços (Milhoes de Reais)")

grafico2


#terceiro exemplo
#comparando dados MG vs BRASIL DE RECEITA POR SETOR DE ATIVIDADE (TOTAL)

dados3 <- pas_tab2715 %>%
  filter(
    regioes %in% c('Minas Gerais','Brasil'),
    str_detect(variavel, "empresas"),
    str_detect(atv_de_servicos, "3.")
  )

grafico3 <- dados3 %>%
  ggplot(aes(x = ano, y = Valor)) +
  geom_line(aes(color = regioes, group = regioes)) +
  ylab("Receita bruta de serviços (Mil Reais)")

grafico3

teste <- pas_tab2715 %>%  filter(grandes_reg_UFs == "Brasil", !is.na(Valor)) 

