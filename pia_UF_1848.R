#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# Script   : pia_sidra.R
# Descrição: Pesquisa Industrial Anual;
#     	     tabela 1848 do SIDRA - IBGE
# Criado em: 10 de outubro de 2020
# Autor    : João Paulo G. Garcia <joaopauloggarcia@gmail.com>
#------------------------------------------------------------------------------
# Software  : R 4.0.2
#
# Requisitos: Se você trabalha  com o Windows, talvez seja  necessário instalar
#	            o Rtools, o que pode ser feito através do pacote "installr";
#
# Instrução : Opcionalmente, criar pasta de arquivos "Entrada" (arquivos de
#             dados baixados) e "Saida" (exportação de tabelas e gráficos)
#------------------------------------------------------------------------------

# PACOTES ---------------------------------------------------------------------
pacotes <- list("data.table", "forcats", "magrittr",
                "ggplot2", "plotly", "RColorBrewer")

pacotes_instalados <- pacotes %in% rownames(installed.packages())

# instalar pacotes
if (any(pacotes_instalados == FALSE)) {
  install.packages(pacotes[!installed_packages])
}

lapply(pacotes, library, character.only=TRUE)

# ESTRUTURA ------------------------------------------------------------------
#...1 - Importação
#...2 - Manipulação
#...3 - Visualização
#...4 - Exportação

#============================= 1.IMPORTAÇÃO ==================================

setwd("~/R/PAC-PAM-PIA")

# objeto com a entrada de dados a serem importados
url_1848 <-  "https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela1848.csv&terr=NC&rank=-&query=t/1848/n1/all/n3/all/v/631,673,810,811,834,840/p/all/c12762/allxt/l/,,p%2Bt%2Bv%2Bc12762"
if (file.exists("Entrada/tab_1848.csv")) {
  entrada <- "Entrada/tab_1848.csv"
} else {
  entrada <- url_1848
}

# importar tabela
tab_1848 <- fread(entrada,
                  integer64 = "numeric",
                  na.strings = c('"-"','"X"'),
                  colClasses = c(list("factor" = c(1:5))),
                  col.names = c("Ano", "UF", "Estado",
                                "Var", "CNAE", "Valor"))

#============================= 2. MANIPULAÇÃO ================================

# EXEMPLO 1 ------------------------------------------------------------------
# 1 UF, 1 Variável e "N" CNAE's

n_cnae <- tab_1848[UF == "31" &
                  CNAE %like% "ferro" &
                  Var %like% "produção industrial"
                   ][, Rank := frank(-Valor, na.last = "keep"), by = Ano]

# EXEMPLO 2 ------------------------------------------------------------------	
# 1 UF, todas as Variáveis (tirando pessoal ocupado) e 1 CNAE

n_var <- tab_1848[UF == "31" &
                   Var  %like% "Mil Reais" &
                   CNAE %like% "10.8"]

# EXEMPLO 3 ------------------------------------------------------------------	
# "N" UF, 1 Variáveis e 1 CNAE

n_uf <- tab_1848[UF %in% c("31", "33", "35", "41") &
                 Var %like% "Pessoal" &
                 CNAE %like% "14 "]

# EXEMPLO 4 ------------------------------------------------------------------	
# Todos os Estado, 2 Variáveis (em formato wide), 6 Anos e todas as CNAE's

# filtrar os dados e então passar para o formato wide com a função "dcast"
wide_var <- tab_1848[UF != "1" &
                   Var %like% "Salário|Custos" &
                   Ano %in% as.factor(2013:2018)] %>%
            dcast(Ano + Estado + CNAE ~ Var, value.var = "Valor")

# simplificar nomes de colunas
colnames(wide_var)[4:5] = c("Custos", "Salarios")

# criar colunas de rank
wide_var[, `:=` (Rank_C = frank(-Custos, na.last = "keep"),
                 Rank_S = frank(-Salarios, na.last = "keep")),
         by = .(Ano, Estado)]

#---------------------------- 3. VISUALIZAÇÃO --------------------------------

# GRÁFICO DE BOLHAS ----------------------------------------------------------
# As 10 maiores CNAE's por Variável, Estado e Ano.

# CANE's em ordem crescente
n_cnae$CNAE <- fct_reorder(n_cnae$CNAE, n_cnae$Rank, .desc = TRUE)

g_bolha <-  n_cnae[Rank <= 10] %>%
  ggplot(aes(x = Ano, y = CNAE, text = paste("Rank: ", Rank))) +
  geom_point(
    aes(size = Valor, color = as.factor(Rank)),
    show.legend = F
  ) +
  scale_size(range = c(3, 12)) +
  scale_color_brewer(palette = "Paired") +
  labs(y = "Produtos da Lavoura") 

# visualizar com o plotly
ggplotly(g_bolha, tooltip = c("text", "y", "size")) %>% hide_guides()

# GRÁFICO DE CAIXA -----------------------------------------------------------
# 1 Estado, 1 Variável, todos os Anos e todas as CNAE's

# usando os ranks podemos escolher a abrangência das CNAE's
distribuicao <- n_cnae[Rank %between% c(20,50)] %>%
  ggplot(aes(x = Ano, y = Valor/1000)) +
  geom_jitter(aes(text = paste("CNAE: ", CNAE)),
              fill = "pink", alpha = 0.3, shape = 21) +
  geom_boxplot(aes(fill = Ano), alpha = 0.6) +
  theme_classic()

# visualizar com o plotly
ggplotly(distribuicao, tooltip = c("text", "y")) %>% hide_guides()

# GRÁFICO DE CORRELAÇÃO ------------------------------------------------------

# usando os ranks podemos escolher a abrangência das duas variáveis
g_corr <- wide_var[Rank_C > 20 & Rank_S > 20] %>%
  ggplot(aes(x = Salarios/1000, y = Custos/1000)) +
  geom_jitter(aes(fill = Estado, text = paste("CNAE: ", CNAE),
                  shape = "21", alpha = 0.3)) +
  geom_smooth(aes(color = Ano), alpha = 0.6) +
  theme_bw() +
  facet_wrap(~Ano, nrow = 2)

ggplotly(g_corr) %>% hide_guides()

# SÉRIE HISTÓRICA ------------------------------------------------------------

# elaboração do gráfico
serie_h <- n_cnae %>%
  ggplot(aes(x = Ano, y = Valor/1000)) +
  geom_line(aes(color = CNAE, group = CNAE)) +
  ylab("VPB Industrial (Mil Reais)")

# visualizar
ggplotly(serie_h, tooltip = c("colors", "y", "x"))
