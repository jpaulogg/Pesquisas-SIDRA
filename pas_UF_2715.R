# PAS

# -----------------------------------------------------------------------------
#                                 1. BASE DE DADOS

# caminho para o arquivo de entrada/ ou o endereço de download.
setwd("~/R/Pesquisas-IBGE/")
#
# tabela 2715                                                              #{{{
url <- "https://sidra.ibge.gov.br/geratabela?format=br.csv&name=tabela2715.csv&terr=NC&rank=-&query=t/2715/n1/all/v/all/p/all/c12354/all/c12355/all/l/,,c12355%2Bc12354%2Bv%2Bp%2Bt"
#
if (file.exists("Entrada/tab_2715.csv")) {
  entrada <- "Entrada/tab_2715.csv"
} else {
  entrada <- url
}
tab_2715 <- fread(entrada,
                  na.strings = c('"-"', '"X"', '".."', '"..."'),
                  colClasses = list("factor" = c(1:6)),
                  col.names = c("ATIVIDADE", "UF", "VARIAVEIS", "ANO"
                                "CO_PAIS", "PAIS", "VALOR"))              #}}}

#============================= 2. MANIPULAÇÃO ================================

# EXEMPLO 1  .......................................                      #{{{
# 1 UF, 1 VARiável e "N" Atividades

n_cnae <- tab_1848[UF == "31" &
                  ATIVIDADES %like% "manutenção" &
                  VAR %like% "remunerações"
                   ][, Rank := frank(-VALOR, na.last = "keep"), by = ANO] #}}}

# EXEMPLO 2                                                               #{{{	
# 1 UF, 2 VARiáveis (Receita e Salários) e 1 Atividade

n_var <- tab_1848[UF == "31" &
                  VAR  %like% "Mil Reais" &
                  ATIVIDADES %like% "informação"]                         #}}}

# EXEMPLO 3                                                               #{{{	
# "N" UF, 1 VARiáveis e 1 ATIVIDADES

n_uf <- tab_1848[UF %in% c("31", "33", "35", "41") &
                 VAR %like% "Pessoal" &
                 ATIVIDADES %like% "4. "]                                 #}}}

# EXEMPLO 4                                                               #{{{ 	
# Todos os Estado, 2 VARiáveis (em formato wide), 6 Anos e todas as Atividades

# filtrar os dados e então passar para o formato wide com a função "dcast"
wide_var <- tab_1848[UF != "Brasil" &
                     VAR %like% "Salários|Receita" &
                     ANO %in% as.factor(2013:2018)] %>%
#
            dcast(ANO + ESTADo + ATIVIDADES ~ VAR, value.var = "VALOR")

# simplificar nomes de colunas
colnames(wide_var)[4:5] = c("Salarios", "Receita.bruta")

# criar colunas de rank
wide_var[, `:=` (Rank_C = frank(-Salarios, na.last = "keep"),
                 Rank_S = frank(-Receita.bruta, na.last = "keep")),
         by = .(ANO, ESTADO)]                                             #}}}

#---------------------------- 3. VISUALIZAÇÃO --------------------------------

# GRÁFICO DE BOLHAS                                                       #{{{
# As 10 maiores Atividades por VARiável, Estado e Ano.

# CANE's em ordem crescente
n_cnae$ATIVIDADES <- fct_reorder(n_cnae$ATIVIDADES, n_cnae$Rank, .desc = TRUE)

g_bolha <-  n_cnae[Rank <= 10] %>%
  ggplot(aes(x = ANO, y = ATIVIDADES, text = paste("Rank: ", Rank))) +
  geom_point(
    aes(size = VALOR, color = as.factor(Rank)),
    show.legend = F
  ) +
  scale_size(range = c(3, 12)) +
  scale_color_brewer(palette = "Paired") +
  labs(y = "Produtos da Lavoura") 

# visualizar com o plotly
ggplotly(g_bolha, tooltip = c("text", "y", "size")) %>% hide_guides()

# GRÁFICO DE CAIXA -----------------------------------------------------------
# 1 Estado, 1 VARiável, todos os Anos e todas as Atividades

# usando os ranks podemos escolher a abrangência das Atividades
distribuicao <- n_cnae[Rank %between% c(20,50)] %>%
  ggplot(aes(x = ANO, y = VALOR/1000)) +
  geom_jitter(aes(text = paste("ATIVIDADES: ", ATIVIDADES)),
              fill = "pink", alpha = 0.3, shape = 21) +
  geom_boxplot(aes(fill = ANO), alpha = 0.6) +
  theme_classic()

# visualizar com o plotly
ggplotly(distribuicao, tooltip = c("text", "y")) %>% hide_guides()

# GRÁFICO DE CORRELAÇÃO ------------------------------------------------------

# usando os ranks podemos escolher a abrangência das duas variáveis
g_corr <- wide_var[Rank_C > 20 & Rank_S > 20] %>%
  ggplot(aes(x = Receita.bruta/1000, y = Salarios/1000)) +
  geom_jitter(aes(fill = ESTADO, text = paste("ATIVIDADES: ", ATIVIDADES),
                  shape = "21", alpha = 0.3)) +
  geom_smooth(aes(color = ANO), alpha = 0.6) +
  theme_bw() +
  facet_wrap(~ANO, nrow = 2)

ggplotly(g_corr) %>% hide_guides()

# SÉRIE HISTÓRICA ------------------------------------------------------------

# elaboração do gráfico
serie_h <- n_cnae %>%
  ggplot(aes(x = ANO, y = VALOR/1000)) +
  geom_line(aes(color = ATIVIDADES, group = ATIVIDADES)) +
  ylab("VPB Industrial (Mil Reais)")

# visualizar
ggplotly(serie_h, tooltip = c("colors", "y", "x"))

# vim: set sw=2 et ft=r fdm=marker:


