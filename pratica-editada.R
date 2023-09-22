# Verificar sua versão do R:
# Deve ser >= 4.1.0
R.version$version.string


# https://beamilz.com/posts/2021-03-29-desvendando-erros/pt/


# IMPORTAÇÃO
# importar vários arquivos ------------------------------------------------
library(dplyr)


library(readxl)
exemplo_dados <- read_excel("dados-brutos/RelatorioQualidadeAguasSuperficiais_clorofila-a_2008-2012.xlsx", 
                            sheet = "Plan1")
View(exemplo_dados)


# Usando import dataset: lembrar de copiar o código!
# library(readxl)
# dados_exemplo <- read_excel("dados-brutos/RelatorioQualidadeAguasSuperficiais_clorofila-a_2008-2012.xlsx")
# View(dados_exemplo)

# dica: use aspas e tab
# "dados-brutos/RelatorioQualidadeAguasSuperficiais_PT_2018-2022.xlsx"


# importando um arquivo
exemplo_dados <- readxl::read_xlsx("dados-brutos/RelatorioQualidadeAguasSuperficiais_clorofila-a_2008-2012.xlsx")

# vendo as colunas
dplyr::glimpse(exemplo_dados)

# descobrindo o caminho de todos os arquivos

fs::dir_ls("dados-brutos/")


arquivos <- fs::dir_ls("dados-brutos/", glob = "*.xlsx")


# precisamos aplicar a função readxl::read_xlsx para todos eles!

# usar purrr::map() !
# exemplo:

vetor <- 1:5

# aplicar a funcao sqrt() em todos os valores de 'vetor'
purrr::map(vetor, sqrt)


# %>% 

# usar para importar:
dados_brutos_infoaguas <- arquivos |>
  purrr::map(readxl::read_xlsx, .progress = TRUE) |>
  dplyr::bind_rows()

dplyr::glimpse(dados_brutos_infoaguas)

# LIMPEZA
# renomear colunas --------------------------------------------------------


names(dados_brutos_infoaguas)

infoaguas_renomeado <- dados_brutos_infoaguas |>
  janitor::clean_names()

dplyr::glimpse(infoaguas_renomeado)



# Criar novas colunas a partir dos dados ----------------------------------

# o valor, a variável que mais temos interesse, está em formato de texto!

infoaguas_renomeado$valor

# usar as.numeric não funciona.
as.numeric(infoaguas_renomeado$valor)


# vamos conhecer as funções de parse!

# exemplo:
readr::parse_number(infoaguas_renomeado$valor,
                    locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
)

# vamos criar uma funcao!


somar <- function(x, y){
  x + y
}


parse_number_br <- function(x) {
  readr::parse_number(x,
                      locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
  )
}

parse_date_br <- function(x) {
  readr::parse_date(x, format = "%d/%m/%Y")
}

parse_date_br("15/02/1993") 


# |> pipe nativo - vem com o R base atual
# %>% pipe - vem com o tidyverse/dplyr..


# exemplo simples de mutate! usando a função que criamos
infoaguas_renomeado |>
  dplyr::mutate(valor_corrigido = parse_number_br(valor), 
                .after = valor) |> View()



# funciona com o indice
infoaguas_renomeado |>
  dplyr::mutate(
    dplyr::across(
      # .cols = argumento das colunas
      .cols = c(1, 2),
      # .fns = argumento da funcao
      .fns = parse_date_br
    ))





infoaguas <- infoaguas_renomeado |>
  dplyr::mutate(
    valor = parse_number_br(valor),
    dplyr::across(
      # .cols = argumento das colunas
      .cols = c("periodo_de", "periodo_ate", "data_coleta", "inicio_operacao"),
      # .fns = argumento da funcao
      .fns = parse_date_br, 
      .names = "{.col}_arrumado"
    ),
    lat = -parzer::parse_lat(latitude),
    long = -parzer::parse_lon(longitude)
  )

dplyr::glimpse(infoaguas)



# como transformar várias colunas de uma vez? arrumar colunas de data!
infoaguas <- infoaguas_renomeado |>
  dplyr::mutate(
    valor = parse_number_br(valor),
    dplyr::across(
      .cols = c("inicio_operacao", "data_coleta", "periodo_de", "periodo_ate"),
      .fns = parse_date_br
    ),
    latitude = -parzer::parse_lat(latitude),
    longitude = -parzer::parse_lon(longitude),
    fim_operacao_texto = as.character(fim_operacao),
    fim_operacao_texto = tidyr::replace_na(fim_operacao_texto, "Em operação")
    # dplyr::na_if(nome_col, "texto_usado_na")
  )


# Exportar os dados tratados
fs::dir_create("dados-tratados")
readr::write_rds(infoaguas, file = "dados-tratados/infoaguas.rds")

readr::read_rds("dados-tratados/infoaguas.rds")

# Dúvida: e as datas?

infoaguas |> 
  dplyr::select(data_coleta) |> 
  dplyr::mutate(
    dia = lubridate::day(data_coleta),
    mes = lubridate::month(data_coleta),
    ano = lubridate::year(data_coleta),
    mes_ano = lubridate::floor_date(data_coleta, "month")
  ) |>
  dplyr::slice_sample(n = 10)


# wide --------------------------------------------------------------------

# https://tidyr.tidyverse.org/reference/pivot_wider.html

infoaguas_wide <- infoaguas |>
  dplyr::select(codigo_ponto, data_coleta, parametro, valor) |>
  tidyr::pivot_wider(names_from = parametro, values_from = valor) |>
  janitor::clean_names()


infoaguas_long <- infoaguas_wide |>
  tidyr::pivot_longer(
    cols = c("fosforo_total", "clorofila_a"),
    names_to = "parametro",
    values_to = "valor"
  )



# mutate ---------------------------------------------------------------

# https://www.sciencedirect.com/science/article/abs/pii/S0925857413003091

calcular_tsi_tsr <- function(TP, CHLA) {
  tsi_tp <- 10 * (6 - ((-0.27637 * SciViews::ln(TP) + 1.329766) / SciViews::ln(2)))
  
  tsi_chla <- 10 * (6 - ((-0.2512 * SciViews::ln(CHLA) + 0.842257) / SciViews::ln(2)
  ))
  
  tsi_tsr <- (tsi_tp + tsi_chla) / 2
  
  tsi_tsr
}


infoaguas_wide |>
  tidyr::drop_na(fosforo_total, clorofila_a) |>
  dplyr::mutate(iet = calcular_tsi_tsr(TP = fosforo_total, CHLA = clorofila_a))




# https://dplyr.tidyverse.org/reference/case_when.html


calcular_classe_tsi <- function(tsi_tsr) {
  tsi_tsr_class <- dplyr::case_when(
    tsi_tsr <= 51.1 ~ "Ultraoligotrophic",
    tsi_tsr >= 51.2 & tsi_tsr < 53.2 ~ "Oligotrophic",
    tsi_tsr >= 53.2 & tsi_tsr < 55.8 ~ "Mesotrophic",
    tsi_tsr >= 55.8 & tsi_tsr < 58.2 ~ "Eutrophic",
    tsi_tsr >= 58.2 & tsi_tsr < 59.1 ~ "Supereutrophic",
    tsi_tsr >= 59.1 ~ "Hypereutrophic"
  )
  
  tsi_tsr_class
}



infoaguas_iet <- infoaguas_wide |>
  tidyr::drop_na(fosforo_total, clorofila_a) |>
  dplyr::mutate(
    tsi_tsr = calcular_tsi_tsr(TP = fosforo_total, 
                               CHLA = clorofila_a),
    tsi_tsr_class = calcular_classe_tsi(tsi_tsr)
  )




# broom ------------

resultado <- lm(formula = fosforo_total ~ clorofila_a,
                data = infoaguas_wide)





resultado_modelo <- lm(formula = fosforo_total ~ clorofila_a,
                       data = infoaguas_wide) |>
  broom::tidy()


resultado_modelo
# # A tibble: 2 × 5
# term        estimate std.error statistic  p.value
# <chr>          <dbl>     <dbl>     <dbl>    <dbl>
# 1 (Intercept) 0.0561   0.00658        8.52 9.44e-16
# 2 clorofila_a 0.000839 0.0000333     25.2  1.91e-74



resultado_modelo$p.value[1]

resultado_modelo$p.value[2]

