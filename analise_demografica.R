# =============================================================================
# ANÁLISE DEMOGRÁFICA COMPARADA: BRASIL E AUSTRÁLIA (1950–2050)
# Fonte de dados: WPP 2024 (pacote wpp2024, ONU)
# Versão final — IC 80% e 95% para população, TFT e expectativa de vida
# =============================================================================

# --- BLOCO 1: INSTALAÇÃO (rode apenas uma vez) --------------------------------
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("scales")
# install.packages("patchwork")
# install.packages("ggrepel")
# install.packages("remotes")
# remotes::install_github("PPgp/wpp2024")

# --- BLOCO 2: CARREGAR PACOTES -----------------------------------------------
library(wpp2024)
library(tidyverse)
library(ggplot2)
library(scales)
library(patchwork)
library(ggrepel)
library(knitr)

# --- BLOCO 3: CONFIGURAÇÕES GLOBAIS ------------------------------------------
if (!dir.exists("figuras")) dir.create("figuras")

COR_BRASIL    <- "#E63946"
COR_AUSTRALIA <- "#457B9D"
COR_SSP1      <- "#2A9D8F"   # verde-azulado (sustentabilidade)
COR_SSP2      <- "#E9C46A"   # amarelo (caminho do meio)
COR_SSP3      <- "#E76F51"   # laranja (rivalidade)

tema_demografico <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13, hjust = 0),
    plot.subtitle    = element_text(colour = "grey40", size = 10, hjust = 0),
    plot.caption     = element_text(colour = "grey50", size = 8, hjust = 1),
    axis.title       = element_text(size = 10),
    legend.position  = "bottom",
    legend.title     = element_text(size = 9, face = "bold"),
    panel.grid.minor = element_blank()
  )

PAISES_ISO   <- c(76L, 36L)
PAISES_NOMES <- c("76" = "Brasil", "36" = "Austrália")

# Função auxiliar: pivota datasets formato largo (colunas = anos "AAAA")
pivotar_largo_anos <- function(df, col_valor, ano_min, ano_max) {
  df |>
    filter(country_code %in% PAISES_ISO) |>
    select(country_code, matches("^\\d{4}$")) |>
    pivot_longer(
      cols      = matches("^\\d{4}$"),
      names_to  = "year",
      values_to = col_valor
    ) |>
    mutate(
      year = as.integer(year),
      pais = PAISES_NOMES[as.character(country_code)]
    ) |>
    filter(year >= ano_min, year <= ano_max)
}

# Função auxiliar: pivota datasets formato largo (colunas = quinquênios "AAAA-AAAA")
pivotar_largo_quin <- function(df, col_valor, ano_min, ano_max) {
  df |>
    filter(country_code %in% PAISES_ISO) |>
    pivot_longer(
      cols      = -c(country_code, name),
      names_to  = "periodo",
      values_to = col_valor
    ) |>
    mutate(
      year = as.integer(str_extract(periodo, "^\\d{4}")),
      pais = PAISES_NOMES[as.character(country_code)]
    ) |>
    filter(!is.na(year), !is.na(.data[[col_valor]]),
           year >= ano_min, year <= ano_max)
}

# =============================================================================
# SEÇÃO A: POPULAÇÃO TOTAL COM IC 80% E 95% (1950–2050)
# =============================================================================

data("pop1dt",      package = "wpp2024")
data("popproj1dt",  package = "wpp2024")
data("popprojHigh", package = "wpp2024")
data("popprojLow",  package = "wpp2024")
data("popproj95u",  package = "wpp2024")
data("popproj95l",  package = "wpp2024")

popproj1dt <- popproj1dt |> mutate(year = as.integer(year))

# Série histórica
pop_hist <- pop1dt |>
  mutate(year = as.integer(year)) |>
  filter(country_code %in% PAISES_ISO) |>
  select(country_code, name, year, pop) |>
  mutate(
    pais  = PAISES_NOMES[as.character(country_code)],
    pop_M = pop / 1000,
    tipo  = "Histórico"
  )

# Projeção mediana
pop_proj_med <- popproj1dt |>
  filter(country_code %in% PAISES_ISO, year >= 2024, year <= 2050) |>
  select(country_code, name, year, pop) |>
  mutate(
    pais  = PAISES_NOMES[as.character(country_code)],
    pop_M = pop / 1000,
    tipo  = "Projeção (mediana)"
  )

# IC 80% — coluna 'pais' adicionada diretamente via mutate
pop_ic80 <- pivotar_largo_anos(popprojHigh, "pop_high", 2024, 2050) |>
  mutate(pop_high_M = pop_high / 1000) |>
  left_join(
    pivotar_largo_anos(popprojLow, "pop_low", 2024, 2050) |>
      mutate(pop_low_M = pop_low / 1000) |>
      select(country_code, year, pop_low_M),
    by = c("country_code", "year")
  ) |>
  mutate(pais = PAISES_NOMES[as.character(country_code)])

# IC 95% — coluna 'pais' adicionada diretamente via mutate
pop_ic95 <- pivotar_largo_anos(popproj95u, "pop_high95", 2024, 2050) |>
  mutate(pop_high95_M = pop_high95 / 1000) |>
  left_join(
    pivotar_largo_anos(popproj95l, "pop_low95", 2024, 2050) |>
      mutate(pop_low95_M = pop_low95 / 1000) |>
      select(country_code, year, pop_low95_M),
    by = c("country_code", "year")
  ) |>
  mutate(pais = PAISES_NOMES[as.character(country_code)])

pop_completo <- bind_rows(
  pop_hist     |> select(country_code, pais, year, pop_M, tipo),
  pop_proj_med |> select(country_code, pais, year, pop_M, tipo)
)

g1 <- ggplot() +
  # IC 95% (mais larga e transparente — atrás)
  geom_ribbon(
    data = pop_ic95,
    aes(x = year, ymin = pop_low95_M, ymax = pop_high95_M, fill = pais),
    alpha = 0.10
  ) +
  # IC 80% (mais estreita e opaca — na frente)
  geom_ribbon(
    data = pop_ic80,
    aes(x = year, ymin = pop_low_M, ymax = pop_high_M, fill = pais),
    alpha = 0.25
  ) +
  geom_line(
    data = pop_completo |> filter(tipo == "Histórico"),
    aes(x = year, y = pop_M, colour = pais, group = pais),
    linewidth = 1.0
  ) +
  geom_line(
    data = pop_completo |> filter(tipo == "Projeção (mediana)"),
    aes(x = year, y = pop_M, colour = pais, group = pais),
    linewidth = 1.0, linetype = "dashed"
  ) +
  geom_vline(xintercept = 2024, colour = "grey60", linetype = "dotted") +
  annotate("text", x = 2025, y = 20,
           label = "Faixa escura: IC 80%\nFaixa clara: IC 95%",
           size = 2.8, colour = "grey40", hjust = 0) +
  scale_colour_manual(values = c("Brasil" = COR_BRASIL, "Austrália" = COR_AUSTRALIA)) +
  scale_fill_manual(values   = c("Brasil" = COR_BRASIL, "Austrália" = COR_AUSTRALIA)) +
  scale_x_continuous(breaks = seq(1950, 2050, by = 10)) +
  scale_y_continuous(labels = label_number(suffix = " M")) +
  labs(
    title    = "Figura 1 – Evolução da população total: Brasil e Austrália (1950–2050)",
    subtitle = "Linha sólida: histórico | Tracejado: mediana | Faixas: IC 80% e 95%",
    x = "Ano", y = "População (milhões)",
    colour = "País", fill = "País",
    caption = "Fonte: United Nations, World Population Prospects 2024."
  ) +
  tema_demografico

ggsave("figuras/fig1_populacao_total.pdf", g1, width = 9, height = 5.5)
ggsave("figuras/fig1_populacao_total.png", g1, width = 9, height = 5.5, dpi = 300)
cat("✔ Figura 1 salva.\n")


# =============================================================================
# SEÇÃO B: TAXA DE FECUNDIDADE TOTAL COM IC 80% E 95%
# =============================================================================

data("tfr",         package = "wpp2024")
data("tfrprojMed",  package = "wpp2024")
data("tfrproj80l",  package = "wpp2024")
data("tfrproj80u",  package = "wpp2024")
data("tfrproj95l",  package = "wpp2024")
data("tfrproj95u",  package = "wpp2024")

tfr_hist <- pivotar_largo_quin(tfr,        "tfr_val", 1950, 2015) |> mutate(tipo = "Histórico")
tfr_med  <- pivotar_largo_quin(tfrprojMed, "tfr_val", 2015, 2050) |> mutate(tipo = "Projeção")
tfr_completo <- bind_rows(tfr_hist, tfr_med)

# IC 80% — pais garantido via pivotar_largo_quin
tfr_ic80 <- pivotar_largo_quin(tfrproj80u, "tfr_high80", 2015, 2050) |>
  select(country_code, pais, year, tfr_high80) |>
  left_join(
    pivotar_largo_quin(tfrproj80l, "tfr_low80", 2015, 2050) |>
      select(country_code, year, tfr_low80),
    by = c("country_code", "year")
  )

# IC 95%
tfr_ic95 <- pivotar_largo_quin(tfrproj95u, "tfr_high95", 2015, 2050) |>
  select(country_code, pais, year, tfr_high95) |>
  left_join(
    pivotar_largo_quin(tfrproj95l, "tfr_low95", 2015, 2050) |>
      select(country_code, year, tfr_low95),
    by = c("country_code", "year")
  )

g2 <- ggplot() +
  geom_ribbon(
    data = tfr_ic95,
    aes(x = year, ymin = tfr_low95, ymax = tfr_high95, fill = pais),
    alpha = 0.10
  ) +
  geom_ribbon(
    data = tfr_ic80,
    aes(x = year, ymin = tfr_low80, ymax = tfr_high80, fill = pais),
    alpha = 0.25
  ) +
  geom_line(
    data = tfr_completo |> filter(tipo == "Histórico"),
    aes(x = year, y = tfr_val, colour = pais, group = pais),
    linewidth = 1.1
  ) +
  geom_line(
    data = tfr_completo |> filter(tipo == "Projeção"),
    aes(x = year, y = tfr_val, colour = pais, group = pais),
    linewidth = 1.1, linetype = "dashed"
  ) +
  geom_hline(yintercept = 2.1, colour = "grey40", linetype = "dotdash",
             linewidth = 0.7) +
  annotate("text", x = 1953, y = 2.3,
           label = "Nível de reposição (2,1)", size = 3, colour = "grey40") +
  scale_colour_manual(values = c("Brasil" = COR_BRASIL, "Austrália" = COR_AUSTRALIA)) +
  scale_fill_manual(values   = c("Brasil" = COR_BRASIL, "Austrália" = COR_AUSTRALIA)) +
  scale_x_continuous(breaks = seq(1950, 2050, by = 10)) +
  scale_y_continuous(limits = c(1, 7), breaks = 1:7) +
  labs(
    title    = "Figura 2 – Taxa de Fecundidade Total: Brasil e Austrália (1950–2050)",
    subtitle = "Linha sólida: histórico | Tracejado: mediana | Faixas: IC 80% e 95%",
    x = "Ano (início do quinquênio)", y = "Filhos por mulher",
    colour = "País", fill = "País",
    caption = "Fonte: United Nations, World Population Prospects 2024."
  ) +
  tema_demografico

ggsave("figuras/fig2_fecundidade.pdf", g2, width = 9, height = 5.5)
ggsave("figuras/fig2_fecundidade.png", g2, width = 9, height = 5.5, dpi = 300)
cat("✔ Figura 2 salva.\n")


# =============================================================================
# SEÇÃO C: EXPECTATIVA DE VIDA COM IC 80% E 95%
# =============================================================================

data("e0M",        package = "wpp2024")
data("e0F",        package = "wpp2024")
data("e0Mproj",    package = "wpp2024")
data("e0Fproj",    package = "wpp2024")
data("e0Mproj80l", package = "wpp2024")
data("e0Mproj80u", package = "wpp2024")
data("e0Fproj80l", package = "wpp2024")
data("e0Fproj80u", package = "wpp2024")
data("e0Mproj95l", package = "wpp2024")
data("e0Mproj95u", package = "wpp2024")
data("e0Fproj95l", package = "wpp2024")
data("e0Fproj95u", package = "wpp2024")

# Combina masculino + feminino pela média; retorna coluna com nome dinâmico
combinar_e0 <- function(dfM, dfF, col_saida, ano_min, ano_max) {
  pivotar_largo_quin(dfM, "valM", ano_min, ano_max) |>
    select(country_code, pais, year, valM) |>
    left_join(
      pivotar_largo_quin(dfF, "valF", ano_min, ano_max) |>
        select(country_code, year, valF),
      by = c("country_code", "year")
    ) |>
    mutate({{ col_saida }} := (valM + valF) / 2) |>
    select(-valM, -valF)
}

ev_hist     <- combinar_e0(e0M, e0F,             e0_val,    1950, 2015) |> mutate(tipo = "Histórico")
ev_med      <- combinar_e0(e0Mproj, e0Fproj,     e0_val,    2015, 2050) |> mutate(tipo = "Projeção")
ev_completo <- bind_rows(ev_hist, ev_med)

ev_ic80 <- combinar_e0(e0Mproj80u, e0Fproj80u, e0_high80, 2015, 2050) |>
  left_join(
    combinar_e0(e0Mproj80l, e0Fproj80l, e0_low80, 2015, 2050) |>
      select(country_code, year, e0_low80),
    by = c("country_code", "year")
  )

ev_ic95 <- combinar_e0(e0Mproj95u, e0Fproj95u, e0_high95, 2015, 2050) |>
  left_join(
    combinar_e0(e0Mproj95l, e0Fproj95l, e0_low95, 2015, 2050) |>
      select(country_code, year, e0_low95),
    by = c("country_code", "year")
  )

g3 <- ggplot() +
  geom_ribbon(
    data = ev_ic95,
    aes(x = year, ymin = e0_low95, ymax = e0_high95, fill = pais),
    alpha = 0.10
  ) +
  geom_ribbon(
    data = ev_ic80,
    aes(x = year, ymin = e0_low80, ymax = e0_high80, fill = pais),
    alpha = 0.25
  ) +
  geom_line(
    data = ev_completo |> filter(tipo == "Histórico"),
    aes(x = year, y = e0_val, colour = pais, group = pais),
    linewidth = 1.1
  ) +
  geom_line(
    data = ev_completo |> filter(tipo == "Projeção"),
    aes(x = year, y = e0_val, colour = pais, group = pais),
    linewidth = 1.1, linetype = "dashed"
  ) +
  scale_colour_manual(values = c("Brasil" = COR_BRASIL, "Austrália" = COR_AUSTRALIA)) +
  scale_fill_manual(values   = c("Brasil" = COR_BRASIL, "Austrália" = COR_AUSTRALIA)) +
  scale_x_continuous(breaks = seq(1950, 2050, by = 10)) +
  scale_y_continuous(limits = c(45, 95), breaks = seq(45, 95, by = 5)) +
  labs(
    title    = "Figura 3 – Expectativa de vida ao nascer: Brasil e Austrália (1950–2050)",
    subtitle = "Média entre sexos | Tracejado: mediana | Faixas: IC 80% e 95%",
    x = "Ano (início do quinquênio)", y = "Anos de vida esperados",
    colour = "País", fill = "País",
    caption = "Fonte: United Nations, World Population Prospects 2024."
  ) +
  tema_demografico

ggsave("figuras/fig3_expectativa_vida.pdf", g3, width = 9, height = 5.5)
ggsave("figuras/fig3_expectativa_vida.png", g3, width = 9, height = 5.5, dpi = 300)
cat("✔ Figura 3 salva.\n")

# =============================================================================
# SEÇÃO D: MIGRAÇÃO LÍQUIDA ANUAL
# =============================================================================

data("mig1dt", package = "wpp2024")

mig_hist <- mig1dt |>
  mutate(year = as.integer(year)) |>
  filter(country_code %in% PAISES_ISO) |>
  select(country_code, name, year, mig) |>
  mutate(
    pais    = PAISES_NOMES[as.character(country_code)],
    mig_mil = mig / 1000
  )

g4 <- ggplot(mig_hist,
             aes(x = year, y = mig_mil, fill = pais, colour = pais)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) +
  facet_wrap(~ pais, scales = "free_y", ncol = 1) +
  scale_fill_manual(values   = c("Brasil" = COR_BRASIL, "Austrália" = COR_AUSTRALIA)) +
  scale_colour_manual(values = c("Brasil" = COR_BRASIL, "Austrália" = COR_AUSTRALIA)) +
  scale_x_continuous(breaks = seq(1950, 2025, by = 10)) +
  scale_y_continuous(labels = label_number(suffix = " mil")) +
  labs(
    title    = "Figura 4 – Migração líquida anual: Brasil e Austrália (1950–2023)",
    subtitle = "Positivo = saldo imigratório; negativo = saldo emigratório",
    x = "Ano", y = "Migração líquida (milhares)",
    caption = "Fonte: United Nations, World Population Prospects 2024."
  ) +
  tema_demografico +
  theme(legend.position = "none")

ggsave("figuras/fig4_migracao.pdf", g4, width = 9, height = 7)
ggsave("figuras/fig4_migracao.png", g4, width = 9, height = 7, dpi = 300)
cat("✔ Figura 4 salva.\n")


# =============================================================================
# SEÇÃO E: PIRÂMIDE ETÁRIA
# =============================================================================

data("popAge5dt", package = "wpp2024")

ano_piramide <- max(popAge5dt$year[popAge5dt$year <= 2023])
cat("Ano usado para pirâmide:", ano_piramide, "\n")

ordem_etaria <- c("0-4","5-9","10-14","15-19","20-24","25-29",
                  "30-34","35-39","40-44","45-49","50-54","55-59",
                  "60-64","65-69","70-74","75-79","80-84","85-89",
                  "90-94","95-99","100+")

piramide_data <- popAge5dt |>
  filter(country_code %in% PAISES_ISO, year == ano_piramide) |>
  select(country_code, name, age, popM, popF) |>
  mutate(pais = PAISES_NOMES[as.character(country_code)]) |>
  group_by(pais) |>
  mutate(
    total = sum(popM + popF, na.rm = TRUE),
    propM = -(popM / total) * 100,
    propF =  (popF / total) * 100
  ) |>
  ungroup() |>
  pivot_longer(cols = c(propM, propF),
               names_to = "sexo", values_to = "prop") |>
  mutate(
    sexo = ifelse(sexo == "propM", "Masculino", "Feminino"),
    age  = factor(age, levels = ordem_etaria)
  ) |>
  filter(!is.na(age))

g5 <- ggplot(piramide_data,
             aes(x = prop, y = age, fill = sexo)) +
  geom_col(width = 0.85, alpha = 0.85) +
  facet_wrap(~ pais) +
  geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.5) +
  scale_fill_manual(values = c("Masculino" = "#4A90D9", "Feminino" = "#E8657A")) +
  scale_x_continuous(
    labels = function(x) paste0(abs(round(x, 1)), "%"),
    limits = c(-8, 8), breaks = seq(-8, 8, by = 2)
  ) +
  labs(
    title    = paste0("Figura 6 – Pirâmide etária: Brasil e Austrália (", ano_piramide, ")"),
    subtitle = "Proporção de cada grupo etário quinquenal sobre o total da população",
    x = "Proporção (%)", y = "Grupo etário",
    fill    = "Sexo",
    caption = "Fonte: United Nations, World Population Prospects 2024."
  ) +
  tema_demografico

ggsave("figuras/fig5_piramides.pdf", g5, width = 10, height = 6.5)
ggsave("figuras/fig5_piramides.png", g5, width = 10, height = 6.5, dpi = 300)
cat("✔ Figura 5 salva.\n")


# =============================================================================
# SEÇÃO F: RAZÃO DE DEPENDÊNCIA DO IDOSO
# =============================================================================

dep_data <- popAge5dt |>
  filter(country_code %in% PAISES_ISO) |>
  mutate(
    pais      = PAISES_NOMES[as.character(country_code)],
    pop_total = popM + popF,
    faixa = case_when(
      age %in% c("0-4","5-9","10-14")                      ~ "jovem",
      age %in% c("15-19","20-24","25-29","30-34","35-39",
                 "40-44","45-49","50-54","55-59","60-64")   ~ "ativa",
      TRUE                                                   ~ "idoso"
    )
  ) |>
  group_by(country_code, pais, year, faixa) |>
  summarise(pop = sum(pop_total, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = faixa, values_from = pop) |>
  mutate(razao_dep_idoso = (idoso / ativa) * 100)

anos_destaque <- c(1950, 1975, 2000) |> keep(~ .x %in% dep_data$year)

g6 <- ggplot(dep_data,
             aes(x = year, y = razao_dep_idoso, colour = pais, group = pais)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = dep_data |> filter(year %in% anos_destaque), size = 3) +
  scale_colour_manual(values = c("Brasil" = COR_BRASIL, "Austrália" = COR_AUSTRALIA)) +
  scale_x_continuous(breaks = seq(1950, 2025, by = 10)) +
  scale_y_continuous(labels = label_percent(scale = 1, suffix = "%"),
                     breaks = seq(0, 40, by = 5)) +
  labs(
    title    = "Figura 7 – Razão de dependência do idoso: Brasil e Austrália (1950–2023)",
    subtitle = "População de 65+ / população de 15–64 anos × 100",
    x = "Ano", y = "Razão de dependência (%)",
    colour  = "País",
    caption = "Fonte: United Nations, World Population Prospects 2024."
  ) +
  tema_demografico

ggsave("figuras/fig6_dependencia.pdf", g6, width = 9, height = 5.5)
ggsave("figuras/fig6_dependencia.png", g6, width = 9, height = 5.5, dpi = 300)
cat("✔ Figura 6 salva.\n")


# =============================================================================
# SEÇÃO G: CENÁRIOS Shared Socioeconomic Pathways (SSPs)
# Baixe o CSV em: https://tntcat.iiasa.ac.at/SspDb
# Salve como "ssp_data.csv" na pasta do projeto e descomente abaixo:
#
ssp_raw <- read_csv("ssp_data.csv")

# Filtrar e transformar dados do Brasil
ssp_brasil <- ssp_raw %>%
  filter(region == "Brazil", scenario %in% c("SSP1", "SSP2", "SSP3")) %>%
  select(scenario, `2025`:`2050`) %>%
  pivot_longer(
    cols = `2025`:`2050`,
    names_to = "year",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = scenario,
    values_from = value
  ) %>%
  mutate(
    year = as.numeric(year),
    pais = "Brasil"
  ) %>%
  select(year, SSP1, SSP2, SSP3, pais)
# Salvar ssp_brasil na pasta atual
write_csv(ssp_brasil, "ssp_brasil.csv")

# Filtrar e transformar dados da Austrália
ssp_australia <- ssp_raw %>%
  filter(region == "Australia", scenario %in% c("SSP1", "SSP2", "SSP3")) %>%
  select(scenario, `2025`:`2050`) %>%
  pivot_longer(
    cols = `2025`:`2050`,
    names_to = "year",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = scenario,
    values_from = value
  ) %>%
  mutate(
    year = as.numeric(year),
    pais = "Austrália"
  ) %>%
  select(year, SSP1, SSP2, SSP3, pais)
# Salvar ssp_australia na pasta atual
write_csv(ssp_australia, "ssp_australia.csv")

ssp_data <- bind_rows(ssp_brasil, ssp_australia) %>%
  pivot_longer(cols = c(SSP1, SSP2, SSP3),
               names_to = "cenario", values_to = "pop_M")

# --- Fígura 5: Cenários SSP -------------------------------------------------
g7 <- ggplot(ssp_data,
             aes(x = year, y = pop_M, colour = cenario,
                 linetype = cenario, group = cenario)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  facet_wrap(~ pais, scales = "free_y") +
  scale_colour_manual(
    values = c("SSP1" = COR_SSP1, "SSP2" = COR_SSP2, "SSP3" = COR_SSP3),
    labels = c("SSP1" = "SSP1 — Sustentabilidade",
               "SSP2" = "SSP2 — Caminho do Meio",
               "SSP3" = "SSP3 — Rivalidade Regional")
  ) +
  scale_linetype_manual(
    values = c("SSP1" = "solid", "SSP2" = "dashed", "SSP3" = "dotdash"),
    labels = c("SSP1" = "SSP1 — Sustentabilidade",
               "SSP2" = "SSP2 — Caminho do Meio",
               "SSP3" = "SSP3 — Rivalidade Regional")
  ) +
  scale_x_continuous(breaks = seq(2025, 2050, by = 5)) +
  scale_y_continuous(labels = label_number(suffix = " M")) +
  labs(
    title    = "Figura 5 – Projeções populacionais por cenário SSP: Brasil e Austrália (2025–2050)",
    subtitle = "Baseado nas narrativas dos Shared Socioeconomic Pathways (IPCC/IIASA)",
    x = "Ano", y = "População (milhões)",
    colour   = "Cenário SSP",
    linetype = "Cenário SSP",
    caption  = "Fontes: IIASA SSP Database 3.1; United Nations WPP 2024."
  ) +
  tema_demografico

ggsave("figuras/fig7_ssp.pdf", g7, width = 10, height = 5.5, device = "pdf")
ggsave("figuras/fig7_ssp.png", g7, width = 10, height = 5.5, dpi = 300)
cat("✔ Figura 7 salva.\n")


# =============================================================================
# SEÇÃO H: TABELA COMPLETA DE INDICADORES — geração automática do .tex
# =============================================================================

pegar_val_largo <- function(df, cod, coluna) {
  df |> filter(country_code == cod) |>
    select(all_of(coluna)) |> pull() |> round(2)
}

# --- Detecção automática de períodos disponíveis ----------------------------
ultimo_tfr <- names(tfr) |> str_subset("^\\d{4}-\\d{4}$") |> sort() |> tail(1)
ultimo_e0  <- names(e0M) |> str_subset("^\\d{4}-\\d{4}$") |> sort() |> tail(1)

# --- População total ---------------------------------------------------------
pop_1950_br  <- pop_hist |> filter(country_code == 76L, year == 1950) |> pull(pop_M) |> round(1)
pop_1950_au  <- pop_hist |> filter(country_code == 36L, year == 1950) |> pull(pop_M) |> round(1)
pop_2024_br  <- pop_hist |> filter(country_code == 76L, year == max(year)) |> pull(pop_M) |> round(1)
pop_2024_au  <- pop_hist |> filter(country_code == 36L, year == max(year)) |> pull(pop_M) |> round(1)
pop_2050_br  <- pop_proj_med |> filter(country_code == 76L, year == 2050) |> pull(pop_M) |> round(1)
pop_2050_au  <- pop_proj_med |> filter(country_code == 36L, year == 2050) |> pull(pop_M) |> round(1)

# --- TFT histórica e projetada -----------------------------------------------
tfr_1950_br  <- pegar_val_largo(tfr,        76L, "1950-1955")
tfr_1950_au  <- pegar_val_largo(tfr,        36L, "1950-1955")
tfr_rec_br   <- pegar_val_largo(tfr,        76L, ultimo_tfr)
tfr_rec_au   <- pegar_val_largo(tfr,        36L, ultimo_tfr)

# Último período disponível em tfrprojMed (projeção)
ultimo_tfr_proj <- names(tfrprojMed) |> str_subset("^\\d{4}-\\d{4}$") |> sort() |> tail(1)
tfr_proj_br  <- pegar_val_largo(tfrprojMed, 76L, "2045-2050")
tfr_proj_au  <- pegar_val_largo(tfrprojMed, 36L, "2045-2050")

# --- Expectativa de vida histórica e projetada --------------------------------
ev_1950_br <- round((pegar_val_largo(e0M, 76L, "1950-1955") +
                       pegar_val_largo(e0F, 76L, "1950-1955")) / 2, 1)
ev_1950_au <- round((pegar_val_largo(e0M, 36L, "1950-1955") +
                       pegar_val_largo(e0F, 36L, "1950-1955")) / 2, 1)

ev_rec_br  <- round((pegar_val_largo(e0M, 76L, ultimo_e0) +
                       pegar_val_largo(e0F, 76L, ultimo_e0)) / 2, 1)
ev_rec_au  <- round((pegar_val_largo(e0M, 36L, ultimo_e0) +
                       pegar_val_largo(e0F, 36L, ultimo_e0)) / 2, 1)

ev_proj_br <- round((pegar_val_largo(e0Mproj, 76L, "2045-2050") +
                       pegar_val_largo(e0Fproj, 76L, "2045-2050")) / 2, 1)
ev_proj_au <- round((pegar_val_largo(e0Mproj, 36L, "2045-2050") +
                       pegar_val_largo(e0Fproj, 36L, "2045-2050")) / 2, 1)

# --- Migração líquida média 1990–2023 ----------------------------------------
# Passo 1 — filtrar e salvar
mig_diag <- mig1dt |>
  filter(country_code %in% PAISES_ISO, year >= 1990) |>
  select(country_code, year, mig)

# Passo 2 — ver os valores
head(mig_diag, 20)

# Passo 3 — ver a média sem arredondamento
mig_hist |>
  filter(country_code %in% PAISES_ISO, year >= 1990) |>
  group_by(country_code) |>
  summarise(
    media_mig     = mean(mig,     na.rm = TRUE),
    media_mig_mil = mean(mig_mil, na.rm = TRUE)
  )

# mig já está em milhares no dataset — usar diretamente sem dividir por 1000
mig_media_br <- mig1dt |>
  filter(country_code == 76L, year >= 1990) |>
  summarise(media = round(mean(mig, na.rm = TRUE), 0)) |>
  pull(media)

mig_media_au <- mig1dt |>
  filter(country_code == 36L, year >= 1990) |>
  summarise(media = round(mean(mig, na.rm = TRUE), 0)) |>
  pull(media)

# Formatar com sinal explícito para migração
fmt_mig <- function(x) ifelse(x >= 0, paste0("+", x), as.character(x))

# --- Montar tabela -----------------------------------------------------------
tabela <- tibble(
  Indicador = c(
    "População 1950 (milhões)",
    paste0("População ", max(pop_hist$year), " (milhões)"),
    "População 2050 --- SSP2 (milhões)",
    paste0("TFT 1950--55 (filhos/mulher)"),
    paste0("TFT ", ultimo_tfr, " (filhos/mulher)"),
    "TFT 2045--50 --- proj. (filhos/mulher)",
    "Expectativa de vida 1950--55 (anos)",
    paste0("Expectativa de vida ", ultimo_e0, " (anos)"),
    "Expectativa de vida 2045--50 --- proj. (anos)",
    "Migra\\c{c}\\~ao l\\'{i}quida m\\'{e}dia 1990--2023 (mil/ano)"
  ),
  Brasil = c(
    pop_1950_br, pop_2024_br, pop_2050_br,
    tfr_1950_br, tfr_rec_br,  tfr_proj_br,
    ev_1950_br,  ev_rec_br,   ev_proj_br,
    fmt_mig(mig_media_br)
  ),
  `Austr\\'{a}lia` = c(
    pop_1950_au, pop_2024_au, pop_2050_au,
    tfr_1950_au, tfr_rec_au,  tfr_proj_au,
    ev_1950_au,  ev_rec_au,   ev_proj_au,
    fmt_mig(mig_media_au)
  )
)

print(tabela)
write_csv(tabela, "figuras/tabela_indicadores.csv")

# --- Gerar bloco LaTeX -------------------------------------------------------
tex_tabela <- knitr::kable(
  tabela,
  format   = "latex",
  booktabs = TRUE,
  escape   = FALSE,
  align    = c("l", "r", "r"),
  # caption sem \footnotesize — isso vai no tex manualmente
  caption  = "Indicadores demográficos selecionados: Brasil e Austrália."
) |>
  as.character() |>
  # Inserir [H] e \label após \begin{table}
  str_replace(
    fixed("\\begin{table}"),
    "\\begin{table}[H]\n\\label{tab:indicadores}"
  ) |>
  # Adicionar nota de rodapé ANTES de \end{table}
  str_replace(
    fixed("\\end{table}"),
    "\\footnotesize{\\textit{Fontes:} United Nations, WPP 2024; IIASA SSP Database.}\n\\end{table}"
  ) |>
  str_replace("TFT 1950",    "\\addlinespace\nTFT 1950")    |>
  str_replace("Expectativa", "\\addlinespace\nExpectativa") |>
  str_replace("Migra",       "\\addlinespace\nMigra")

writeLines(tex_tabela, "figuras/tabela_indicadores.tex")

cat("✔ Tabela salva em figuras/tabela_indicadores.csv\n")
cat("✔ Tabela LaTeX salva em figuras/tabela_indicadores.tex\n")
cat("  Inclua no .tex com: \\input{figuras/tabela_indicadores.tex}\n")





# =============================================================================
# RESUMO FINAL
# =============================================================================
cat("\n=============================================================\n")
cat("  FIGURAS GERADAS (dados reais do WPP 2024):\n")
cat("    fig1_populacao_total  — população total + IC 80% e 95%\n")
cat("    fig2_fecundidade      — TFT histórica + IC 80% e 95%\n")
cat("    fig3_expectativa_vida — e0 histórica + IC 80% e 95%\n")
cat("    fig4_migracao         — migração líquida anual\n")
cat("    fig5_piramides        — pirâmides etárias ordenadas\n")
cat("    fig6_dependencia      — razão de dependência do idoso\n")
cat("\n  PENDENTE:\n")
cat("    fig7_ssp — baixe CSV em tntcat.iiasa.ac.at/SspDb\n")
cat("=============================================================\n")
