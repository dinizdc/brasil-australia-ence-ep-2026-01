# Análise Demográfica Comparada: Brasil e Austrália (1950–2050)

Scripts em R para análise demográfica comparada entre Brasil e Austrália, desenvolvidos como suporte empírico ao trabalho acadêmico *"Transições Demográficas em Mundos Divergentes: Uma Análise Comparada e Preditiva entre Brasil e Austrália (1950–2050)"*.

---

## Sobre o projeto

Este repositório contém o código completo para extração, tratamento e visualização de dados demográficos históricos e projetados, utilizando exclusivamente fontes oficiais e verificáveis. Todos os dados provêm do pacote [`wpp2024`](https://github.com/PPgp/wpp2024), interface direta com a **Revisão de 2024 do World Population Prospects (WPP 2024)** da Organização das Nações Unidas.

As projeções incluem intervalos de incerteza probabilísticos de **80% e 95%** para as principais variáveis demográficas, conforme metodologia oficial do WPP. Os cenários futuros seguem as narrativas dos **Shared Socioeconomic Pathways (SSPs)** do IPCC, com dados do [IIASA SSP Database](https://tntcat.iiasa.ac.at/SspDb).

---

## Estrutura do repositório

```
.
├── analise_demografica.R       # Script principal de análise e geração de figuras
├── README.md                   # Este arquivo
└── figuras/                    # Criada automaticamente ao rodar o script
    ├── fig1_populacao_total.pdf/.png
    ├── fig2_fecundidade.pdf/.png
    ├── fig3_expectativa_vida.pdf/.png
    ├── fig4_migracao.pdf/.png
    ├── fig5_piramides.pdf/.png
    ├── fig6_dependencia.pdf/.png
    ├── tabela_indicadores.csv
    └── tabela_indicadores.tex
```

---

## Figuras geradas

| Figura | Descrição | Dados |
|--------|-----------|-------|
| Fig. 1 | Evolução da população total (1950–2050) com IC 80% e 95% | `pop1dt`, `popproj1dt`, `popprojHigh/Low`, `popproj95u/l` |
| Fig. 2 | Taxa de Fecundidade Total (1950–2050) com IC 80% e 95% | `tfr`, `tfrprojMed`, `tfrproj80/95 u/l` |
| Fig. 3 | Expectativa de vida ao nascer (1950–2050) com IC 80% e 95% | `e0M/F`, `e0Mproj/Fproj`, variantes 80/95% |
| Fig. 4 | Migração líquida anual (1950–2023) | `mig1dt` |
| Fig. 5 | Pirâmides etárias quinquenais | `popAge5dt` |
| Fig. 6 | Razão de dependência do idoso (65+/15–64) | `popAge5dt` |

---

## Fontes de dados

| Fonte | Descrição | Acesso |
|-------|-----------|--------|
| WPP 2024 | World Population Prospects 2024, ONU | [`wpp2024`](https://github.com/PPgp/wpp2024) |
| IIASA SSP Database | Projeções por cenário SSP (SSP1, SSP2, SSP3) | [tntcat.iiasa.ac.at/SspDb](https://tntcat.iiasa.ac.at/SspDb) |

> **Nota:** Os dados dos cenários SSP (Figura 7) precisam ser baixados manualmente no portal do IIASA. Instruções detalhadas estão na Seção G do script.

---

## Requisitos

- R ≥ 4.4.0
- Pacotes CRAN:

```r
install.packages(c("tidyverse", "ggplot2", "scales", "patchwork", "ggrepel", "knitr", "remotes"))
```

- Pacote WPP 2024 (GitHub):

```r
remotes::install_github("PPgp/wpp2024")
```

---

## Como usar

1. Clone o repositório:

```bash
git clone https://github.com/seu-usuario/seu-repositorio.git
cd seu-repositorio
```

2. Abra o RStudio e instale os pacotes (rode o Bloco 1 do script uma única vez)

3. Execute o script completo:

```r
source("analise_demografica.R")
```

4. As figuras serão salvas automaticamente na pasta `figuras/` nos formatos `.pdf` e `.png`

5. Para a Figura 7 (cenários SSP), siga as instruções na Seção G do script para baixar o CSV do IIASA

---

## Integração com LaTeX

As figuras em `.pdf` são geradas para uso direto em documentos LaTeX via `\includegraphics`. A tabela de indicadores é exportada como bloco `.tex` pronto para ser incluído com `\input{figuras/tabela_indicadores.tex}`.

No preâmbulo do documento LaTeX, declare:

```latex
\graphicspath{{figuras/}}
```

---

## Referências

- United Nations, Department of Economic and Social Affairs, Population Division (2024). *World Population Prospects 2024*. https://population.un.org/wpp/
- Riahi, K. et al. (2017). The Shared Socioeconomic Pathways and their energy, land use, and greenhouse gas emissions implications. *Global Environmental Change*, 42, 153–168. https://doi.org/10.1016/j.gloenvcha.2016.05.009
- Ševčíková, H. & Gerland, P. (2024). *wpp2024: World Population Prospects 2024*. R package. https://github.com/PPgp/wpp2024
- Wickham, H. et al. (2019). Welcome to the tidyverse. *Journal of Open Source Software*, 4(43), 1686. https://doi.org/10.21105/joss.01686
- Wickham, H. (2016). *ggplot2: Elegant Graphics for Data Analysis*. Springer. https://ggplot2.tidyverse.org

---

## Licença

Este repositório é disponibilizado para fins acadêmicos. Os dados utilizados são de domínio público e estão sujeitos às condições de uso das fontes originais (ONU/WPP e IIASA).
