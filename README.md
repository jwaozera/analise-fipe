# Análise Exploratória de Preços de Veículos (Tabela FIPE)

Este repositório contém um projeto de análise estatística voltado para o mercado automobilístico brasileiro, utilizando dados da Tabela FIPE (Fundação Instituto de Pesquisas Econômicas).

## 📊 Visão Geral do Projeto

O objetivo principal é realizar um estudo descritivo e visual da distribuição de preços de veículos, segmentando os resultados por tipo (carros, motos e caminhões) e por combustível. A análise utiliza uma abordagem estatística para identificar tendências de mercado, *outliers* e o comportamento das marcas.

### Principais Funcionalidades

*   **Processamento de Dados Volumosos**: Leitura e manipulação de arquivos `.parquet` com milhões de registros.
*   **Estatística Descritiva Completa**: Cálculo de medidas de tendência central (média, mediana, moda), dispersão (variância, desvio padrão) e forma (assimetria e curtose).
*   **Análise por Amostragem Temporal**: Filtro por *snapshots* mensais para garantir que cada veículo seja contado apenas uma vez, evitando distorções.
*   **Visualizações Profissionais**: Geração automática de gráficos de distribuição, boxplots comparativos e rankings de marcas.

## 🛠️ Tecnologias Utilizadas

O projeto foi desenvolvido em **R**, utilizando as seguintes bibliotecas:

*   `dplyr` & `tidyr`: Manipulação e limpeza de dados.
*   `ggplot2`: Criação de gráficos estéticos e informativos.
*   `arrow`: Leitura eficiente de arquivos no formato Parquet.
*   `e1071`: Cálculos estatísticos avançados (Skewness e Kurtosis).
*   `scales`: Formatação de eixos monetários (R$) e percentuais.

## 📂 Estrutura do Repositório

*   `FIPE.ipynb`: Notebook interativo (R) com a análise passo a passo e explicações detalhadas.
*   `analise_fipe.R`: Script R puro para execução em lote (*batch*) e automação.
*   `fipex-prices-latest-merged.parquet`: Dataset base contendo os registros da FIPE.

## 💾 Dataset

Os dados utilizados neste projeto foram obtidos através do dataset [Fipex Veículos Brasil](https://huggingface.co/datasets/alanwgt/fipex-veiculos-brasil) no Hugging Face, mantido por Alan W. G. Tejo.

## 🚀 Como Executar

### Opção 1: Google Colab (Recomendado)
Para executar no Google Colab:
1. Carregue o arquivo `FIPE.ipynb` no Colab.
2. Certifique-se de alterar o ambiente de execução para **R**.
3. Faça o upload do arquivo `fipex-prices-latest-merged.parquet` para a área de arquivos do Colab.
4. Execute as células de instalação de pacotes e análise.

### Opção 2: Ambiente Local
Certifique-se de ter o **R** (versão 4.0+) instalado.
1. Instale as dependências:
   ```r
   install.packages(c("arrow", "dplyr", "ggplot2", "e1071", "scales", "tidyr"))
   ```
2. Certifique-se de que o arquivo `.parquet` está na raiz do projeto.
3. Execute o script `analise_fipe.R` ou o notebook `FIPE.ipynb`.

---
*Este projeto foi desenvolvido para fins acadêmicos e de estudo de Ciência de Dados.*
