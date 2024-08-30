# Classificação de Dados de Câncer de Mama

Este repositório contém todas as implementações de código de classificação para os conjuntos de dados sobre câncer de mama retirados dos seguintes repositórios: TCGA, GEO, e Wisconsin. Os projetos incluem pré-processamento de dados, treinamento de modelos de classificação e avaliação, com foco em reprodutibilidade e análise robusta.

## Bibliotecas e Dependências

Dentro de cada pasta, você encontrará um arquivo (`requirements.txt` para Python ou `DESCRIPTION`/`renv.lock` para R) que especifica todas as versões das bibliotecas utilizadas. Isso garante que as análises possam ser reproduzidas com as mesmas versões das dependências.

## Estrutura e Descrição do Repositório

Cada pasta no repositório corresponde a um conjunto de dados específico e contém todos os códigos e arquivos necessários para a reprodução das análises:

### TCGA Repository

*Neste projeto, realizamos uma série de etapas para a análise dos dados do TCGA. Primeiro, aplicamos a normalização usando o Z-score para padronizar os dados. Em seguida, empregamos a Análise de Componentes Principais (PCA) para reduzir a dimensionalidade dos dados. Utilizamos diversos modelos de classificação, incluindo Random Forest (RF), Regressão Logística (RL), Multilayer Perceptron (MLP), Convolutional Neural Networks (CNN) e Support Vector Machine (SVM). O repositório contém códigos tanto com a otimização de hiperparâmetros usando Optuna quanto versões sem o uso do Optuna, permitindo comparações entre diferentes abordagens.*

### GEO Repository

*Para os dados do GEO, seguimos uma abordagem semelhante. Inicialmente, aplicamos a normalização com o Z-score para padronizar os dados. Em seguida, utilizamos a PCA para reduzir a dimensionalidade, facilitando a análise dos dados. Implementamos vários modelos de classificação, como Random Forest (RF), Regressão Logística (RL), Multilayer Perceptron (MLP), Convolutional Neural Networks (CNN) e Support Vector Machine (SVM). O repositório inclui códigos que utilizam Optuna para otimização de hiperparâmetros, bem como versões dos códigos sem essa otimização, permitindo uma análise mais completa das técnicas empregadas.*

### Wisconsin Repository

*A análise dos dados do Wisconsin seguiu uma abordagem um pouco diferente, devido à natureza binária dos dados.*

