# Modelagem-de-Dados-de-Contagem-com-Regressao-Poisson-em-R

Neste estudo de caso, utilizamos técnicas de regressão Poisson para modelar dados de contagem, especificamente a quantidade de atrasos em voos. O objetivo é entender os fatores que influenciam os atrasos e prever a quantidade de atrasos com base em variáveis explicativas. Utilizaremos a linguagem R para realizar todas as etapas do processo, desde o carregamento dos dados até a avaliação do modelo.

## Instalação e Carregamento de Pacotes Necessários

Primeiro, instalamos e carregamos os pacotes necessários:

# Pacotes utilizados
pacotes <- c("plotly", "tidyverse", "knitr", "kableExtra", "fastDummies", "reshape2",
             "lmtest", "splines", "jtools", "questionr", "MASS", "pscl", "overdisp")

if (sum(as.numeric(!pacotes %in% installed.packages())) != 0) {
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for (i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()
  }
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

## Distribuição Poisson - Parte Conceitual

Definimos funções para a distribuição Poisson com diferentes valores de lambda e plotamos as funções:

# Função da distribuição Poisson com lambda = 1
poisson_fun_1 <- function(x) {
  lambda <- 1
  (exp(-lambda) * lambda ^ x) / factorial(x)
}

# Função da distribuição Poisson com lambda = 4
poisson_fun_4 <- function(x) {
  lambda <- 4
  (exp(-lambda) * lambda ^ x) / factorial(x)
}

# Função da distribuição Poisson com lambda = 10
poisson_fun_10 <- function(x) {
  lambda <- 10
  (exp(-lambda) * lambda ^ x) / factorial(x)
}

# Plotagem das funções estabelecidas anteriormente
ggplotly(
  ggplot(data.frame(x = 0:20), aes(x = x)) +
    stat_function(fun = poisson_fun_1, size = 1.2, aes(color = "Lambda igual a 01")) +
    stat_function(fun = poisson_fun_4, size = 1.2, aes(color = "Lambda igual a 04")) +
    stat_function(fun = poisson_fun_10, size = 1.2, aes(color = "Lambda igual a 10")) +
    scale_color_manual("Valores de lambda", values = c("darkorchid", "orange", "black")) +
    labs(y = "Probabilidades", x = "m") +
    theme_bw()
)

## Carregamento da Base de Dados

Carregamos os dados da base `atrasos_poisson.RData`:

# Carregando a base de dados
load(file = "atrasos_poisson.RData")

## Observação da Base de Dados

Visualizamos a base de dados e calculamos estatísticas descritivas:

# Visualizando a base de dados
atrasos_poisson %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", full_width = F, font_size = 12)

# Estrutura da base de dados
glimpse(atrasos_poisson)

# Estatísticas descritivas univariadas da base de dados
summary(atrasos_poisson)

# Tabela de frequências da variável dependente
freq(atrasos_poisson$atrasos) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", full_width = T, font_size = 12)

# Diagnóstico preliminar da presença da superdispersão na variável dependente
atrasos_poisson %>%
  summarise(média = mean(atrasos_poisson$atrasos), variância = var(atrasos_poisson$atrasos)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", full_width = T, font_size = 12)

# Visualização da distribuição da variável dependente
ggplotly(
  atrasos_poisson %>%
    ggplot() +
    geom_histogram(aes(x = atrasos, fill = ..count..), bins = 5, color = "black") +
    labs(x = "Quantidade de Atrasos", y = "Frequência") +
    scale_fill_gradient("Contagem", low = "darkorchid", high = "orange") +
    theme_bw()
)

## Modelagem de Regressão Poisson

Agora, estimamos um modelo de regressão Poisson para os dados:

# Estimando o modelo de regressão Poisson
modelo_poisson <- glm(atrasos ~ ., data = atrasos_poisson, family = poisson)

# Resumo do modelo
summary(modelo_poisson)

## Diagnóstico do Modelo

Verificamos os resíduos e a adequação do modelo:


# Resíduos do modelo
residuos <- residuals(modelo_poisson, type = "deviance")

# Plotagem dos resíduos
ggplot(data.frame(residuos = residuos), aes(x = residuos)) +
  geom_histogram(fill = "darkorchid", color = "black", bins = 30) +
  labs(x = "Resíduos", y = "Frequência") +
  theme_bw()

## Avaliação de Superdispersão

Calculamos a superdispersão dos dados:

# Função para calcular a superdispersão
calcular_superdispersao <- function(modelo) {
  phi <- sum(residuals(modelo, type = "pearson")^2) / df.residual(modelo)
  return(phi)
}

# Superdispersão
superdispersao <- calcular_superdispersao(modelo_poisson)
superdispersao

## Conclusão

Este estudo de caso demonstrou como técnicas de regressão Poisson podem ser utilizadas para modelar dados de contagem. Através da integração de dados, limpeza, modelagem e avaliação, fomos capazes de gerar insights valiosos para a tomada de decisões estratégicas.

### Pontos Importantes

1. **Introdução**: Fornece uma visão geral do projeto e sua importância.
2. **Instalação e Carregamento de Pacotes Necessários**: Descreve a instalação e carregamento dos pacotes necessários.
3. **Distribuição Poisson - Parte Conceitual**: Detalha a criação de funções para a distribuição Poisson e sua plotagem.
4. **Carregamento da Base de Dados**: Descreve o carregamento dos dados.
5. **Observação da Base de Dados**: Visualização e estatísticas descritivas dos dados.
6. **Modelagem de Regressão Poisson**: Explica a estimação do modelo de regressão Poisson.
7. **Diagnóstico do Modelo**: Verificação dos resíduos e adequação do modelo.
8. **Avaliação de Superdispersão**: Calcula a superdispersão dos dados.
9. **Conclusão**: Reflete sobre a experiência e o impacto do projeto.
