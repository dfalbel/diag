diag
====

Pacote para geração de gráficos de diagnóstico para modelos de regressão, baseado nos códigos do professor Gilberto A. Paula
(www.ime.usp.br/~giapaula).

## Atualmente implementado

A única função implementada é a `envel_norm` que pode ser utilizada da seguinte forma:

    modelo <- lm(mpg ~ cyl + disp, data = mtcars)
    envel_norm(modelo)

## Chaining

As funções desse pacote são pensadas para que seja possível utlizar o operado `%>%`. Então, é possível criar o gráfico
de envelope da forma a seguir:

    modelo %>% envel_norm() %>% idf_indice(5)

## Em desenvolvimento...

* Outros gráficos de diagnóstico (para outros modelos e para o próprio modelo normal)
* Função que podem ser utilizadas para identificar observações no gráfico.
* Melhorias no desempenho das funções que calculam os resíduos e bandas de confiança.
