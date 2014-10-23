diag
====

Pacote para geração de gráficos de diagnóstico para modelos de regressão, baseado nos códigos do professor Gilberto A. Paula
(www.ime.usp.br/~giapaula).

## Atualmente implementado

#### Gráfico Quantil-Quantil

Para fazer o gráfico quantil-quantil com banda de confiança para o modelo normal:

    modelo <- lm(mpg ~ cyl + disp, data = mtcars)
    modelo %>% envel_norm()
    
Identifique as observações que estão fora da banda com o comando:

    modelo %>% envel_norm() %>% idf_todas()

#### Gráfico de influência (medida h)

Para fazer o gráfico da medida h use o comando:

    modelo %>% infl_norm()


## Chaining

As funções desse pacote são pensadas para que seja possível utlizar o operado `%>%`. Então, é possível criar o gráfico
de envelope da forma a seguir:

    modelo %>% envel_norm() %>% idf_indice(5)

## Em desenvolvimento...

* Outros gráficos de diagnóstico (para outros modelos e para o próprio modelo normal)
* Função que podem ser utilizadas para identificar observações no gráfico.
* Melhorias no desempenho das funções que calculam os resíduos e bandas de confiança.
