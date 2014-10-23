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

#### Gráfico da distância de Cook

O gráfico da distância de Cook é feito usando:

    modelo %>% dcook_norm()

#### Gráfico do resíduo padronizado pelo índice

Este gráfico pode ser gerado assim:

    modelo %>% resind_norm()
    
Obs: eu sei que este nome está horrível :(

#### Gráfico do resíduo pelo valor ajustado

Este gráfico é gerado com a seguinte sequência de comandos:

    modelo %>% resva_norm()
    
Obs: este nome também está bem ruim.


## Chaining

As funções desse pacote são pensadas para que seja possível utlizar o operado `%>%`. Então, é possível criar o gráfico
de envelope e em seguida identificar a observação 5 com o comando:

    modelo %>% envel_norm() %>% idf_indice(5)

## Em desenvolvimento...

* Outros gráficos de diagnóstico (para outros modelos e para o próprio modelo normal)
* Função que podem ser utilizadas para identificar observações no gráfico.
* Melhorias no desempenho das funções que calculam os resíduos e bandas de confiança.
