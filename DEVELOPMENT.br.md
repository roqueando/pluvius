# DEVELOPMENT

Aqui eu vou tentar criar alguns itens TODO e descrever o que eu estou fazendo por features/hotfixes/releases.

## migrate/gopher

* [09.12.2024] Começando o pipeline em Go
    - {EPIC} criar o pipeline worker em Go para o preprocessamento do dataset
        - [ ] Tentar usar o encoding/csv e channels para processar em streaming o CSV
            - Levando em consideração que o tamanho mínimo desse dataset gira em torno de ~500MB
        - [ ] Primeira tentativa, printar as linhas ao ir processando os dados


#### Descrição dos dados, marcaçãoo de quem vai ser usado
>#### Os dados que não for usado não vai ser marcado
>#### Marcados com ~ serão transformados em um novo dado numérico
>#### Marcados com x serão apenas limpos

- [~] Data { será transformado em unix time para ser usado como medida temporária numérica }
- [~] Hora UTC { será transformado em unix time para ser usado como medida temporária numérica }
- [x] PRECIPITAÇÃO TOTAL, HORÁRIO (mm)
- [x] PRESSÃO ATMOSFERICA MAX.NA HORA ANT. (AUT) (mB)
- [x] PRESSÃO ATMOSFERICA MIN. NA HORA ANT. (AUT) (mB)
- [x] TEMPERATURA MÁXIMA NA HORA ANT. (AUT) (°C),
- [x] TEMPERATURA MÍNIMA NA HORA ANT. (AUT) (°C),
- [x] TEMPERATURA ORVALHO MAX. NA HORA ANT. (AUT) (°C),
- [x] TEMPERATURA ORVALHO MIN. NA HORA ANT. (AUT) (°C),
- [~] UMIDADE REL. MAX. NA HORA ANT. (AUT) (%),
- [~] UMIDADE REL. MIN. NA HORA ANT. (AUT) (%),


### Limpeza dos dados

- [x] Como a Data e Hora são datas temporais, elas precisam ser transformadas em números. Uma ideia que tenho é transforma-las em números UNIX Timestamp para que se tornem números escalares. (Não faz sentido, o certo é o que vem abaixo)
- [ ] Data e hora serão quebradas em partes sendo: dia, mês, ano, hora, minuto. Caso haja necessidade de escalar ou normalizar, se torna mais fácil.

## Feature Engineering, cálculo de novas features baseadas nas features anteriores

- [ ] One-Hot Encoding da Data
- [ ] One-Hot Encoding da Hora
- [ ] Agrupar por dia do mês e puxar mínimo e máximo de cada atributo e colocar como uma nova feature `{feature}_min(or_max)__by_day_in_month`
- [ ] Agrupar por dia do mês e puxar a média da temperatura por dia do mês e colocar como uma nova feature `{feature}_avg_by_day_in_month`
- [ ] {?maybe} Agrupar por dia do mês e puxar a diferença entre as features minima e máxima `{feature}_diff_by_day_in_month`
