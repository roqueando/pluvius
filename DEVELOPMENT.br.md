# DEVELOPMENT

Aqui eu vou tentar criar alguns itens TODO e descrever o que eu estou fazendo por features/hotfixes/releases.

## feat/featurizer

* [06.12.2024] Começando o featurizer, um worker haskell
    - [ ] {EPIC} criar o pipeline worker em haskell para o preprocessamento do dataset
        - [ ] Tentar usar o Conduit para processar em streaming o CSV
            - Levando em consideração que o tamanho mínimo desse dataset gira em torno de ~500MB
        - [ ] Primeira tentativa, printar as linhas ao ir processando os dados


#### Descrição dos dados, marcaçãoo de quem vai ser usado
>#### Os dados que não for usado não vai ser marcado
>#### Marcados com ~ serão transformados em um novo dado numérico
>#### Marcados com x serão apenas limpos

[~] Data { será transformado em unix time para ser usado como medida temporária numérica }
[~] Hora UTC { será transformado em unix time para ser usado como medida temporária numérica }
[x] PRECIPITAÇÃO TOTAL, HORÁRIO (mm)
[x] PRESSÃO ATMOSFERICA MAX.NA HORA ANT. (AUT) (mB)
[x] PRESSÃO ATMOSFERICA MIN. NA HORA ANT. (AUT) (mB)
[x] TEMPERATURA MÁXIMA NA HORA ANT. (AUT) (°C),
[x] TEMPERATURA MÍNIMA NA HORA ANT. (AUT) (°C),
[x] TEMPERATURA ORVALHO MAX. NA HORA ANT. (AUT) (°C),
[x] TEMPERATURA ORVALHO MIN. NA HORA ANT. (AUT) (°C),
[~] UMIDADE REL. MAX. NA HORA ANT. (AUT) (%),
[~] UMIDADE REL. MIN. NA HORA ANT. (AUT) (%),


### Limpeza dos dados

Como a Data e Hora são datas temporais, elas precisam ser transformadas em números. Uma ideia que tenho é transforma-las
em números UNIX Timestamp para que se tornem números escalares

## Alteração do escopo, o projeto será um projeto haskell com utilitários python
- [ ] tentar passar os arquivos do cabal para o projeto raiz
