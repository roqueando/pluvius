# DEVELOPMENT

Aqui eu vou tentar criar alguns itens TODO e descrever o que eu estou fazendo por features/hotfixes/releases.

## feat/featurizer

* [06.12.2024] Começando o featurizer, um worker haskell
    - {EPIC} criar o pipeline worker em haskell para o preprocessamento do dataset
        - [x] Tentar usar o Conduit para processar em streaming o CSV
            - Levando em consideração que o tamanho mínimo desse dataset gira em torno de ~500MB
        - [x] Primeira tentativa, printar as linhas ao ir processando os dados


### Descrição dos dados, marcação de quem vai ser usado

- date :: DATA
- hour :: HORA
- rain :: PRECIPITACAO em milímetros (mm)
- pmax :: PRESSÃO ATMOSFERICA MAX.NA HORA ANT. (AUT) (mB)
- pmin :: PRESSÃO ATMOSFERICA MIN. NA HORA ANT. (AUT) (mB)
- tmax :: TEMPERATURA MÁXIMA NA HORA ANT. (AUT) (°C)
- tmin :: TEMPERATURA MÍNIMA NA HORA ANT. (AUT) (°C)
- dpmax :: TEMPERATURA ORVALHO MAX. NA HORA ANT. (AUT) (°C)
- dpmin :: TEMPERATURA ORVALHO MIN. NA HORA ANT. (AUT) (°C)
- hmax :: UMIDADE REL. MAX. NA HORA ANT. (AUT) (%)
- hmin :: UMIDADE REL. MIN. NA HORA ANT. (AUT) (%)


## Alteração do escopo, o projeto será um projeto haskell com utilitários python
- [x] tentar passar os arquivos do cabal para o projeto raiz


* [07.12.2024] Processamento dos dados
    - [ ] {EPIC} criar o pipeline worker em haskell para o preprocessamento do dataset
        - [x] O processamento come muita RAM na hora, suspeito que possa ser o print na tela
        - [x] Ajustar para só processar e filtrar os dados sem printar nada na tela.

* Descobri que ao usar as opções do GHC como `-threaded` e otimização, já consegue dar um ótimo resultado porém ainda não estou usando Haskell de forma paralela e nem concorrente.

## Feature Engineering
>### Calculando features a partir de features existentes

NOTA: Percebi que a ideia que estou implementando não é uma pesquisa num jupyter notebook da vida, e sim de fato o projeto real ja sendo "produtizado", porém como não tenho o modelo pronto, irei puxar via batches e usar uma flag para controlar o fluxo do pipeline.

1. [x] Adicionar no docker-compose o mongo db como banco de feature store
2. [x] Criar a query para puxar os dados para criar o enriched via Haskell
    - Para fazer isso posso criar funções diferentes para fazer queries e aggregates, tendo como base [esse script](scripts/mongo_enrich.js)
    - [x] Calcular a diferença entre `{feature}_max` e `{feature}_min` gerando `{feature}diff`
    - [x] Calcular a média de cada feature ao todo

## Processamento das features calculadas
>### Processar a data e hora para serem dados numéricos e aplicar a normalização dos dados

Aqui temos duas etapas separadas, fazer com que todo nosso dado calculado seja dados numéricos (pois só calculamos as features numéricas), e fazer com que nossos dados sejam devidamente normalizados. Com isso teremos a introdução de uma distribuição normal e desvios padrões e como eles podem ajudar a saber se a normalização ocorreu de uma boa forma ou não.

1. [ ] Transformar date em dia mes ano, a data vem nesse formato: `2019/01/01` (ano/mes/dia), só precisamos alterar para que ao invés de slashes, use traços.
2. [ ] Aplicar normalização e salvar com a condição que o desvio padrão seja entre 0 e 1.


[  ] {???} Ajustar o utils de download dataset do python para armazenar o dataset num mongodb
