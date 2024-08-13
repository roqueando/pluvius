defmodule Pluvius.IRIS do
  require Explorer.DataFrame, as: DF

  @feature_columns ~w(sepal_length sepal_width petal_length petal_width)

  def dataset, do: Explorer.Datasets.iris()

  def normalize do
    DF.mutate(
      dataset(),
      for col <- across(^@feature_columns) do
        {col.name, (col - mean(col)) / variance(col)}
      end
    )
    |> DF.mutate([species: Explorer.Series.cast(species, :category)])
    |> DF.shuffle()
  end

  def split do
    df = normalize()
    train_df = DF.slice(df, 0..119)
    test_df = DF.slice(df, 120..149)

    x_train =
      train_df[@feature_columns]
      |> Nx.stack(axis: -1)

    x_test =
      test_df[@feature_columns]
      |> Nx.stack(axis: -1)

    y_train = 
      train_df["species"]
      |> Nx.stack(axis: -1)
      |> Nx.equal(Nx.iota({1, 3}, axis: -1))

    y_test = 
      test_df["species"]
      |> Nx.stack(axis: -1)
      |> Nx.equal(Nx.iota({1, 3}, axis: -1))

    {{x_train, y_train}, {x_test, y_test}}
  end

  def model do
    {{x_train, y_train}, _} = split()

    m =
      Axon.input("iris_features", shape: {nil, 4})
      |> Axon.dense(3, activation: :softmax)

    data_stream = Stream.repeatedly(fn -> {x_train, y_train} end)
    {m, data_stream}
  end

  def train do
    {model, data_stream} = model()

    model
    |> Axon.Loop.trainer(:categorical_cross_entropy, :sgd)
    |> Axon.Loop.metric(:accuracy)
    |> Axon.Loop.run(data_stream, %{}, iterations: 500, epochs: 10)
  end

  def test(trained_model_state) do
    {model, _} = model()
    {_, {x_test, y_test}} = split()

    model
    |> Axon.Loop.evaluator()
    |> Axon.Loop.metric(:accuracy)
    |> Axon.Loop.run([{x_test, y_test}], trained_model_state)
  end
end
