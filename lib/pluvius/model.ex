defmodule Pluvius.Model do
  def train do
    %{model: model, stream: stream} = model()

    # FIXME: choose correctly the training options
    # TODO: Pluvius will be a regression model so
    # is not needed to clean and normalize rmm feature
    model
    |> Axon.Loop.trainer(:binary_cross_entropy, :adam)
    |> Axon.Loop.metric(:accuracy)
    |> Axon.Loop.run(stream, %{}, iterations: 500, epochs: 20)
  end

  def test(model_state) do
    %{model: model, test_set: test_set} = model()

    model
    |> Axon.Loop.evaluator()
    |> Axon.Loop.metric(:accuracy)
    |> Axon.Loop.run([test_set], model_state)
  end

  defp model do
    {train_set, test_set} = Pluvius.Extract.run()

    # FIXME: create correctly the model architecture
    model =
      Axon.input("pluvius_features", shape: {nil, 12})
      |> Axon.dense(6, activation: :relu)
      |> Axon.dropout(rate: 0.5)
      |> Axon.dense(1, activation: :softmax)

    data_stream = Stream.repeatedly(fn -> train_set end)

    %{
      train_set: train_set,
      test_set: test_set,
      model: model,
      stream: data_stream
    }
  end
end
