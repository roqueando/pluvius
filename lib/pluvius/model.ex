defmodule Pluvius.Model do
  def train do
    %{model: model, stream: stream} = model()

    # FIXME: choose correctly the training options
    model
    |> Axon.Loop.trainer(:binary_cross_entropy, :sgd)
    |> Axon.Loop.metric(:accuracy)
    |> Axon.Loop.run(stream, %{}, iterations: 500, epochs: 10)
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
