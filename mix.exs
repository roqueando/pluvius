defmodule Pluvius.MixProject do
  use Mix.Project

  def project do
    [
      app: :pluvius,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Pluvius.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:nimble_csv, "~> 1.2"},
      {:flow, "~> 1.2"},
      {:axon, "~> 0.6.1"},
      {:explorer, "~> 0.9"},
      {:nx, "~> 0.7.3"}
    ]
  end
end
