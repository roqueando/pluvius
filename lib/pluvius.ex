defmodule Pluvius do
  @moduledoc """
  Documentation for `Pluvius`.
  """

  def enrich_data(date), do: Core.PipelineUseCase.run_pipeline(date)
end
