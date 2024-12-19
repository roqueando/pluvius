defmodule Core.PipelineUseCase do
  @database Application.compile_env(:pluvius, :database)

  def run_pipeline(date) do
    with {:ok, :enriched_data} <- @database.enrich_data(date) do
      {:ok, :enriched_data}
    else
      {:error, reason} -> {:error, reason}
    end
  end

end
