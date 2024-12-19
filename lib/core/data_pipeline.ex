defmodule Core.DataPipeline do
  @callback enrich_data(String.t()) :: {:ok, :data_enriched} | {:error, term()}
end
