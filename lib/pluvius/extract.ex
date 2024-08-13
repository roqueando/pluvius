defmodule Pluvius.Extract do
  require Explorer.DataFrame, as: DF

  @moduledoc """
  explanation of feature columns
  - tind: temperature in day
  - t[max/min]: max/min temperature in day

  - hind: humidity in day
  - h[max/min]: max/min humidiy in day

  - dpind: dew point in day (ponto de orvalho -- PT-BR)
  - dp[max/min]: max/min dew point in day

  - pind: pressure in day
  - p[max/min] max/min pressure in day
  """
  @feature_columns ~w(tind tmax tmin hind hmax hmin dpind dpmax dpmin pind pmax pmin)

  def run do
    dataset()
    |> clean()
    |> normalize()
    |> split()
  end

  defp dataset do
    get_data()
    |> DF.new()
  end

  defp clean(dataset) do
    dataset
    |> DF.mutate(
      tind: (if is_nil(tind), do: mean(tind), else: tind),
      hind: (if is_nil(hind), do: mean(hind), else: hind),
      dpind: (if is_nil(dpind), do: mean(dpind), else: dpind),
      hmax: (if is_nil(hmax), do: mean(hmax), else: hmax),
      hmin: (if is_nil(hmin), do: mean(hmin), else: hmin),
      dpmax: (if is_nil(dpmax), do: mean(dpmax), else: dpmax),
      dpmin: (if is_nil(dpmin), do: mean(dpmin), else: dpmin)
    )
  end

  defp normalize(dataset) do
    DF.mutate(
      dataset,
      for col <- across(^@feature_columns) do
        {col.name, (col - mean(col)) / variance(col)}
      end
    )
    |> DF.mutate(rmm: (if rmm == 0.0, do: 0, else: 1))
    |> DF.shuffle()
  end

  defp split(normalized_dataset) do
    train_df = DF.sample(normalized_dataset, 0.8)
    test_df = DF.sample(normalized_dataset, 0.2)

    x_train =
      train_df[@feature_columns]
      |> Nx.stack(axis: -1)

    x_test =
      test_df[@feature_columns]
      |> Nx.stack(axis: -1)

    y_train =
      train_df["rmm"]
      |> Nx.stack(axis: -1)

    y_test =
      test_df["rmm"]
      |> Nx.stack(axis: -1)

    {{x_train, y_train}, {x_test, y_test}}
  end

  defp get_data do
    NimbleCSV.define(Pluvius.Parser, separator: ";", escape: "\"")

    Path.relative("data/weather_sp_jan_aug.csv")
    |> File.stream!([:trim_bom])
    |> Pluvius.Parser.parse_stream()
    |> Stream.map(fn data ->
      [
        date,
        _time,
        tind,
        tmax,
        tmin,
        hind,
        hmax,
        hmin,
        dpind,
        dpmax,
        dpmin,
        pind,
        pmax,
        pmin,
        _,
        _,
        _,
        _,
        _
      ] = data

      rmm = List.last(data)

      with {:ok, converted_date} <- convert_date(date),
           {:ok, converted_tind} <- convert_float(tind),
           {:ok, converted_tmax} <- convert_float(tmax),
           {:ok, converted_tmin} <- convert_float(tmin),
           {:ok, converted_hind} <- convert_float(hind),
           {:ok, converted_hmax} <- convert_float(hmax),
           {:ok, converted_hmin} <- convert_float(hmin),
           {:ok, converted_dpind} <- convert_float(dpind),
           {:ok, converted_dpmax} <- convert_float(dpmax),
           {:ok, converted_dpmin} <- convert_float(dpmin),
           {:ok, converted_pind} <- convert_float(pind),
           {:ok, converted_pmax} <- convert_float(pmax),
           {:ok, converted_pmin} <- convert_float(pmin),
           {:ok, converted_rmm} <- convert_float(rmm) do
        %{
          #date: converted_date,
          #time: nil,
          tind: converted_tind,
          tmax: converted_tmax,
          tmin: converted_tmin,
          hind: converted_hind,
          hmax: converted_hmax,
          hmin: converted_hmin,
          dpind: converted_dpind,
          dpmax: converted_dpmax,
          dpmin: converted_dpmin,
          pind: converted_pind,
          pmax: converted_pmax,
          pmin: converted_pmin,
          rmm: converted_rmm
        }
      end
    end)
    |> Enum.to_list()
  end

  defp convert_date(date) do
    date
    |> String.split("/")
    |> Enum.reverse()
    |> Enum.join("-")
    |> Date.from_iso8601()
  end

  defp convert_float(""), do: {:ok, nil}

  defp convert_float(n) do
    try do
      {:ok, String.to_float(n)}
    rescue
      _ in ArgumentError -> {:error, :failed_to_convert}
    end
  end
end
