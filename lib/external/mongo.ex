defmodule External.Mongo do
  @behaviour Core.DataPipeline

  def enrich_data(date) do
    {:ok, top} =
      Mongo.start_link(
        url: "mongodb://localhost:27017/feature_store",
        username: "pluvius",
        password: "local_password",
        database: "feature_store",
        auth_source: "admin"
      )

    top
    |> Mongo.aggregate("raw", [
      %{
        "$match" => %{
          "date" => date
        }
      },
      %{
        "$project" => %{
          "date" => 1,
          "hour" => 1,
          "rain" => 1,
          "pmax" => %{"$divide" => ["$pmax", 10]},
          "pmin" => %{"$divide" => ["$pmin", 10]},
          "tmax" => 1,
          "tmin" => 1,
          "dpmax" => 1,
          "dpmin" => 1,
          "hmax" => 1,
          "hmin" => 1,
          "pdiff" => %{"$subtract" => ["$pmax", "$pmin"]},
          "tdiff" => %{"$subtract" => ["$tmax", "$tmin"]},
          "dpdiff" => %{"$subtract" => ["$dpmax", "$dpmin"]},
          "hdiff" => %{"$subtract" => ["$hmax", "$hmin"]},
          "pmax_avg" => %{"$avg" => "$pmax"},
          "pmin_avg" => %{"$avg" => "$pmin"},
          "tmax_avg" => %{"$avg" => "$tmax"},
          "tmin_avg" => %{"$avg" => "$tmin"},
          "dpmax_avg" => %{"$avg" => "$dpmax"},
          "dpmin_avg" => %{"$avg" => "$dpmin"},
          "hmax_avg" => %{"$avg" => "$hmax"},
          "hmin_avg" => %{"$avg" => "$hmin"}
        }
      },
      %{
        "$out" => %{
          "db" => "feature_store",
          "coll" => "enriched"
        }
      }
    ])

    Process.exit(top, :normal)
    {:ok, :enriched_data}
  end
end
