db.raw.aggregate([
  {
    $match: {
      date: '2019/01/01' // example that how can I aggregate by some condition
    },
  },
  {
    $project: {
      date: 1,
      hour: 1,
      rain: 1,
      pmax: 1,
      pmin: 1,
      tmax: 1,
      tmin: 1,
      dpmax: 1,
      dpmin: 1,
      hmax: 1,
      hmin: 1,
      pdiff: {$subtract: ["$pmax", "$pmin"]},
      tdiff: {$subtract: ["$tmax", "$tmin"]},
      dpdiff: {$subtract: ["$dpmax", "$dpmin"]},
      hdiff: {$subtract: ["$hmax", "$hmin"]},
      pmax_avg: {$avg: "$pmax"},
      pmin_avg: {$avg: "$pmin"},
      tmax_avg: {$avg: "$tmax"},
      tmin_avg: {$avg: "$tmin"},
      dpmax_avg: {$avg: "$dpmax"},
      dpmin_avg: {$avg: "$dpmin"},
      hmax_avg: {$avg: "$hmax"},
      hmin_avg: {$avg: "$hmin"},
    }
  },
  {
    $out: {
      db: "feature_store",
      coll: "enriched_tst"
    }
  }
])
