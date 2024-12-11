package feature_extractor

type Weather struct {
	Date string `csv:"date"`
	Hour string  `csv:"hour"`
	Rain *float64  `csv:"rain"`
	Pmax *float64  `csv:"pmax"`
	Pmin *float64  `csv:"pmin"`
	Tmax *float64  `csv:"tmax"`
	Tmin *float64  `csv:"tmin"`
	Dpmax *float64  `csv:"dpmax"`
	Dpmin *float64  `csv:"dpmin"`
	Hmax *float64  `csv:"hmax"`
	Hmin *float64  `csv:"hmin"`
}

type enriched_weather struct {
	 	day int `csv:"day"`
    month int `csv:"month"`
    year int `csv:"year"`
    hour int `csv:"hour"`
    minute int `csv:"minute"`

    // Min and Max by day of month (DoM)
    pmax_dom float64 `csv:""`
    pmin_dom float64 `csv:""`
    tmax_dom float64 `csv:""`
    tmin_dom float64 `csv:""`
    dpmax_dom float64 `csv:""`
    dpmin_dom float64 `csv:""`
    hmax_dom float64 `csv:""`
    hmin_dom float64 `csv:""`

    // Difference from Min Max by day of month
    p_diff float64 `csv:""`
    t_diff float64 `csv:""`
    dp_diff float64 `csv:""`
    h_diff float64 `csv:""`

    // Average (mean) from min max by day of month
    pAvgMinDom float64 `csv:""`
    pAvgMaxDom float64 `csv:""`
    tAvgMinDom float64 `csv:""`
    tAvgMaxDom float64 `csv:""`
    dpAvgMinDom float64 `csv:""`
    dpAvgMaxDom float64 `csv:""`
    hAvgMinDom float64 `csv:""`
    hAvgMaxDom float64 `csv:""`
}
