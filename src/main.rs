use std::path::Path;
use candle_core::{Tensor, DType, Device};
use chrono::NaiveDate;
use polars::prelude::*;
use polars::error::PolarsResult;
 
struct Weather {
    date: NaiveDate,
    time: String,
    tind: f64,
    tmax: f64,
    tmin: f64,
    hind: f64,
    hmax: f64,
    hmin: f64,
    dpind: f64,
    dpmax: f64,
    dpmin: f64,
    pind: f64,
    pmax: f64,
    pmin: f64,
    wvel: f64,
    wraj: f64,
    rad: f64,
    rain: f64
}

fn to_tensor(_data: Vec<Weather>) -> Tensor {
    todo!()
}

fn format_date(df: DataFrame) -> PolarsResult<DataFrame> {
    let date = df.column("Data")?
        .cast(&DataType::Datetime(
                polars::datatypes::TimeUnit::Milliseconds,
                None))?
        .strftime("%Y-%m-%dT%H:%M:%SZ")?;

    println!("{:?}", date);
    Ok(df)
}

fn main() {
    let feature_columns = vec![
        "Temp. Ins. (C)", "Temp. Max. (C)", "Temp. Min. (C)",
        "Umi. Ins. (%)",	"Umi. Max. (%)",	"Umi. Min. (%)",
        "Pto Orvalho Ins. (C)",	"Pto Orvalho Max. (C)",	"Pto Orvalho Min. (C)",
        "Pressao Ins. (hPa)",	"Pressao Max. (hPa)",	"Pressao Min. (hPa)",
        "Vel. Vento (m/s)",	"Dir. Vento (m/s)",	"Raj. Vento (m/s)",
        "Radiacao (KJ/m²)"
    ];

    let label_column = "Chuva (mm)";

    let model_path = Path::new("./science/rf_pluvius.onnx");
    let data_path = Path::new("./data/real_data.csv");

    let model = candle_onnx::read_file(model_path);

    let df = LazyCsvReader::new(data_path)
        .with_has_header(true)
        .with_separator(b';')
        .finish()
        .unwrap();


    let data = format_date(df.collect().unwrap()).unwrap();
    //let data = df.collect().unwrap();
    println!("{}", data);
}
