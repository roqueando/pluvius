use std::path::Path;
use candle_core::{Tensor, DType, Device};
use chrono::NaiveDate;
use polars::prelude::*;
 
//"Data";"Hora (UTC)";"Temp. Ins. (C)";"Temp. Max. (C)";"Temp. Min. (C)";"Umi. Ins. (%)";"Umi. Max. (%)";"Umi. Min. (%)";"Pto Orvalho Ins. (C)";"Pto Orvalho Max. (C)";"Pto Orvalho Min. (C)";"Pressao Ins. (hPa)";"Pressao Max. (hPa)";"Pressao Min. (hPa)";"Vel. Vento (m/s)";"Dir. Vento (m/s)";"Raj. Vento (m/s)";"Radiacao (KJ/m²)";"Chuva (mm)"

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

fn main() {
    let model_path = Path::new("./science/rf_pluvius.onnx");
    let model = candle_onnx::read_file(model_path);

    let lazy = LazyCsvReader::new("./data/real_data.csv")
        .with_has_header(true)
        .finish()
        .unwrap();

    println!("{:?}", lazy);
}
