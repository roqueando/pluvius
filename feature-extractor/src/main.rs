use polars::prelude::*;

fn main() {
    let df = CsvReadOptions::default()
        .with_infer_schema_length(Some(100))
        .with_has_header(true)
        .try_into_reader_with_file_path(Some("./data/raw/2019.csv".into()))
        .unwrap()
        .finish()
        .unwrap();

    println!("{:?}", df);
}
