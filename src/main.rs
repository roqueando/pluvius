fn main() {
    let model_path = "../science/rf_pluvius.onnx";
    let model = candle_onnx::read_file(model_path);

    match model {
        Ok(m) => println!("{:?}", m),
        Err(err) => panic!("{}", err),
    }

}
