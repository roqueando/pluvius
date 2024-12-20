use mongodb::{Client, options::{ClientOptions, ResolverConfig}};

#[tokio::main]
async fn main() {
    let client_uri = "mongodb+srv://pluvius:local_password@localhost:27017/feature_store";
    let options = ClientOptions::parse(client_uri).await.unwrap();
    let client = Client::with_options(options).unwrap();

    let database = client.database("feature_store");
    println!("opa meu consagrated");
}
