use mongodb::{Client, options::{ClientOptions, ResolverConfig}};

fn main() {
    let client_uri = "mongodb+srv://pluvius:local_password@localhost:27017/feature_store";
    let options = ClientOptions::parse_with_resolver_config(&client_uri, ResolverConfig);

    println!("opa meu consagrated");
}
