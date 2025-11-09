use axum::{
    Json, Router,
    response::Html,
    routing::{get, post},
};
use serde::{Deserialize, Serialize};
use std::net::SocketAddr;
use tokio::net::TcpListener;

#[derive(Deserialize)]
struct Req {
    text: String,
}
#[derive(Serialize)]
struct Resp {
    result: String,
}

static INDEX_HTML: &str = include_str!("../index.html");

#[tokio::main]
async fn main() {
    let app = Router::new()
        .route("/", get(|| async { Html(INDEX_HTML) }))
        .route("/run", post(run));

    let addr = SocketAddr::from(([127, 0, 0, 1], 8080));
    println!("→ open http://{}", addr);
    axum::serve(TcpListener::bind(addr).await.unwrap(), app)
        .await
        .unwrap();
}

async fn run(Json(Req { text }): Json<Req>) -> Json<Resp> {
    let parsed = match lambda_stlc_module::parser::parse(&text) {
        Ok(ok) => ok,
        Err(err) => {
            let result = format!("Parse error\n{}", err);
            return Json(Resp { result });
        }
    };
    let result = format!("{}", parsed);
    Json(Resp { result })
}
