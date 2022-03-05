FROM rust:1.59.0 AS builder
WORKDIR /fishnet
COPY . .
RUN (git submodule update --init --recursive || true) && cargo build --release -vv

# Not using alpine due to https://andygrove.io/2020/05/why-musl-extremely-slow/
FROM debian:11-slim
COPY --from=builder /fishnet/target/release/fishnet /fishnet
COPY docker-entrypoint.sh /docker-entrypoint.sh
CMD ["/docker-entrypoint.sh"]
