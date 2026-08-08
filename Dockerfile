FROM docker.io/niklasf/fishnet-builder:12.5.0 AS builder
ENV RUSTC_WRAPPER=/usr/bin/sccache
ENV SCCACHE_DIR=/sccache
ENV SCCACHE_CACHE_SIZE=250M
WORKDIR /fishnet
ARG OFFICIAL_STOCKFISH_VERSION
ARG FAIRY_STOCKFISH_VERSION
ENV OFFICIAL_STOCKFISH_VERSION=$OFFICIAL_STOCKFISH_VERSION
ENV FAIRY_STOCKFISH_VERSION=$FAIRY_STOCKFISH_VERSION
COPY . .
RUN --mount=type=cache,target=/sccache sccache --show-stats && cargo auditable build --release -vv && sccache --show-stats

FROM docker.io/alpine:3
RUN apk --no-cache add bash
COPY --from=builder /fishnet/target/*-unknown-linux-musl/release/fishnet /fishnet
COPY scripts/docker-entrypoint.sh /docker-entrypoint.sh
CMD ["/docker-entrypoint.sh"]
