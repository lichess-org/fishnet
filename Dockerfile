FROM docker.io/niklasf/fishnet-builder:12.5.0 AS builder
ENV RUSTC_WRAPPER=/usr/bin/sccache
ENV SCCACHE_DIR=/sccache
ENV SCCACHE_CACHE_SIZE=250M
WORKDIR /fishnet
ARG OFFICIAL_STOCKFISH_HASH
ARG OFFICIAL_STOCKFISH_DATE
ARG FAIRY_STOCKFISH_HASH
ARG FAIRY_STOCKFISH_DATE
ENV OFFICIAL_STOCKFISH_HASH=$OFFICIAL_STOCKFISH_HASH
ENV OFFICIAL_STOCKFISH_DATE=$OFFICIAL_STOCKFISH_DATE
ENV FAIRY_STOCKFISH_HASH=$FAIRY_STOCKFISH_HASH
ENV FAIRY_STOCKFISH_DATE=$FAIRY_STOCKFISH_DATE
COPY . .
RUN --mount=type=cache,target=/sccache sccache --show-stats && cargo auditable build --release -vv && sccache --show-stats

FROM docker.io/alpine:3
RUN apk --no-cache add bash
COPY --from=builder /fishnet/target/*-unknown-linux-musl/release/fishnet /fishnet
COPY scripts/docker-entrypoint.sh /docker-entrypoint.sh
CMD ["/docker-entrypoint.sh"]
