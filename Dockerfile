FROM docker.io/niklasf/fishnet-builder:12.5.0 AS builder
ENV RUSTC_WRAPPER=/usr/bin/sccache
ENV SCCACHE_DIR=/sccache
ENV SCCACHE_CACHE_SIZE=250M
WORKDIR /fishnet
COPY . .
RUN --mount=type=cache,target=/sccache sccache --show-stats && cargo auditable build --release -vv && sccache --show-stats

FROM docker.io/alpine:3
RUN apk --no-cache add bash \
&& addgroup -S -g 10001 fishnet \
&& adduser -S -D -H \
    -u 10001 \
    -G fishnet \
    -s /sbin/nologin \
    fishnet
COPY --from=builder /fishnet/target/*-unknown-linux-musl/release/fishnet /fishnet
COPY scripts/docker-entrypoint.sh /docker-entrypoint.sh
USER 10001:10001
CMD ["/docker-entrypoint.sh"]
