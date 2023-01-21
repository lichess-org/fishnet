FROM niklasf/fishnet-builder:4 AS builder
WORKDIR /fishnet
COPY . .
RUN cargo auditable build --release -vv

FROM alpine:3
RUN apk --no-cache add bash
COPY --from=builder /fishnet/target/x86_64-unknown-linux-musl/release/fishnet /fishnet
COPY docker-entrypoint.sh /docker-entrypoint.sh
CMD ["/docker-entrypoint.sh"]

# Set permissions for OpenShift, since there is an arbitrary user ID assigned at runtime. 
# But this user is member of group ID 0, so we can change permissions to allow working.
RUN touch /.fishnet-stats && \
    chgrp -R 0 /.fishnet-stats && \
    chmod -R 775 /.fishnet-stats
