FROM debian:buster-slim AS downloader

ARG VERSION=latest
ARG ARCH=x86_64

RUN apt-get update \
  && apt-get install -y \
    curl jq \
  && curl -sL $(curl -sL https://api.github.com/repos/niklasf/fishnet/releases/latest | jq -r '.assets[].browser_download_url' | grep "$ARCH.*linux") -o /fishnet \
  && chmod +x /fishnet

# Not using alpine due to https://andygrove.io/2020/05/why-musl-extremely-slow/
FROM debian:buster-slim
COPY --from=downloader /fishnet /fishnet

ENV CORES=auto

CMD /fishnet \
  --no-conf \
  --auto-update \
  --cores $CORES \
  --key $FISHNET_API_KEY
