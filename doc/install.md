# Installation methods

## Download binary

See [README.md](/README.md).

## From source

Requires [a recent Rust toolchain](https://rustup.rs/), a C++ compiler
(`clang` recommended), `strip`, and `make`.

`sccache` recommended for repeated builds.

```sh
git clone --recursive https://github.com/lichess-org/fishnet.git
cd fishnet
RUSTC_WRAPPER=sccache RUSTFLAGS="-C target-cpu=native" cargo run --release -vv --
```

To update, do not forget `git submodule update` before building again:

```sh
git pull
git submodule update
RUSTC_WRAPPER=sccache RUSTFLAGS="-C target-cpu=native" cargo run --release -vv --
```

Optional environment variables to configure Stockfish builds:

* `COMP`: `clang`, `gcc` (default on Linux), `mingw` (default on Windows)
* `CXX`
* `CXXFLAGS`
* `DEPENDFLAGS`
* `LDFLAGS`
* `MAKE`
* `SDE_PATH`

## Docker

```sh
docker run -it \
  --name fishnet \
  --read-only \
  --tmpfs /tmp:rw,nosuid,nodev,exec \
  --cap-drop=ALL \
  --security-opt="no-new-privileges=true" \
  -e KEY=abcdef \
  niklasf/fishnet:2
```

Per default, runs with `n-1` cores, alternatively, specify the number of cores to use with:

```sh
docker run -it \
  --name fishnet \
  --read-only \
  --tmpfs /tmp:rw,nosuid,nodev,exec \
  --cap-drop=ALL \
  --security-opt="no-new-privileges=true" \
  -e KEY=abcdef \
  -e CORES=n \
  niklasf/fishnet:2
```

For the full list of configurable environment variables, see [docker-entrypoint.sh](/scripts/docker-entrypoint.sh).

To update, since we named the image `fishnet`:

```sh
docker rm fishnet
docker pull niklasf/fishnet:2
# Run again as described above
```

## Docker Compose

Create a `compose.yml` file:

```yaml
services:
  fishnet:
    image: niklasf/fishnet:2
    environment:
      - KEY
      - CORES
      - ENDPOINT
      - USER_BACKLOG
      - SYSTEM_BACKLOG
      - MAX_BACKOFF
      - CPU_PRIORITY
    read_only: true
    tmpfs:
      - /tmp:rw,nosuid,nodev,exec
    cap_drop:
      - ALL
    security_opt:
      - no-new-privileges:true
```

And a corresponding `.env` file:

```
KEY=abcdef
```

Then:

```sh
docker compose up -d
```

## Kubernetes

Create `fishnet.yaml` as follows and edit `KEY`, `replicas`, and `limits`:

```yaml
apiVersion: v1
kind: Namespace
metadata:
  name: fishnet
  labels:
    pod-security.kubernetes.io/enforce: restricted
---
apiVersion: v1
kind: Secret
metadata:
  name: fishnet
  namespace: fishnet
stringData:
  KEY: abcdef
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: fishnet
  namespace: fishnet
spec:
  replicas: 3
  selector:
    matchLabels:
      app: fishnet
  strategy:
    rollingUpdate:
      maxSurge: 0
      maxUnavailable: 1
  template:
    metadata:
      labels:
        app: fishnet
    spec:
      automountServiceAccountToken: false
      securityContext:
        runAsNonRoot: true
        runAsUser: 10001
        runAsGroup: 10001
        seccompProfile:
          type: RuntimeDefault
      containers:
        - name: fishnet
          image: niklasf/fishnet:2
          imagePullPolicy: Always
          env:
            - name: CORES
              value: all  # Use full limits.cpu
          envFrom:
            - secretRef:
                name: fishnet
          resources:
            limits:
              cpu: "4"
              memory: 1Gi  # Scale with number of cores (conservatively 256Mi per core)
          securityContext:
            allowPrivilegeEscalation: false
            readOnlyRootFilesystem: true
            capabilities:
              drop: ["ALL"]
          volumeMounts:
            - name: tmp
              mountPath: /tmp
      volumes:
        - name: tmp
          emptyDir: {}
```

Then:

```sh
kubectl apply -f fishnet.yaml
```

To view logs:

```sh
kubectl logs deployment/fishnet -n fishnet
```

To update, since the image pull policy is set to `Always`:

```sh
kubectl rollout restart deployment/fishnet -n fishnet
```
