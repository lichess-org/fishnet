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

Optional environment variables:

* `COMP`: `clang` (default), `gcc`, `mingw` (default on Windows)
* `CXX`
* `CXXFLAGS`
* `DEPENDFLAGS`
* `LDFLAGS`
* `MAKE`
* `SDE_PATH`

## Docker

```sh
docker run -it --name fishnet -e KEY=abcdef niklasf/fishnet:2
```

Per default, runs with `n-1` cores, alternatively, specify the number of cores to use with:

```sh
docker run -it --name fishnet -e KEY=abcdef -e CORES=n niklasf/fishnet:2
```

For the full list of configurable environment variables, see [docker-entrypoint.sh](/scripts/docker-entrypoint.sh).

To update, since we named the image `fishnet`:

```sh
docker rm fishnet
docker pull niklasf/fishnet:2
docker run -it --name fishnet -e KEY=abcdef niklasf/fishnet:2
```

## Kubernetes

Create `fishnet.yaml` as follows and edit `fishnet-private-key`:

```yaml
apiVersion: v1
kind: Namespace
metadata:
  name: fishnet
---
apiVersion: v1
kind: Pod
metadata:
  name: fishnet-pod
  namespace: fishnet
spec:
  containers:
    - name: fishnet-pod
      image: niklasf/fishnet:2
      imagePullPolicy: Always
      env:
        # - name: CORES
        #   valueFrom:
        #     configMapKeyRef:
        #       name: fishnet-config
        #       key: cores
        - name: KEY
          valueFrom:
            secretKeyRef:
              name: lichess
              key: fishnet-private-key
  restartPolicy: Always
---
apiVersion: v1
kind: Secret
metadata:
  name: lichess
  namespace: fishnet
data:
  fishnet-private-key: <UPDATE here with your fishnet private key as BASE64 encoded string>
# ---
# apiVersion: v1
# kind: ConfigMap
# metadata:
#   name: fishnet-config
#   namespace: fishnet
# data:
#   cores: "4"
```

Uncomment the `configMap` to change the number of cores used.

To view logs:

```sh
kubectl logs fishnet-pod -n=fishnet
```

Delete to update, since the image pull policy is set to `Always`:

```sh
kubectl delete pod fishnet-pod -n=fishnet
kubectl apply -f fishnet.yaml
```
