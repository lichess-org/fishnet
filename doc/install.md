# Installation methods

## Download binary

See [README.md](/README.md).

## From source

Assuming you have [a recent Rust toolchain](https://rustup.rs/), a C++ compiler, strip, and make installed:

```sh
git clone --recursive https://github.com/niklasf/fishnet.git
cd fishnet
cargo run --release -vv --
```

To update, do not forget `git submodule update` before building again:

```sh
git pull
git submodule update
cargo run --release -vv --
```

## AUR

For Arch Linux users, the following third-party packages are available on AUR:

* https://aur.archlinux.org/packages/fishnet-bin/
* https://aur.archlinux.org/packages/fishnet/
* https://aur.archlinux.org/packages/fishnet-git/

## Docker

```sh
docker run -it --name fishnet -e KEY=abcdef niklasf/fishnet:2
```

Per default, runs with `n-1` cores, alternatively, specify the number of cores to use with:

```sh
docker run -it --name fishnet -e KEY=abcdef -e CORES=n niklasf/fishnet:2
```

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
