# fishnet: distributed Stockfish analysis for lichess.org

[![crates.io](https://img.shields.io/crates/v/fishnet.svg)](https://crates.io/crates/fishnet)
[![Docker](https://img.shields.io/docker/v/niklasf/fishnet?label=docker&sort=semver)](https://hub.docker.com/r/niklasf/fishnet)

## Installation

1. Request your personal fishnet key: https://lichess.org/get-fishnet

2. Install and run the fishnet client.

   **Download standalone binary**

   Select the binary for your platform
   [from the latest release](https://github.com/lichess-org/fishnet/releases)
   and run it.

   ```sh
   # After download:
   mv fishnet-x86_64-unknown-linux-musl fishnet
   chmod +x fishnet
   ./fishnet --auto-update
   ```

   **Useful commands**

   ```sh
   ./fishnet configure              # Rerun config dialog
   ./fishnet systemd --auto-update  # Print a .service file
   ./fishnet --help                 # List commands and options
   ```

   **Other installation methods:**
   [From source](/doc/install.md#from-source),
   [Docker](/doc/install.md#docker),
   [Kubernetes](/doc/install.md#kubernetes),
   [OpenShift](/doc/openshift/README.md)

3. Pick an update strategy.

   **Automatic updates**

   Run with `--auto-update` as recommended above.

   **Subscribe to release announcements**

   With a GitHub account, you can _watch_ this repository (can be set to
   release announcements only). See the top right corner on this page.

## Video introduction

Watch [@arex](https://lichess.org/@/arex) explain fishnet.

[![Video introduction](https://i3.ytimg.com/vi/C2SjcVbRfp0/maxresdefault.jpg)](https://youtu.be/C2SjcVbRfp0)

## FAQ

### Which engine does fishnet use?

fishnet uses [Stockfish](https://github.com/official-stockfish/Stockfish)
(hence the name) and [Fairy-Stockfish](https://github.com/ianfab/Fairy-Stockfish)
for chess variants.

### What are the requirements?

| Available for | 64-bit Intel and AMD        | ARMv8 / Silicon              |
| ------------- | --------------------------- | ---------------------------- |
| **Linux**     | `x86_64-unknown-linux-musl` | `aarch64-unknown-linux-musl` |
| **Windows**   | `x86_64-pc-windows-gnu.exe` |                              |
| **macOS**     | `x86_64-apple-darwin`       | `aarch64-apple-darwin`       |
| **FreeBSD**   | build from source           |                              |

- Needs Linux or an operating system from around 2019 or later
- Will max out the configured number of CPU cores
- Uses about 64 MiB RAM per CPU core
- A small amount of disk space
- Low-bandwidth network communication with Lichess servers
  (only outgoing HTTP requests, so probably no firewall configuration
  required, IPv4 not required)

### Is my CPU fast enough?

Almost all processors will be able to meet the requirement of ~2 meganodes in
6 seconds. Clients on the faster end will automatically be assigned
analysis jobs that have humans waiting for the result (the user queue, as
opposed to the system queue for slower clients).

### Why does my client remain idle?

Your client may remain idle if fishnet estimates that another client would
be able to complete the next batch more quickly, or if the client has been
configured to join the queue only if a backlog is building up. By standing
by, you're still contributing to reliability by providing redundancy, and also
to the _potential_ maximum throughput in case requests peak.

### What happens if I stop my client?

Feel free to turn your client on and off at any time. By default, the client
will try to finish any batches it has already started. On immediate shutdown,
the client tries to inform Lichess that batches should be reassigned.
If even that fails, Lichess will reassign the batches after a timeout.

### Will fishnet use my GPU?

No, Stockfish is a classical alpha-beta engine. The neural network evaluation
of Stockfish NNUE works efficiently on CPUs.

### Why do I need a key?

The key allows us to trace provided analysis back to your Lichess account.
You can use a single key to run multiple instances.

You do not need to request a key (nor our permission) to run private instances.

### Is fishnet secure?

To the best of our knowledge. All engine input is carefully validated.

Note that you implicitly trust the authors and the GitHub and Amazon S3
infrastructure when running with `--auto-update`. You can mitigate this by
running fishnet as an unprivileged user.

[`cargo-crev`](https://github.com/crev-dev/cargo-crev) is used to review the
trustworthiness of dependencies.
[`cargo-auditable`](https://github.com/rust-secure-code/cargo-auditable)
is used to embed dependency meta data into binaries.

### Is there a leaderboard of contributors?

No, sorry, not publicly. It would incentivize gaming the metrics.

### Can I autoscale fishnet in the cloud?

There is currently no ready-made solution, but
[an API for monitoring the job queue status](/doc/protocol.md#status)
is provided.

## Protocol

![Sequence diagram](/doc/sequence-diagram.png)

See [protocol.md](/doc/protocol.md) for details.
Also supports [`SSLKEYLOGFILE`](https://wiki.wireshark.org/TLS#tls-decryption) for inspection at runtime.

## License

fishnet is licensed under the GPLv3+. See LICENSE.txt or `./fishnet license`
for the full license text.
