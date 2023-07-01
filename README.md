# CL/I (SEFM2023 public version)
This repository contains the source code of the CL/I compiler along with a few models described in our SEFM2023 submission. It is publicly available on [Github](https://github.com/CAPRICA-Project/CL-I) and [Zenodo](https://zenodo.org/record/8181401).


# Setup
Several options are available to get CL/I up and running.


## Binary release (Docker, Linux, `x86_64`)
If you are running Linux with an `x86_64` architecture (AMD64) or the virtualization options for your platform are sufficiently efficient, you can download Docker binary releases for CL/I both on [Zenodo](https://zenodo.org/record/8181401) and [Docker Hub](https://hub.docker.com/r/bensmrs/cl-i). Please download Docker first. We DO NOT recommend this option if you are running macOS.


### Zenodo
You can download a `.zip` archive containing the CL/I source code and a Docker image exported as a `.tar.gz` file on [Zenodo](https://zenodo.org/record/8181401). After downloading the archive, go to its base path, for example by running:

```sh
unzip CL-I-SEFM2023.zip && cd CL-I
```

Load the docker image:

```sh
docker load < cl-i.tar.gz
```

and start a terminal:

```sh
docker run -it bensmrs/cl-i:sefm2023
```


### Docker Hub
The fastest way to spin up CL/I is to download the Docker image directly:

```sh
docker pull bensmrs/cl-i:sefm2023
```

and start a terminal:

```sh
docker run -it bensmrs/cl-i:sefm2023
```


## Source release

You can also compile CL/I from its sources. They are available on [Github](https://github.com/CAPRICA-Project/CL-I) and [Zenodo](https://zenodo.org/record/8181401). You can either build the Docker image or build the executable outside of Docker. For macOS users, we STRONGLY discourage you to try to build the Docker image.

First, download the source and go to the base directory.


### Docker image

> We have chosen not to fix the versions of the base Docker image and opam packages in our Dockerfile. This ensures that the image is always up to date, at the risk of breaking compatibility in the future. If this is the case, please use a binary release or build CL/I without Docker.

Build the Docker image:

```sh
docker build -t cl-i .
```

The build can take a while. If Z3 is still compiling after 30 minutes, it may be wise to choose another method. Then, start a terminal:

```sh
docker run -it cl-i
```


### Vanilla build

> You need to have `opam` and `git` installed on your machine. Additionally, you must install the GMP library (`libgmp-dev` on Debian, `gmp` on Homebrew) to compile the Z3 OCaml binding.

> On some systems, opam invokes `make -j1`, which can lead to a very slow compilation.

To build the CL/I binary, run

```sh
./make.sh
```

The build can take a while. If Z3 is still compiling after 30 minutes, it may be wise to opt for a binary release.


# Usage

From your terminal (inside Docker if you use Docker), invoking the CL/I compiler is done using:

```sh
./clc <model>
```

We provide two models in the `Models` directory, corresponding to the two examples of the SEFM2023 article. Model checking should terminate in less than a minute on a modern machine.

> The Proxmox VE model in `Models/Article-Proxmox.cl1` is partially obtained from a running Proxmox VE configuration with scripts not given here.


## Interpreting the results

The output of `clc` is rather crude. If you read `UNSAT`, it means that the model cannot be satisfied. If you read `SAT`, it means that the model has at least a solution. You can play with the examples to trigger `UNSAT` verdicts.