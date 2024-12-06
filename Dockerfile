FROM amd64/ubuntu:24.04

USER root
RUN mkdir -p /root/app
WORKDIR /root/app

# prerequisite packages
RUN apt-get update -qq && \
    apt-get upgrade -qq -y && \
    DEBIAN_FRONTEND=noninteractive apt-get install -qq -y \
            apt-transport-https \
            autoconf \
            automake \
            binutils \
            bison \
            bubblewrap \
            build-essential \
            ca-certificates \
            curl \
            file \
            git \
            gnupg2 \
            jq \
            locales \
            pkg-config \
            re2c \
            software-properties-common \
            tar \
            time \
            tzdata \
            unzip \
            vim \
            wget \
            xorg-dev && \
    apt-get clean -qq -y && \
    apt-get autoclean -qq -y && \
    apt-get autoremove -qq -y

# locales
RUN locale-gen en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

# apt languages
RUN DEBIAN_FRONTEND=noninteractive apt-get install -qq -y \
            llvm \
            clang \
            fp-compiler \
            gfortran \
            gnat \
            gnucobol \
            guile-3.0 \
            rakudo

# libraries
RUN DEBIAN_FRONTEND=noninteractive apt-get install -qq -y \
            libbz2-dev \
            libffi-dev \
            liblzma-dev \
            libncurses5-dev \
            libreadline-dev \
            libssl-dev \
            libyaml-dev \
            libsqlite3-dev \
            libxml2-dev \
            libxmlsec1-dev \
            libc6-dev \
            libz3-dev \
            libgd-dev \
            libpcre2-dev \
            libpcre3-dev \
            libonig-dev \
            libpq-dev \
            libedit-dev \
            libgdbm-dev \
            libcurl4-openssl-dev \
            libunistring-dev \
            libgc-dev \
            libpng-dev \
            libxslt-dev \
            libgmp3-dev \
            libtool \
            libncurses-dev \
            libssh-dev \
            libzip-dev \
            libevent-dev \
            libicu-dev \
            libglu1-mesa-dev \
            unixodbc-dev \
            zlib1g-dev && \
    apt-get clean -qq -y && \
    apt-get autoclean -qq -y && \
    apt-get autoremove -qq -y

# asdf languages
RUN git clone https://github.com/asdf-vm/asdf.git /root/.asdf
RUN chmod 755 /root/.asdf/asdf.sh
RUN echo "/root/.asdf/asdf.sh" >> /etc/bash.bashrc

# Add asdf and above languages to PATH
ENV PATH="${PATH}:/root/.asdf/shims:/root/.asdf/bin"

COPY .tool-versions /root/app/.

RUN asdf plugin-add golang
RUN asdf install golang

RUN asdf plugin-add rust
RUN asdf install rust

RUN asdf plugin-add swift https://github.com/drujensen/asdf-swift
RUN asdf install swift

RUN asdf plugin-add crystal
RUN asdf install crystal

RUN asdf plugin-add java
RUN asdf install java

RUN asdf plugin-add kotlin
RUN asdf install kotlin

RUN asdf plugin-add clojure
RUN asdf install clojure

RUN asdf plugin-add scala
RUN asdf install scala

RUN asdf plugin-add dotnet
RUN asdf install dotnet

RUN asdf plugin-add nodejs
RUN asdf install nodejs

RUN asdf plugin-add bun
RUN asdf install bun

RUN asdf plugin-add python
RUN asdf install python

RUN asdf plugin-add ruby
RUN asdf install ruby

RUN asdf plugin add perl
RUN asdf install perl

RUN asdf plugin-add php
RUN asdf install php

RUN asdf plugin-add lua
RUN asdf install lua

RUN asdf plugin-add nim
RUN asdf install nim

RUN asdf plugin-add dart
RUN asdf install dart

RUN asdf plugin-add haskell
RUN asdf install haskell

RUN asdf plugin-add ocaml
RUN asdf install ocaml

RUN asdf plugin-add sbcl
RUN asdf install sbcl

RUN asdf plugin-add elm
RUN asdf install elm

RUN asdf plugin-add julia
RUN asdf install julia

RUN asdf plugin-add elixir
RUN asdf install elixir

RUN asdf plugin-add erlang
RUN asdf install erlang

RUN asdf plugin-add groovy
RUN asdf install groovy

RUN asdf plugin-add r https://github.com/asdf-community/asdf-r.git
RUN asdf install r

RUN asdf plugin-add dmd https://github.com/sylph01/asdf-dmd.git
RUN asdf install dmd

RUN asdf plugin add powershell-core https://github.com/daveneeley/asdf-powershell-core
RUN asdf install powershell-core

RUN asdf plugin add zig https://github.com/zigcc/asdf-zig
RUN asdf install zig

# RUN asdf plugin add mojo https://github.com/alvesgabriel/asdf-mojo.git
# RUN asdf install mojo

# broken
# RUN asdf plugin-add v
# RUN asdf install v

# RUN asdf plugin-add pony https://github.com/enilsen16/asdf-pony.git
# RUN asdf install pony
RUN SHELL=/bin/sh sh -c "$(curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/ponylang/ponyup/latest-release/ponyup-init.sh)"
RUN /root/.local/share/ponyup/bin/ponyup update ponyc release

# needs apt install ninja-build and pip3 install meson
# RUN asdf plugin-add janet https://github.com/Jakski/asdf-janet.git
# RUN asdf install janet
RUN wget -q https://github.com/janet-lang/janet/releases/download/v1.36.0/janet-v1.36.0-linux-x64.tar.gz
RUN tar xzf janet-v1.36.0-linux-x64.tar.gz
RUN mv janet-v1.36.0-linux /usr/share/janet
RUN rm janet-v1.36.0-linux-x64.tar.gz

# RUN asdf plugin-add luajit https://github.com/smashedtoatoms/asdf-luaJIT
# RUN asdf install luajit
RUN apt-get install -qq -y luajit

# RUN asdf plugin-add tcl https://github.com/mdekstrand/asdf-tcl
# RUN asdf install tcl
RUN apt-get install -qq -y tcl

ENV PATH="${PATH}:/root/.asdf/shims:/root/.asdf/bin:/usr/share/janet:/usr/share/qb64:/usr/share/dlang/ldc-1.28.1/bin:/root/.local/share/ponyup/bin:/usr/share/v"

COPY . /root/app
CMD ["/bin/bash", "-c", "./run.sh"]
