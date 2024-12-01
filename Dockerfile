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
            build-essential \
            ca-certificates \
            curl \
            file \
            git \
            jq \
            locales \
            pkg-config \
            software-properties-common \
            time \
            tzdata \
            unzip \
            vim \
            wget && \
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
            fp-compiler \
            gfortran \
            gnat \
            gnucobol \
            rakudo

# libraries
RUN DEBIAN_FRONTEND=noninteractive apt-get install -qq -y \
                libbz2-dev \
                libffi-dev \
                liblzma-dev \
                libncursesw5-dev \
                libreadline-dev \
                libssl-dev \
                libyaml-dev \
                libsqlite3-dev \
                libxml2-dev \
                libxmlsec1-dev \
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

RUN asdf plugin-add swift
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

RUN asdf plugin-add dotnet-core
RUN asdf install dotnet-core

RUN asdf plugin-add nodejs
RUN asdf install nodejs

# RUN asdf plugin-add bunjs https://github.com/drujensen/asdf-bunjs.git
# RUN asdf plugin-add bunjs
# RUN asdf install bunjs

RUN asdf plugin-add python
RUN asdf install python

RUN asdf plugin-add pypy https://github.com/drujensen/asdf-pypy.git
#RUN asdf plugin-add pypy
RUN asdf install pypy

RUN asdf plugin-add ruby
RUN asdf install ruby

#RUN asdf plugin add perl https://github.com/ouest/asdf-perl.git
RUN asdf plugin add perl
RUN asdf install perl

RUN asdf plugin-add php
RUN asdf install php

#RUN asdf plugin-add lua https://github.com/Stratus3D/asdf-lua.git
RUN asdf plugin-add lua
RUN asdf install lua

RUN asdf plugin-add luaJIT
RUN asdf install luaJIT

RUN asdf plugin-add R
RUN asdf install R

#RUN asdf plugin-add julia https://github.com/rkyleg/asdf-julia.git
RUN asdf plugin-add julia
RUN asdf install julia

RUN asdf plugin-add nim
RUN asdf install nim

#RUN asdf plugin-add janet https://github.com/Jakski/asdf-janet.git
RUN asdf plugin-add janet
RUN asdf install janet

RUN asdf plugin-add dart
RUN asdf install dart

RUN asdf plugin-add pony
RUN asdf install pony

RUN asdf plugin-add v
RUN asdf install v

RUN asdf plugin-add haskell
RUN asdf install haskell

RUN asdf plugin-add ocaml
RUN asdf install ocaml

RUN asdf plugin-add sbcl
RUN asdf install sbcl

RUN asdf plugin-add elm
RUN asdf install elm

#RUN asdf plugin-add elixir https://github.com/asdf-vm/asdf-elixir.git
RUN asdf plugin-add elixir
RUN asdf install elixir

#RUN asdf plugin-add erlang https://github.com/asdf-vm/asdf-erlang.git
RUN asdf plugin-add erlang
RUN asdf install erlang

#RUN asdf plugin-add dmd https://github.com/sylph01/asdf-dmd.git
RUN asdf plugin-add dmd
RUN asdf install dmd

# RUN asdf plugin-add guile https://github.com/indiebrain/asdf-guile.git
RUN asdf plugin-add guile
RUN asdf install guile

RUN asdf plugin-add tcl
RUN asdf install tcl

COPY . /root/app
CMD ["/bin/bash", "-c", "./run.sh"]
