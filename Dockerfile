FROM ubuntu:18.04

USER root
ENV PATH="${PATH}:/root/.asdf/shims:/root/.asdf/bin"
RUN mkdir -p /root/app
WORKDIR /root/app

# prerequisite packages
RUN apt-get update -qq && \
    apt-get upgrade -qq -y && \
    apt-get install -qq -y \
            automake \
            autoconf \
            pkg-config \
            gcc-6 \
            build-essential \
            bison \
            libedit-dev \
            libreadline-dev \
            zlib1g-dev \
            libgdbm-dev \
            libcurl4-openssl-dev \
            libpng-dev \
            libssl-dev \
            libyaml-dev \
            libxslt-dev \
            libffi-dev \
            libgmp3-dev \
            libtool \
            libncurses-dev \
            libssh-dev \
            unixodbc-dev \
            libzip-dev \
            libbz2-dev \
            libevent-dev \
            libicu-dev \
            liblzma-dev \
            apt-transport-https \
            ca-certificates \
            gnupg2 \
            software-properties-common \
            bubblewrap \
            xorg-dev \
            vim \
            git \
            curl \
            jq \
            wget \
            time \
            unzip && \
    apt-get clean -qq -y && \
    apt-get autoclean -qq -y && \
    apt-get autoremove -qq -y && \
    rm -rf /var/cache/debconf/*-old && \
    rm -rf /var/lib/apt/lists/* && \
    rm -rf /usr/share/doc/*

# D
RUN curl -fsS https://dlang.org/install.sh | bash -s ldc

# Mono
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
RUN echo "deb https://download.mono-project.com/repo/ubuntu stable-bionic main" | tee /etc/apt/sources.list.d/mono-official-stable.list
RUN apt update
RUN apt-get install -qq -y mono-devel

# Pony
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys E04F0923 B3B48BDA
RUN add-apt-repository "deb https://dl.bintray.com/pony-language/ponylang-debian  $(lsb_release -cs) main"
RUN apt-get install -qq -y ponyc

#Swift
RUN wget -q -O - https://swift.org/keys/all-keys.asc | gpg --import -
RUN wget https://swift.org/builds/swift-5.2.1-release/ubuntu1804/swift-5.2.1-RELEASE/swift-5.2.1-RELEASE-ubuntu18.04.tar.gz
RUN tar xzf swift-5.2.1-RELEASE-ubuntu18.04.tar.gz
RUN mv swift-5.2.1-RELEASE-ubuntu18.04 /usr/share/swift
RUN rm swift-5.2.1-RELEASE-ubuntu18.04.tar.gz
RUN echo "export PATH=/usr/share/swift/usr/bin:$PATH" >> ~/.bashrc

#Powershell
RUN wget -q https://packages.microsoft.com/config/ubuntu/18.04/packages-microsoft-prod.deb
RUN dpkg -i packages-microsoft-prod.deb
RUN apt-get update
RUN add-apt-repository universe
RUN apt-get install -y powershell

# V
RUN wget -q https://github.com/vlang/v/releases/latest/download/v_linux.zip
RUN mkdir /usr/share/v_linux
RUN mv v_linux.zip /usr/share/v_linux/.
RUN cd /usr/share/v_linux && unzip v_linux.zip && rm v_linux.zip
RUN echo "export PATH=/user/share/v_linux:$PATH" >> ~/.bashrc

# apt languages
RUN DEBIAN_FRONTEND=noninteractive apt-get install -qq -y \
            cython \
            fp-compiler \
            gfortran \
            ocaml \
            perl6 \
            sbcl \
            tcl \
            guile-2.2 \
            luajit

# asdf languages
RUN git clone https://github.com/asdf-vm/asdf.git /root/.asdf --branch v0.7.8
RUN echo "/root/.asdf/asdf.sh" >> ~/.bashrc
RUN asdf update
RUN chmod 755 /root/.asdf/asdf.sh

RUN asdf plugin-add java
RUN asdf install java adopt-openjdk-14+36
RUN asdf global java adopt-openjdk-14+36

RUN asdf plugin-add crystal
RUN asdf install crystal 0.34.0

RUN asdf plugin-add clojure
RUN asdf install clojure 1.10.1

RUN asdf plugin-add dart https://github.com/baysao/asdf-dart.git
RUN asdf install dart 2.7.2

RUN asdf plugin-add dotnet-core
RUN asdf install dotnet-core 3.1.201

RUN asdf plugin-add elixir
RUN asdf install elixir 1.10.0

RUN asdf plugin-add elm
RUN asdf install elm 0.19.1

RUN asdf plugin-add erlang
RUN asdf install erlang 22.3.1

RUN asdf plugin-add golang
RUN asdf install golang 1.14.1

RUN asdf plugin-add haskell
RUN asdf install haskell 8.10.1

RUN asdf plugin-add julia
#RUN asdf install julia 1.4.0

RUN asdf plugin-add lua
#RUN asdf install lua 5.4.0

RUN asdf plugin-add nim
RUN asdf install nim v1.2.0

RUN asdf plugin-add nodejs https://github.com/asdf-vm/asdf-nodejs.git \
&& bash ~/.asdf/plugins/nodejs/bin/import-release-team-keyring
RUN asdf install nodejs 12.16.1

RUN asdf plugin-add perl https://github.com/BeijingPM/asdf-perl.git
RUN asdf install perl 5.30.2

RUN asdf plugin-add php
#RUN asdf install php 7.4.4

RUN asdf plugin-add python
RUN asdf install python 3.8.2

RUN asdf plugin-add R
RUN asdf install R 3.6.3

RUN asdf plugin-add ruby
RUN asdf install ruby 2.7.1

RUN asdf plugin-add rust
RUN asdf install rust 1.42.0

COPY . /root/app
CMD ./run.sh
