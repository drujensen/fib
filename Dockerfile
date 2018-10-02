FROM ubuntu:latest

USER root
ENV PATH="${PATH}:/root/.asdf/shims:/root/.asdf/bin"
RUN mkdir -p /app
WORKDIR /app

RUN apt-get update -qq && \
    apt-get upgrade -qq -y && \
    apt-get install -qq -y \
            gcc-6 \
            bison \
            libedit-dev \
            libreadline-dev \
            zlib1g-dev \ 
            libgdbm-dev \
            build-essential \
            automake \
            autoconf \
            libcurl4-openssl-dev \
            pkg-config \
            libpng-dev \
            libssl-dev \
            libyaml-dev \
            libxslt-dev \
            libffi-dev \
            libgmp3-dev \
            libtool \
            libncurses5-dev \
            libssh-dev \
            unixodbc-dev \
            git \
            curl \
            unzip && \
    apt-get clean -qq -y && \
    apt-get autoclean -qq -y && \
    apt-get autoremove -qq -y && \
    rm -rf /var/cache/debconf/*-old && \
    rm -rf /var/lib/apt/lists/* && \
    rm -rf /usr/share/doc/*

RUN git clone https://github.com/asdf-vm/asdf.git /root/.asdf

RUN asdf plugin-add php         && asdf install php         7.2.10
RUN asdf plugin-add crystal     && asdf install crystal     0.26.1
RUN asdf plugin-add dart https://github.com/baysao/asdf-dart.git \
                                && asdf install dart        2.0.0
RUN asdf plugin-add dotnet-core && asdf install dotnet-core 2.1.4
RUN asdf plugin-add dmd         && asdf install dmd         2.082.0
RUN asdf plugin-add erlang      && asdf install erlang      21.1
RUN asdf plugin-add elixir      && asdf install elixir      1.7.3
RUN asdf plugin-add elm         && asdf install elm         0.19.0
RUN asdf plugin-add golang      && asdf install golang      1.11
RUN asdf plugin-add haskell     && asdf install haskell     7.10.3
RUN asdf plugin-add java        && asdf install java        10.0.1
RUN asdf plugin-add julia       && asdf install julia       1.0.0
RUN asdf plugin-add ruby        && asdf install ruby        2.5.1
RUN asdf plugin-add nodejs https://github.com/asdf-vm/asdf-nodejs.git \
&& bash ~/.asdf/plugins/nodejs/bin/import-release-team-keyring \
                                && asdf install nodejs      8.12.0
RUN asdf plugin-add ocaml       && asdf install ocaml       4.07.0
RUN asdf plugin-add perl https://github.com/BeijingPM/asdf-perl.git \
                                && asdf install perl        5.28.0
RUN asdf plugin-add python      && asdf install python      2.7.15
RUN asdf plugin-add rust        && asdf install rust        1.29.1
RUN asdf plugin-add swift       && asdf install swift       4.1.2
RUN asdf plugin-add nim         && asdf install nim         v0.19.0
RUN asdf plugin-add clojure     && asdf install clojure     1.9.0

#RUN asdf plugin-add lua         && asdf install lua         5.4.0
#RUN asdf plugin-add R           && asdf install R           3.5.1
#RUN asdf plugin-add perl6       && asdf install perl6       2018.06
#RUN asdf plugin-add python3     && asdf install python3     3.5.6
#RUN asdf plugin-add tcl         && asdf install tcl         8.6

COPY . /app
CMD ./run.sh
