FROM ubuntu:20.04

RUN apt-get -y update && \
    apt-get -y install curl && \
    apt-get -y install cabal-install && \
    apt-get -y install z3 && \
    apt-get -y install git && \
    rm -rf /var/cache/apt/lists

RUN apt-get -y install ghc
RUN apt-get -y install zlib1g-dev libsqlite3-dev

RUN git clone https://github.com/kadena-io/txg.git

WORKDIR /txg

RUN cabal update
RUN cabal new-build
RUN cp ./dist-newstyle/build/x86_64-linux/ghc-8.6.5/txg-1.0.0/x/mempool-p2p-tester/build/mempool-p2p-tester/mempool-p2p-tester .
RUN strip -s mempool-p2p-tester

ENTRYPOINT ["/txg/mempool-p2p-tester"]
CMD ["--help"]
