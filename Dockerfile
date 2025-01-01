FROM haskell:9.4.8-slim as builder

WORKDIR /build

COPY ./pluvius.cabal cabal.project.freeze ./
COPY ./app/ ./app/
COPY ./src/ ./src/
COPY ./test/ ./test/

RUN apt-get update && apt-get install -y zlib1g-dev pkg-config netbase

ENV CABAL_DIR=/build
RUN cabal update && cabal build --dependencies-only
RUN cabal install feature-extractor --installdir=./bin


FROM debian:bullseye-slim

RUN apt-get update && apt-get install -y zlib1g-dev pkg-config netbase

WORKDIR /app
COPY --from=builder /build/bin/feature-extractor .
CMD ["./feature-extractor"]
