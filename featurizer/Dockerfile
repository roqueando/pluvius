FROM haskell:9.4.8-slim as builder

WORKDIR /build

COPY ./featurizer .

ENV CABAL_DIR=/build
RUN cabal update && cabal build --dependencies-only
RUN cabal build
RUN cabal install

FROM debian:bullseye-slim

WORKDIR /app
COPY --from=builder /build/.cabal/featurizer .
CMD ["./featurizer"]
