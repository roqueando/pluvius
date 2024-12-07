FROM haskell:9.4.8-slim as builder

WORKDIR /build

COPY ./pluvius.cabal .
COPY ./app ./app
COPY ./src ./src

ENV CABAL_DIR=/build
RUN cabal update && cabal build --dependencies-only
RUN cabal build
RUN cabal install --installdir=.

FROM debian:bullseye-slim

WORKDIR /app
COPY --from=builder /build/feature-extractor .
CMD ["./feature-extractor"]
