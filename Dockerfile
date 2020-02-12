FROM utdemir/ghc-musl:v4-libgmp-ghc865 as haskell

RUN mkdir /app
WORKDIR /app

RUN cabal update
ADD ldabot.cabal .
RUN cabal build || true

ADD . .
RUN cabal new-install
RUN strip --strip-all /root/.cabal/bin/ldabot-prod

FROM alpine as upx

RUN apk add -u upx

COPY --from=haskell /root/.cabal/bin/ldabot-prod /app
RUN upx --best /app

FROM scratch

COPY --from=gcr.io/distroless/base /etc/ssl /etc/ssl
COPY --from=upx /app /app

ENTRYPOINT ["/app"]