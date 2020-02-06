FROM haskell:8.6.5 as haskell

RUN mkdir /app
WORKDIR /app

ADD stack.yaml .
ADD stack.yaml.lock .
ADD package.yaml .

RUN mkdir src
RUN mkdir app
RUN mkdir test

RUN stack setup
RUN stack build || true

ADD . .

RUN stack install

FROM gcr.io/distroless/base
COPY --from=haskell /lib/x86_64-linux-gnu/libz* /lib/x86_64-linux-gnu/
COPY --from=haskell /usr/lib/x86_64-linux-gnu/libgmp* /usr/lib/x86_64-linux-gnu/

COPY --from=haskell /root/.local/bin/ldabot-exe /app

ENTRYPOINT ["/app"]