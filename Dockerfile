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

RUN sed -i "s/    ghc-options:/    cc-options: -static\n    ld-options: -static -pthread\n    ghc-options:\n    - -O2\n    - -static/g" package.yaml

RUN stack install --executable-stripping

FROM scratch

COPY --from=haskell /root/.local/bin/ldabot-exe /app

ENTRYPOINT ["/app"]