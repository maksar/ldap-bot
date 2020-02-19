FROM alpine as upx

COPY .stack-work/docker/_home/.local/bin/ldabot-prod /app
RUN apk add -u upx
RUN upx --best --ultra-brute /app

FROM scratch

COPY --from=gcr.io/distroless/base /etc/ssl /etc/ssl
COPY --from=upx /app /app
COPY --from=fpco/stack-build:lts-14.25 /lib/x86_64-linux-gnu/ld-linux* /lib/x86_64-linux-gnu/libc.* /lib/x86_64-linux-gnu/libnss_dns.* /lib/x86_64-linux-gnu/libresolv.* /lib/

ENTRYPOINT ["/app"]