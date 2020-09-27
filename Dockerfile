FROM alpine as upx

RUN apk add -u upx ca-certificates && update-ca-certificates
COPY .stack-work/docker/_home/.local/bin/ldabot-prod /app
RUN upx --best /app

FROM scratch

COPY --from=upx /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/
COPY --from=upx /app /app

ENTRYPOINT ["/app"]