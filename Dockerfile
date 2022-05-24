FROM alpine:3.16
MAINTAINER Milt Reder <milt@yetanalytics.com>

ADD target/bundle /bundle

RUN apk update \
        && apk upgrade \
        && apk add ca-certificates \
        && update-ca-certificates \
        && apk add --no-cache openjdk11 \
        && rm -rf /var/cache/apk/*

WORKDIR /bundle
EXPOSE 9090
ENTRYPOINT ["bin/run.sh"]
CMD ["-h"]
