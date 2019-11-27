FROM openjdk:12
MAINTAINER Milt Reder <milt@yetanalytics.com>

ADD target/bundle /bundle
WORKDIR /bundle
ENTRYPOINT ["bin/run.sh"]
CMD ["-h"]
