FROM openjdk:12
MAINTAINER Milt Reder <milt@yetanalytics.com>

ADD target/bundle /bundle
WORKDIR /bundle
EXPOSE 9090
ENTRYPOINT ["bin/run.sh"]
CMD ["-h"]
