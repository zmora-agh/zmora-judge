FROM ubuntu:16.04

RUN apt-get update \
  && apt-get install -qq -y build-essential cmake bison flex git pkg-config libcap-dev libprotobuf-dev protobuf-compiler netbase --fix-missing --no-install-recommends

RUN mkdir /opt/zmora-judge

WORKDIR /opt/zmora-judge

COPY nsjail ./nsjail
RUN cd nsjail && make && mv nsjail /usr/bin && cd .. && rm -rf nsjail

COPY scripts/* ./

COPY zmora-runner/src/* /usr/bin/
COPY bin/* /usr/bin/
RUN chmod +x /usr/bin/zmora*

STOPSIGNAL SIGTERM

CMD ["./docker-entrypoint.sh"]
