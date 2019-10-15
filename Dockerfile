FROM oracle/graalvm-ce:19.2.0.1 as builder
RUN gu install native-image

ENV GRAALVM_HOME=$JAVA_HOME

RUN curl -O https://download.clojure.org/install/linux-install-1.10.1.469.sh
RUN chmod +x linux-install-1.10.1.469.sh
RUN ./linux-install-1.10.1.469.sh

RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app

COPY deps.edn /usr/src/app/
RUN clojure -R:native-image
COPY . /usr/src/app

RUN clojure -A:native-image

RUN chmod 755 dictionary-validator

FROM alpine:3.9.4 as validator

WORKDIR /opt
COPY --from=builder /usr/src/app/dictionary-validator /usr/local/bin/dictionary-validator

CMD ["dictionary-validator"]
