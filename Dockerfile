FROM oracle/graalvm-ce:19.1.1 as builder
RUN gu install native-image

ENV GRAALVM_HOME=$JAVA_HOME

RUN curl -O https://download.clojure.org/install/linux-install-1.10.0.442.sh
RUN chmod +x linux-install-1.10.0.442.sh
RUN ./linux-install-1.10.0.442.sh

RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app

COPY deps.edn /usr/src/app/
RUN clojure -R:graal-dictionary-validator
COPY . /usr/src/app

RUN clojure -A:graal-dictionary-validator

RUN cp target/app dictionary-validator
RUN chmod 755 dictionary-validator

FROM alpine:3.9.4 as validator

WORKDIR /opt
COPY --from=builder /usr/src/app/dictionary-validator /usr/local/bin/dictionary-validator

CMD ["dictionary-validator"]
