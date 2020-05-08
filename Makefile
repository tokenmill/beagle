lint-code:
	clojure -A:clj-kondo --config '{:output {:exclude-files ["java"]}}'

unit-test:
	clojure -A:runner:test -e :noisy

build-dictionary-validator:
	docker build --target builder -f Dockerfile -t tokenmill/beagle-dictionary-validator .
	docker rm build || true
	docker create --name build tokenmill/beagle-dictionary-validator
	docker cp build:/usr/src/app/dictionary-validator dictionary-validator

build-graal-validator-docker:
	docker build --target validator -f Dockerfile -t tokenmill/beagle-dictionary-validator .

recompile-java-interface:
	rm -rf classes
	mkdir classes
	clojure -e "(require 'beagle.java.annotation) (compile 'beagle.java.annotation) (compile 'beagle.java.java)"
