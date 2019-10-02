lint-code:
	clojure -A:clj-kondo

unit-test:
	clojure -A:test

build-graal-validator:
	docker build --target builder -f Dockerfile -t registry.gitlab.com/tokenmill/clj-luwak/dictionary-validator .
	docker rm build || true
	docker create --name build registry.gitlab.com/tokenmill/clj-luwak/dictionary-validator
	docker cp build:/usr/src/app/dictionary-validator dictionary-validator

build-graal-validator-docker:
	docker build --target validator -f Dockerfile -t registry.gitlab.com/tokenmill/clj-luwak/dictionary-validator .

recompile-java-interface:
	rm -rf classes
	mkdir classes
	clojure -e "(require 'beagle.java.annotation) (compile 'beagle.java.annotation) (compile 'beagle.java.java)"
