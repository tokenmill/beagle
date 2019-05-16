lint:
	clojure -A:kibit
	clojure -A:eastwood

unit-test:
	clojure -A:test

build-graal-validator:
	docker build --target builder -f Dockerfile -t registry.gitlab.com/tokenmill/clj-luwak/dictionary-validator .
	docker rm build || true
	docker create --name build registry.gitlab.com/tokenmill/clj-luwak/dictionary-validator
	docker cp build:/usr/src/app/dictionary-validator dictionary-validator

build-graal-validator-docker:
	docker build --target validator -f Dockerfile -t registry.gitlab.com/tokenmill/clj-luwak/dictionary-validator .
