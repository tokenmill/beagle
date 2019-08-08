lint-code:
	docker run \
	-v $(PWD)/src:/src -v $(PWD)/spec:/spec -v $(PWD)/test:/test \
	--rm borkdude/clj-kondo clj-kondo \
	--lint src spec test

unit-test:
	clojure -A:test

build-graal-validator:
	docker build --target builder -f Dockerfile -t registry.gitlab.com/tokenmill/clj-luwak/dictionary-validator .
	docker rm build || true
	docker create --name build registry.gitlab.com/tokenmill/clj-luwak/dictionary-validator
	docker cp build:/usr/src/app/dictionary-validator dictionary-validator

build-graal-validator-docker:
	docker build --target validator -f Dockerfile -t registry.gitlab.com/tokenmill/clj-luwak/dictionary-validator .
