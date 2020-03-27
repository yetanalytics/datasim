.PHONY: clean bundle test-cli test-cli-comprehensive test-cli-output test-unit ci server test-bundle-output

GROUP_ID ?= com.yetanalytics
ARTIFACT_ID ?= datasim
VERSION ?= 0.1.0-SNAPSHOT
MAIN_NS ?= com.yetanalytics.datasim.main

clean:
	rm -rf target

target/bundle:
	clojure -A:build $(GROUP_ID) $(ARTIFACT_ID) $(VERSION)
	chmod u+x target/$(ARTIFACT_ID)-$(VERSION)/bin/run.sh
	chmod u+x target/$(ARTIFACT_ID)-$(VERSION)/bin/server.sh
	mv target/$(ARTIFACT_ID)-$(VERSION) target/bundle

bundle: target/bundle



test-unit:
	clojure -Adev:cli:test:runner

test-cli:
	clojure -A:cli:run -p dev-resources/profiles/cmi5/fixed.json -a dev-resources/personae/simple.json -l dev-resources/alignments/simple.json -o dev-resources/parameters/simple.json validate-input dev-resources/input/simple.json

test-cli-comprehensive:
	clojure -A:cli:run -i dev-resources/input/simple.json validate-input dev-resources/input/simple.json

test-cli-output:
	clojure -A:cli:run -i dev-resources/input/simple.json generate

test-bundle-output: bundle
	cd target/bundle; bin/run.sh -i ../../dev-resources/input/simple.json generate


ci: test-unit test-cli

server:
	clojure -A:server
