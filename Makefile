.PHONY: clean bundle test-cli test-unit ci

GROUP_ID ?= com.yetanalytics
ARTIFACT_ID ?= datasim
VERSION ?= 0.1.0-SNAPSHOT
MAIN_NS ?= com.yetanalytics.datasim.main

clean:
	rm -rf target

target/bundle:
	clojure -A:build $(GROUP_ID) $(ARTIFACT_ID) $(VERSION) $(MAIN_NS) cli
	chmod u+x target/$(ARTIFACT_ID)-$(VERSION)/bin/run.sh
	mv target/$(ARTIFACT_ID)-$(VERSION) target/bundle

bundle: target/bundle



test-unit:
	clojure -Adev:cli:test:runner

test-cli:
	clojure -A:cli:run -p dev-resources/profiles/cmi5/fixed.json -a dev-resources/personae/simple.json -l dev-resources/alignments/simple.json -o dev-resources/parameters/simple.json validate-input

ci: test-unit test-cli
