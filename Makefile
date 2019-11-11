.PHONY: clean bundle ci

GROUP_ID ?= com.yetanalytics
ARTIFACT_ID ?= datasim
VERSION ?= 0.1.0-SNAPSHOT
MAIN_NS ?= com.yetanalytics.datasim

clean:
	rm -rf target

target/bundle:
	clojure -A:build $(GROUP_ID) $(ARTIFACT_ID) $(VERSION) $(MAIN_NS)
	chmod u+x target/$(ARTIFACT_ID)-$(VERSION)/bin/run.sh
	mv target/$(ARTIFACT_ID)-$(VERSION) target/bundle

bundle: target/bundle

ci:
	clojure -A:test:runner
