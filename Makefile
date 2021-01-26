.PHONY: clean bundle test-cli test-cli-comprehensive test-cli-output test-unit ci server test-bundle-output

GROUP_ID ?= com.yetanalytics
ARTIFACT_ID ?= datasim
VERSION ?= 0.1.0-SNAPSHOT
MAIN_NS ?= com.yetanalytics.datasim.main

clean:
	rm -rf target

target/bundle/datasim_cli.jar:
	mkdir -p target/bundle
	rm -f pom.xml
	clojure -X:depstar uberjar :no-pom false :sync-pom true :aliases '[:cli]' :aot true :group-id $(GROUP_ID) :artifact-id $(ARTIFACT_ID)-cli :version '"$(VERSION)"' :jar target/bundle/datasim_cli.jar :main-class com.yetanalytics.datasim.main
	rm -f pom.xml

target/bundle/datasim_server.jar: # no AOT for this one
	mkdir -p target/bundle
	rm -f pom.xml
	clojure -X:depstar uberjar :no-pom false :sync-pom true :aliases '[:server]' :group-id $(GROUP_ID) :artifact-id $(ARTIFACT_ID)-server :version '"$(VERSION)"' :jar target/bundle/datasim_server.jar :main-class com.yetanalytics.datasim.server
	rm -f pom.xml

target/bundle/datasim_onyx.jar:
	mkdir -p target/bundle
	rm -f pom.xml
	clojure -X:depstar uberjar :no-pom false :sync-pom true :aliases '[:onyx,:cli]' :aot true :group-id $(GROUP_ID) :artifact-id $(ARTIFACT_ID)-onyx :version '"$(VERSION)"' :jar target/bundle/datasim_onyx.jar :main-class com.yetanalytics.datasim.onyx.main
	rm -f pom.xml

target/bundle/bin:
	mkdir -p target/bundle/bin
	cp -r scripts/*.sh target/bundle/bin
	chmod +x target/bundle/bin

target/bundle: target/bundle/bin target/bundle/datasim_cli.jar target/bundle/datasim_server.jar target/bundle/datasim_onyx.jar

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
