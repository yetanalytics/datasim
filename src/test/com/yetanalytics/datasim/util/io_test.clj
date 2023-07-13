(ns com.yetanalytics.datasim.util.io-test
  (:require [clojure.test :refer [deftest testing is]]
            [com.yetanalytics.datasim.util.io :as io]
            [com.yetanalytics.datasim.test-constants :as const]))

(deftest read-json-location-test
  (testing "Reads files as json"
    (is (map? (io/read-json-location const/cmi5-profile-filepath))))
  (testing "Throws wrapped I/O errors when it can't read something"
    (is (= ::io/io-error
           (try (io/read-json-location "notthere.json")
                (catch clojure.lang.ExceptionInfo exi
                  (:type (ex-data exi))))))
    (is (= ::io/io-error
           (try (io/read-json-location "https://notarealhost329083")
                (catch clojure.lang.ExceptionInfo exi
                  (:type (ex-data exi))))))
    (is (= ::io/io-error
           (try (io/read-json-location "https://www.google.com/bingisgreat")
                (catch clojure.lang.ExceptionInfo exi
                  (:type (ex-data exi)))))))
  (testing "Throws wrapped parser errors when it can't read a format"
    (is (= ::io/parse-error
           (try (io/read-json-location "deps.edn")
                (catch clojure.lang.ExceptionInfo exi
                  (:type (ex-data exi))))))))
