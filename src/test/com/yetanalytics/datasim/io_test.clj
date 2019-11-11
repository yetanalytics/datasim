(ns com.yetanalytics.datasim.io-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.datasim.io :as io]))

(deftest read-location-test
  (testing "Reads files as json"
    (is (map? (io/read-location "dev-resources/profiles/cmi5/fixed.json"))))
  (testing "Throws wrapped I/O errors when it can't read something"
    (is (= ::io/io-error
           (try (io/read-location "notthere.json")
                (catch clojure.lang.ExceptionInfo exi
                  (:type (ex-data exi))))))
    (is (= ::io/io-error
           (try (io/read-location "https://notarealhost329083")
                (catch clojure.lang.ExceptionInfo exi
                  (:type (ex-data exi))))))
    (is (= ::io/io-error
           (try (io/read-location "https://www.google.com/bingisgreat")
                (catch clojure.lang.ExceptionInfo exi
                  (:type (ex-data exi)))))))
  (testing "Throws wrapped parser errors when it can't read a format"
    (is (= ::io/parse-error
           (try (io/read-location "deps.edn" :fmt :json)
                (catch clojure.lang.ExceptionInfo exi
                  (:type (ex-data exi))))))))
