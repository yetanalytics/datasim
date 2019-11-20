(ns com.yetanalytics.datasim.io-test
  (:require [clojure.test :refer :all]
            [com.yetanalytics.datasim.io :as io]
            [com.yetanalytics.datasim.input.profile :as profile]))

(deftest read-loc-json-test
  (let [p (profile/map->Profile {})]
    (testing "Reads files as json"
      (is (map? (io/read-loc-json p "dev-resources/profiles/cmi5/fixed.json"))))
    (testing "Throws wrapped I/O errors when it can't read something"
      (is (= ::io/io-error
             (try (io/read-loc-json p "notthere.json")
                  (catch clojure.lang.ExceptionInfo exi
                    (:type (ex-data exi))))))
      (is (= ::io/io-error
             (try (io/read-loc-json p "https://notarealhost329083")
                  (catch clojure.lang.ExceptionInfo exi
                    (:type (ex-data exi))))))
      (is (= ::io/io-error
             (try (io/read-loc-json p "https://www.google.com/bingisgreat")
                  (catch clojure.lang.ExceptionInfo exi
                    (:type (ex-data exi)))))))
    (testing "Throws wrapped parser errors when it can't read a format"
      (is (= ::io/parse-error
             (try (io/read-loc-json p "deps.edn")
                  (catch clojure.lang.ExceptionInfo exi
                    (:type (ex-data exi)))))))))
