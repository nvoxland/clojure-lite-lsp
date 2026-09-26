(ns csl.normalize-test
  (:require
   [clj-kondo.core :as kondo]
   [clojure.test :refer [deftest is testing]]
   [csl.normalize :as normalize]))

(defn kondo
  "Run clj-kondo on `code` as file `filename` with the options csl uses."
  [code filename & {:keys [external?]}]
  (with-in-str code
    (kondo/run! {:lint ["-"]
                 :filename filename
                 :lang (keyword (re-find #"[^.]+$" filename))
                 :cache false
                 :skip-lint external?
                 :config {:output {:canonical-paths true}
                          :linters (normalize/only-unresolved-namespace-linter)
                          :analysis (if external?
                                      normalize/dependency-analysis-options
                                      normalize/project-analysis-options)}})))

(defn elements
  "The normalized elements of `code`, as the only file analyzed."
  [code filename & opts]
  (let [units (normalize/normalize (apply kondo code filename opts)
                                   {:external? (:external? (apply hash-map opts))})]
    (is (= [filename] (keys units)))
    (get units filename)))

(defn of-kind [kind els] (filterv #(= kind (:kind %)) els))

(deftest var-definitions
  (let [[f m p] (of-kind :var-def (elements "(ns a)
(defn f \"Docs.\" [x] x)
(defmacro m [& body] `(do ~@body))
(defn- p [] 1)" "a.clj"))]
    (testing "names, positions and the whole form"
      (is (= {:kind :var-def :ns "a" :name "f" :lang #{:clj}
              :pos [2 7 2 8] :form [2 1 2 23]
              :defined-by "clojure.core/defn" :doc "Docs."}
             (select-keys f [:kind :ns :name :lang :pos :form :defined-by :doc]))))
    (testing "arglists are kept for hover"
      (is (= ["[x]"] (get-in f [:extra :arglist-strs]))))
    (testing "flags"
      (is (= #{} (:flags f)))
      (is (= #{:macro} (:flags m)))
      (is (= #{:private} (:flags p))))))

(deftest private-definitions-are-dropped-from-dependencies
  (is (= ["f"] (map :name (of-kind :var-def (elements "(ns a) (defn f []) (defn- p [])" "a.clj"
                                                      :external? true))))))

(deftest namespace-definitions-usages-and-aliases
  (let [els (elements "(ns a (:require [clojure.string :as str] [clojure.set]))" "a.clj")]
    (is (= [{:kind :ns-def :name "a" :pos [1 5 1 6]}]
           (map #(select-keys % [:kind :name :pos]) (of-kind :ns-def els))))
    (testing "a namespace usage names its target"
      (is (= #{["clojure.string" [1 18 1 32]] ["clojure.set" [1 43 1 54]]}
             (set (map (juxt :name :pos) (of-kind :ns-usage els))))))
    (testing "an alias is its own element, at the alias's position"
      (is (= [{:kind :ns-alias :name "clojure.string" :alias "str" :pos [1 37 1 40]}]
             (map #(select-keys % [:kind :name :alias :pos]) (of-kind :ns-alias els)))))))

(deftest var-usages
  (let [[join-alias join-fq] (of-kind :var-usage (elements "(ns a (:require [clojure.string :as str]))
(defn f [] (str/join [1]) (clojure.string/join [2]))" "a.clj"))]
    (is (= {:kind :var-usage :ns "clojure.string" :name "join" :alias "str"
            :from-ns "a" :from-var "f" :pos [2 13 2 21] :flags #{}}
           (select-keys join-alias [:kind :ns :name :alias :from-ns :from-var :pos :flags])))
    (testing "written fully qualified, without an alias"
      (is (= #{:fully-qualified} (:flags join-fq))))))

(deftest defmethod-usages-are-flagged
  (let [els (elements "(ns a) (defmulti mm identity) (defmethod mm :x [_] 1)" "a.clj")]
    (is (= [#{:defmethod}] (map :flags (filter #(= "mm" (:name %)) (of-kind :var-usage els)))))))

(deftest keywords
  (let [code "(ns a (:require [re-frame.core :as rf]))
(rf/reg-event-db ::save (fn [db _] db))
(defn f [m] (::save m) (:other m))"]
    (testing "registered keywords are definitions, the rest usages"
      (let [els (elements code "a.clj")]
        (is (= [["a" "save"]] (map (juxt :ns :name) (of-kind :keyword-def els))))
        ;; clj-kondo also reports the ns form's :require and :as
        (is (every? (set (map (juxt :ns :name) (of-kind :keyword-usage els)))
                    [["a" "save"] [nil "other"]]))))
    (testing "dependencies keep definitions but no keyword usages"
      (let [els (elements code "a.clj" :external? true)]
        (is (= 1 (count (of-kind :keyword-def els))))
        (is (empty? (of-kind :keyword-usage els)))))))

(deftest locals
  (let [els (elements "(ns a) (defn f [x] (let [y x] y))" "a.clj")
        [x y] (of-kind :local els)
        usages (of-kind :local-usage els)]
    (is (= ["x" "y"] (map :name [x y])))
    (testing "a local's form is its scope"
      (is (= [1 17 1 34] (:form x))))
    (testing "local usages point at their local"
      (is (= #{(:local-id x) (:local-id y)} (set (map :local-id usages))))
      (is (every? :pos usages)))))

(deftest quoted-symbols-and-java-classes
  (let [els (elements "(ns a (:import [java.io File])) (def s 'clojure.core/map) (File. \"x\")" "a.clj")]
    (is (= [["clojure.core" "map"]] (map (juxt :ns :name) (of-kind :symbol-usage els))))
    (is (some #(= "java.io.File" (:name %)) (of-kind :java-class-usage els)))
    (is (every? :pos els))))

(deftest protocol-impls
  (let [els (elements "(ns a) (defprotocol P (m [this])) (defrecord R [] P (m [this] 1))" "a.clj")]
    (is (= [{:kind :protocol-impl :ns "a" :name "m" :impl-ns "a"}]
           (map #(select-keys % [:kind :ns :name :impl-ns]) (of-kind :protocol-impl els))))))

(deftest cljc-elements-are-merged-across-languages
  (let [els (elements "(ns a) (defn f [] 1) #?(:cljs (defn g [] 2))" "a.cljc")]
    (is (= [["f" #{:clj :cljs}] ["g" #{:cljs}]]
           (map (juxt :name :lang) (of-kind :var-def els))))
    (testing "even though clj-kondo reports different :defined-by per language"
      (is (= "clojure.core/defn" (:defined-by (first (of-kind :var-def els))))))))

(deftest cljs-files-are-cljs
  (is (every? #(= #{:cljs} (:lang %)) (elements "(ns a) (defn f [] 1)" "a.cljs"))))

(deftest unresolved-namespaces-become-usages
  (let [[u] (filter #(= "union" (:name %))
                    (of-kind :var-usage (elements "(ns a) (defn f [] (set/union #{1} #{2}))" "a.clj")))]
    (is (= {:ns "set" :alias "set" :flags #{:unresolved :fully-qualified} :pos [1 20 1 29]}
           (select-keys u [:ns :alias :flags :pos])))))

;; Rules clj-kondo doesn't easily produce on demand: synthetic input.

(deftest invalid-elements-are-dropped
  (let [units (normalize/normalize
               {:analysis {:var-definitions [{:filename "a.clj" :ns 'a :name 'ok :row 1 :col 1 :end-row 1 :end-col 9
                                              :name-row 1 :name-col 7 :name-end-row 1 :name-end-col 9}
                                             {:filename "a.clj" :ns 'a :name 'derived :derived-location true
                                              :name-row 1 :name-col 7 :name-end-row 1 :name-end-col 9}
                                             {:filename "a.clj" :ns 'a :name 'derived2 :derived-name-location true
                                              :name-row 1 :name-col 7 :name-end-row 1 :name-end-col 9}
                                             {:filename "a.clj" :ns 'a :name 'nopos}
                                             {:filename "a.clj" :ns 'a
                                              :name-row 1 :name-col 7 :name-end-row 1 :name-end-col 9}]
                           :java-class-definitions [{:filename "x.jar:java/io/File.class" :class "java.io.File"}]}}
               {:external? false})]
    (is (= ["ok"] (map :name (get units "a.clj"))))
    (testing "except Java class definitions, which have no positions"
      (is (= [{:kind :java-class-def :name "java.io.File"}]
             (map #(select-keys % [:kind :name]) (get units "x.jar:java/io/File.class")))))))

(deftest imported-vars-remember-their-origin
  (let [[f] (of-kind :var-def (elements "(ns api (:require [potemkin :refer [import-vars]] [impl])) (import-vars [impl f])" "api.clj"))]
    (is (= "impl" (get-in f [:extra :imported-ns])))))
