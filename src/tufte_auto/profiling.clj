(ns tufte-auto.profiling
  "A mechanism for profiling function calls."
  (:require
   [clojure.string :as string]
   [tufte-auto.profiling.impl :as impl]
   [schema.core :as s]
   [taoensso.tufte])
  (:import
   (java.util.regex Pattern)))

(defn profiling**
  [directives f]
  (let [pdata (taoensso.tufte/new-pdata {:dynamic? true})
        original-values (promise)
        redef-bindings (into {}
                             (map (fn [[a f]]
                                    [(resolve a) f]))
                             (impl/profiling-bindings-for pdata
                                                          (impl/expand-directives directives)
                                                          original-values))]
    (assert (contains? #{nil "false"} (System/getProperty "clojure.compiler.direct-linking"))
            "tufte-auto.profiling cannot work over a direct-linked project")
    (with-redefs-fn redef-bindings
      (fn []
        (taoensso.tufte/with-profiling pdata {:dynamic? true}
          (taoensso.tufte/add-basic-println-handler! {})
          (let [v (f)]
            [(taoensso.tufte/format-pstats (deref pdata))
             v]))))))

(defn profiling*
  "A non-macro version `#'profiling` for optional cleanliness. See its docstring."
  [directives f]
  (let [[data v] (profiling** directives f)]
    (println data)
    v))

(defmacro profiling
  "Executes `body` while automatically profiling all *public* defns belonging to the namespaces denoted by `directives`.

  The profiling works even in face of arbitrary concurrent access (e.g. `clojure.core.async/go` blocks).

  `directives` can be a vector of:
    - ns aliases (e.g. `sut`) or
    - symbols denoting namespaces (e.g. `clojure.set`).
      - In case of ambiguity, aliases take preference.
    - regexes, that will act as a filter of against the output of `(clojure.core/all-ns)`.

  NOTE: namespaces believed to be problematic or irrelevant will not be profiled: test code, direct-linked code, code in the `user` ns."
  {:style/indent 1}
  [directives & body]
  {:pre [(vector? directives)
         (seq body)]}
  `(profiling* ~(list 'quote directives) (fn []
                                           (do
                                             ~@body))))

(defmacro profiling-fixture
  "Makes a profiling fixture for `directives`.

  See the `#'profiling` docstring."
  [directives]
  `(fn [tests#]
     (profiling ~directives (tests#))))
