(ns tufte-auto.profiling.impl
  (:refer-clojure :exclude [ifn?])
  (:require
   [taoensso.tufte]
   [clojure.string :as string]
   [schema.core :as s]
   taoensso.tufte.impl)
  (:import
   (java.util.regex Pattern)
   (clojure.lang Atom)
   (taoensso.tufte.impl PData)
   (clojure.lang IFn)))

(def ^{:arglists '([x])} ifn?
  "Is `x` invokable, and not a collection?"
  (every-pred clojure.core/ifn?
              (complement coll?)))

(defn ns-publics-of [sym]
  (or (some-> *ns* ns-aliases (get sym) ns-publics)
      (some-> sym find-ns ns-publics)))

(s/defschema PData?
  "A 'pdata' object, following the `#'taoensso.tufte/new-pdata` naming convention."
  (s/pred (partial instance? PData)))

(s/defschema Promise
  (s/pred (fn promise? [x]
            (-> x realized?) ;; try throwing an exception
            (ifn? x))))

(s/defn make-capture-time-fn [pdata :- PData?
                              var-ref :- (s/pred var?)
                              o :- Promise
                              g :- (s/pred symbol?)]
  (let [f (fn [& args]
            (let [t0 (System/nanoTime)
                  f (doto (get @o g)
                      assert)
                  v (apply f args)
                  t1 (- (System/nanoTime)
                        t0)]
              (taoensso.tufte/capture-time! pdata
                                            (-> var-ref symbol keyword)
                                            t1)
              v))]
    f
    #_ (reify
         IFn
         (invoke [this]
           (f))
         (invoke [this a]
           (f a))
         (invoke [this a b]
           (f a b))
         (invoke [this a b c]
           (f a b c))
         ;; XXX complete this...
         clojure.lang.IFn$OL
         (invokePrim [this]
           (long (f a))))))

(s/defn bindings-for
  [prefix :- (s/pred symbol?)
   pdata :- PData?
   original-values :- Promise
   a :- (s/pred (partial instance? Atom))
   [var-name var-ref]]
  (s/validate (s/pred symbol?) var-name)
  (s/validate (s/pred var?) var-ref)
  (let [v @var-ref
        g (gensym)]
    (when (and (ifn? v)
               (->> var-ref meta keys (not-any? #{:macro :const})))
      (swap! a assoc g v)
      [(symbol (str prefix) (str var-name))
       (make-capture-time-fn pdata var-ref original-values g)])))

(s/defn profiling-bindings-for
  [pdata :- PData?
   syms
   original-values :- Promise]
  {:pre  [(not (realized? original-values))]
   :post [(realized? original-values)]}
  (let [a (atom {})
        v (->> syms
               (mapcat (fn [sym]
                         (->> (ns-publics-of sym)
                              (keep (partial bindings-for sym pdata original-values a))
                              vec)))
               (into {}))]
    (deliver original-values @a)
    v))

;; https://github.com/clojure/clojure/blob/30a36cbe0ef936e57ddba238b7fa6d58ee1cbdce/build.xml#L59-L84
(def clojure-core-direct-linked
  "Libraries known to be distributed as direct-linked should not be profiled, because it will not work."
  (->> '[core
         core.protocols
         core.server
         main
         set
         edn
         xml
         zip
         inspector
         walk
         stacktrace
         template
         test
         test.tap
         test.junit
         pprint
         java.io
         repl
         java.browse
         java.javadoc
         java.shell
         java.browse-ui
         string
         data
         reflect
         datafy]
       (map (fn [s]
              (str "^clojure." s "$")))
       (string/join "|")
       re-pattern))

(defn expand-directives [directives & {:keys [all-ns-fn]
                                       :or   {all-ns-fn all-ns}}]
  (->> directives
       (mapcat (fn [x]
                 (if (symbol? x)
                   [x]
                   (do
                     (assert (instance? Pattern x))
                     (->> (all-ns-fn)
                          (map str)
                          (remove (some-fn (partial re-find clojure-core-direct-linked) ;; direct-linked, cannot be redefed
                                           (partial re-find #"^user$") ;; unlikely to be relevant
                                           (partial re-find #"^user\.*$") ;; unlikely to be relevant
                                           (partial re-find #"cider.nrepl.middleware.test") ;; test runners are irrelevant
                                           (partial re-find #"-test$") ;; can plausibly cause issues / unlikely to be relevant
                                           (partial re-find #"-test-helpers$") ;; can plausibly cause issues / unlikely to be relevant
                                           (partial re-find (re-pattern (str "^" (-> ::_ namespace (string/replace ".impl" "")) "$")))
                                           (partial re-find (re-pattern (str "^" (-> ::_ namespace) "$"))) ;; plausible to cause stack overflows
                                           (partial re-find #"^taoensso.tufte.*"))) ;; plausible to cause stack overflows
                          (filter (partial re-find x))
                          (map symbol))))))
       distinct))
