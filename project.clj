(defproject threatgrid/resolve-java-sources-and-javadocs "1.3.0"
  :description "Makes available .jars with Java sources and javadocs for a given project."

  :url "https://github.com/threatgrid/clj-experiments"

  :license {:name "EPL-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}

  ;; Temporary fork addressing https://github.com/brandonbloom/fipp/issues/72
  :dependencies [[threatgrid/fipp "0.6.24" :exclusions [org.clojure/clojure]]
                 [prismatic/schema "1.1.9"]
                 [com.taoensso/tufte "2.1.0"]]

  :eval-in-leiningen ~(nil? (System/getenv "no_eval_in_leiningen"))

  :profiles {;; These developing the plugin when (false? eval-in-leiningen):
             :dev                 {:dependencies [[clj-commons/pomegranate "1.2.0"]
                                                  [org.clojure/clojure "1.10.1"]
                                                  [com.taoensso/tufte "2.1.0"]]}

             :integration-testing {:source-paths ["integration-testing"]
                                   :dependencies [[com.taoensso/tufte "2.1.0"]]}

             :self-test           {:middleware   [leiningen.resolve-java-sources-and-javadocs/middleware]
                                   ;; ensure that at least one dependency will fetch sources:
                                   :dependencies [[puppetlabs/trapperkeeper-webserver-jetty9 "4.1.0"]]}

             :profiling           {:dependencies [[com.taoensso/tufte "2.1.0"]]}}

  :aliases {"integration-test" ["with-profile" "+integration-testing" "run" "-m" "integration-test"]})
