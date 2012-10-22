(defproject behave "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [quil "1.6.0"]
                 [cljts "0.2.0"]
                 [incanter "1.3.0" :exclusions [swank-clojure
                                                incanter/incanter-processing]]]
  :jvm-opts ["-XX:-OmitStackTraceInFastThrow" "-Xmx1028m"])
