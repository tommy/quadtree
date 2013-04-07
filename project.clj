(defproject behave "0.1.0-SNAPSHOT"
  :main quad.demo
  :repl-init quad.demo
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/tools.logging "0.2.3"]
                 [quil "1.6.0"]
                 [cljts "0.2.0"]]
               
  :jvm-opts ["-XX:-OmitStackTraceInFastThrow" "-Xmx1028m"])
