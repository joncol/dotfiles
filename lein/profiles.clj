{:user {:dependencies [[org.clojure/clojure "1.9.0"]
                       [org.clojure/tools.trace "0.7.9"]
                       [spyscope "0.1.7-SNAPSHOT"]]
        :injections [(require 'spyscope.core)
                     (use 'clojure.repl)
                     (use 'no.disassemble)]
        :plugins [[lein-nodisassemble "0.1.3"]
                  [lein-ancient "0.6.15"]
                  [lein-cljfmt "0.5.7"]
                  [lein-kibit "0.1.6"]
                  [venantius/ultra "0.5.2"]]}}
