{:user {:dependencies [#_[org.clojure/clojure "1.9.0"]
                       #_[org.clojure/tools.trace "0.7.9"]
                       #_[spyscope "0.1.7-SNAPSHOT"]]
        :injections [#_(require 'spyscope.core)
                     #_(use 'clojure.repl)
                     #_(use 'no.disassemble)]
        :plugins [#_[lein-nodisassemble "0.1.3"]
                  [lein-ancient "0.6.15"]
                  [lein-cljfmt "0.6.5"]
                  [lein-kibit "0.1.8"]
                  #_[venantius/ultra "0.5.2"]]}}
