{:user
 {:dependencies        [[cljdev "0.6.11"]]
  :injections          [(require 'nu)]
  :repositories        {"my.datomic.com" {:url   "https://my.datomic.com/repo"
                                          :creds :gpg}}
  :plugin-repositories [["nu-maven" {:url "s3p://nu-maven/releases/"}]]
  :plugins             [[s3-wagon-private "1.3.1"]]}
 :repl {:plugins [[cider/cider-nrepl "0.16.0"]
                  [refactor-nrepl "2.3.1"]
                  [lein-kibit "0.1.5"]
                  [lein-bikeshed "0.5.0"]]
        :dependencies [[org.clojure/tools.nrepl "0.2.12"]
                       [mvxcvi/puget "1.0.1"]]
        :repl-options {:timeout 120000}}}
