(ns ^:figwheel-no-load crud.dev
  (:require
    [crud.core :as core]
    [devtools.core :as devtools]))

(devtools/install!)

(enable-console-print!)

(core/init!)
