(ns reagenttest.doo-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [reagenttest.core-test]))

(doo-tests 'reagenttest.core-test)

