(import (scheme base)
        (rename (testsuite) (run-tests run-scheme-tests))
        (rename (sunny table) (run-tests run-table-tests))
        (sunny testing))

(report-tests
  (run-scheme-tests)
  (run-table-tests))
