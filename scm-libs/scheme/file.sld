(define-library (scheme file)
  (export
    file-exists?
    open-input-file
    open-output-file)

  (import (native file)))
