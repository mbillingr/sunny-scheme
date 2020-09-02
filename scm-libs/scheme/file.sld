(define-library (scheme write)
  (export
    file-exists?
    open-input-file
    open-output-file)

  (import (native file)))
