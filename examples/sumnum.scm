
(use pythonista.argparse)

(let ((parser (make-argument-parser :description "Process some integers.")))
  (parser-on parser "integer" :metavar "N" :type <integer> :nargs "+"
             :help "an integer for the accumulator")
  (parser-on parser "--sum" :dest :accumulate :action :store-const
             :const (lambda (integers)
                      (fold + 0 integers))
             :default (lambda (integers)
                        (fold max 0 integers))
             :help "sum the integers (default: find the max)")
  (let* ((args (parser-parse parser))
         (integers (tree-map-get args :integer))
         (accumulate (tree-map-get args :accumulate)))
    (display (accumulate integers)))))
