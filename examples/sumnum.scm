
(use pythonista.argparse)

(let ((parser (make-argument-parser :description "Process some integers.")))
  (argument-parser-on parser "integer" :metavar "N" :type <integer> :nargs "+"
                      :help "an integer for the accumulator")
  (argument-parser-on parser "--sum" :dest "accumulate" :action :store-const
                      :const sum :default max
                      :help "sum the integers (default: find the max)")
  (let (args (argument-parser-parse-args parser))
    (display (accumulate (ref integers args)))))
