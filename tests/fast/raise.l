(assert (throws? '(parse . "unexpected token: ;")
                 (raise 'parse "unexpected token: %s" ";")))

(assert (throws? '(io . "path=\"/tmp/a\"")
                 (raise 'io "path=%r" "/tmp/a")))

(assert (throws? '(state . "counter=42 ratio=1.250000")
                 (raise 'state "counter=%d ratio=%f" 42 1.25)))

(assert (throws? '(:oops . "boom")
                 (raise :oops "boom")))

(assert (throws? '(format . "missing format argument for %s")
                 (raise 'parse "expected %s at %s" "identifier")))

(assert (throws? '(format . "format argument for %d is String, expected Integer")
                 (raise 'parse "n=%d" "3")))

(assert (throws? '(type . "argument fmt is Integer, expected String")
                 (raise 'parse 1)))
