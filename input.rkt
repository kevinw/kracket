(if #\c
                 (let [(foo #\f)]
                   (if foo 42 100))
                 99)
              "42")
