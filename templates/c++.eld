c++-mode c-mode

(if "if (" p ") {" n> r> n "}" >)
(for "for (" p ") {" n> r> n "}" >)
(while "while (" p ") {" n> r> n "}" >)
(do "do {" n> r> n "} while (" > p ")")
(ifelse "if (" p ") {" n> r> n> "} else {" > n> n "}" >)
(inc "#include " q)
(lambda "[" p "] " "(" p ") " "{" q "}")
