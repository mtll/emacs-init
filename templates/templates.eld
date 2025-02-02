;; taken from https://github.com/minad/tempel/blob/5b09f612cfd805dba5e90bf06580583cab045499/README.org#template-file-format

fundamental-mode ;; Available everywhere

(today (format-time-string "%Y-%m-%d"))

latex-mode org-mode

;; (abstract "\\begin{abstract}\n" r> n> "\\end{abstract}")
;; (align "\\begin{align}\n" r> n> "\\end{align}")
;; (alignn "\\begin{align*}\n" r> n> "\\end{align*}")
;; (appendix "\\begin{appendix}\n" r> n> "\\end{appendix}")
;; (begin "\\begin{" (s env) "}" r> n> "\\end{" (s env) "}")
;; (center "\\begin{center}\n" r> n> "\\end{center}")
;; (displaymath "\\begin{displaymath}\n" r> n> "\\end{displaymath}")
;; (document "\\begin{document}\n" r> n> "\\end{document}")
;; (enumerate "\\begin{enumerate}\n\\item " r> n> "\\end{enumerate}")
;; (equation "\\begin{equation}" r> n> "\\end{equation}")
;; (flushleft "\\begin{flushleft}" r> n> "\\end{flushleft}")
;; (flushright "\\begin{flushright}" r> n> "\\end{flushright}")
;; (frac "\\frac{" p "}{" q "}")
;; (sum "\\sum_{" p "}^{" q "}")
;; (product "\\prod_{" p "}^{" q "}")
;; (fussypar "\\begin{fussypar}" r> n> "\\end{fussypar}")
;; (gather "\\begin{gather}\n" r> n> "\\end{gather}")
;; (gatherr "\\begin{gather*}\n" r> n> "\\end{gather*}")
;; (itemize "\\begin{itemize}\n\\item " r> n> "\\end{itemize}")
;; (letter "\\begin{letter}\n" r> n> "\\end{letter}")
;; (math "\\begin{math}\n" r> n> "\\end{math}")
;; (minipage "\\begin{minipage}[t]{0.5\linewidth}\n" r> n> "\\end{minipage}")
;; (quotation "\\begin{quotation}\n" r> n> "\\end{quotation}")
;; (quote "\\begin{quote}\n" r> n> "\\end{quote}")
;; (sloppypar "\\begin{sloppypar}\n" r> n> "\\end{sloppypar}")
;; (theindex "\\begin{theindex}\n" r> n> "\\end{theindex}")
;; (trivlist "\\begin{trivlist}\n" r> n> "\\end{trivlist}")
;; (verbatim "\\begin{verbatim}\n" r> n> "\\end{verbatim}")
;; (verbatimm "\\begin{verbatim*}\n" r> n> "\\end{verbatim*}")

org-mode

(nm "#+name: " r)
(cpt "#+caption: ")
(drw ":" p ":" n r ":end:")
(bgn "#+begin_" (s name) n> r> n "#+end_" name)
(qu "#+begin_quote" n> r> n "#+end_quote")
(snote "#+begin_sidenote" n> r> n "#+end_sidenote")
(mnote "#+begin_marginnote" n> r> n "#+end_marginnote")
(ex "#+begin_example" n> r> n "#+end_example")
(cent "#+begin_center" n> r> n "#+end_center")
(ascii "#+begin_export ascii" n> r> n "#+end_export")
(html "#+begin_export html" n> r> n "#+end_export")
(latex "#+begin_export latex" n> r> n "#+end_export")
(com "#+begin_comment" n> r> n "#+end_comment")
(verse "#+begin_verse" n> r> n "#+end_verse")
(src "#+begin_src " q n r n "#+end_src")
(gnuplot "#+begin_src gnuplot :var data=" (p "table") " :file " (p "plot.png") n r n "#+end_src" :post (org-edit-src-code))
(elisp "#+begin_src emacs-lisp" n r n "#+end_src" :post (org-edit-src-code))
(inlsrc "src_" p "{" q "}")
(title "#+title: " p n "#+author: David Feller" n "#+language: en")

c++-mode c-mode

(if "if (" p ") {" n> r> n "}" >)
(for "for (" p ") {" n> r> n "}" >)
(while "while (" p ") {" n> r> n "}" >)
(do "do {" n> r> n "} while (" > p ")")
(ifelse "if (" p ") {" n> r> n> "} else {" > n> n "}" >)
(inc "#include " q)

c++-mode

(lambda "[" p "] " "(" p ") " "{" q "}")

text-mode

(box "┌─" (make-string (length str) ?─) "─┐" n
     "│ " (s str)                       " │" n
     "└─" (make-string (length str) ?─) "─┘" n)
(abox "+-" (make-string (length str) ?-) "-+" n
      "| " (s str)                       " |" n
      "+-" (make-string (length str) ?-) "-+" n)
(cut "--8<---------------cut here---------------start------------->8---" n r n
     "--8<---------------cut here---------------end--------------->8---" n)
(rot13 (p "plain text" text) n "----" n (rot13 text))
(calc (p "taylor(sin(x),x=0,3)" formula) n "----" n (format "%s" (calc-eval formula)))
(table (p (read-number "Rows: ") rows noinsert)
       (p (read-number "Cols: ") cols noinsert)
       "| " (p "  ") (* (1- cols) " | " (p "  ")) " |" n
       "|" (* cols "----|") n
       (* rows "| " (p "  ") (* (1- cols) " | " (p "  ")) " |" n))

c-mode :when (re-search-backward "^\\S-*$" (line-beginning-position) 'noerror)

(inc "#include <" (p (concat (file-name-base (or (buffer-file-name) (buffer-name))) ".h")) ">")
(incc "#include \"" (p (concat (file-name-base (or (buffer-file-name) (buffer-name))) ".h")) "\"")

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
