;; This file is part of the parinfer-elisp project:
;; https://github.com/oakmac/parinfer-elisp
;;
;; You can run this file on the command line:
;; emacs --script test.el
;;
;; It will run the Parinfer test suite and output the results to the console.
;; The script will exit with "1" on any test failure. Exit "0" otherwise.

(require 'json)
(load-file "parinferlib.el")

;; NOTE: useful when debugging a failed test
;; (setq debug-on-error t)

;;------------------------------------------------------------------------------
;; Util functions
;;------------------------------------------------------------------------------

(defun get-string-from-file (file-path)
  "Return file-path's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun print-err (msg)
  (message msg))

(defun println (txt)
  (princ (concat txt "\n")))

(defun squiggly-line ()
  (println "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"))

(defun string-join (l)
  (mapconcat 'identity l "\n"))

(defun test-by-line-no (tests line-no)
  (find line-no tests :key (lambda (test)
                             (plist-get (plist-get test :in)
                                        :fileLineNo))))

(defun convert-result-error (error)
  "Converts error from the library result into a list."
  (list (plist-get error :name)
        (plist-get error :line-no)
        (plist-get error :x)))

(defun convert-test-error (error)
  "Converts error from the test JSON into a list."
  (list (plist-get error :name)
        (plist-get error :lineNo)
        (plist-get error :x)))

(defun convert-result-tabstops (tabstops)
  "Converts tabStops from the library result into a list."
  (mapcar
    (lambda (ts) (list (plist-get ts :ch)
                       (plist-get ts :line-no)
                       (plist-get ts :x)))
    tabstops))

(defun convert-test-tabstops (tabstops)
  "Converts tabStops from the test JSON into a list."
  (mapcar
    (lambda (ts) (list (plist-get ts :ch)
                       (plist-get ts :lineNo)
                       (plist-get ts :x)))
    tabstops))

;;------------------------------------------------------------------------------
;; Parser functions
;;------------------------------------------------------------------------------

(defun make-parse-error (line-no message)
  (format "test parse error at line %d: %s" line-no message))

(defun parse-cursor-from-line (options input-line-no output-line-no line)
  "Extracts cursor from LINE, returning cleaned line. Expects nonempty alist in OPTIONS. Signals error if cursor is already present"
  (save-match-data
    (let* ((cursor-x (string-match "|" line))
           (clean (replace-regexp-in-string "|" "" line)))
      (when cursor-x
        (when (alist-get :cursor-x options)
          (throw 'parinferlib-error (make-parse-error input-line-no
                                                      (format "cursor already found at line %d" (alist-get :cursor-line options)))))
        (when (string-match "|" line (1+ cursor-x))
          (throw 'parinferlib-error (make-parse-error input-line-no
                                                      "only one cursor allowed")))
        (setf (alist-get :cursor-x options) cursor-x
              (alist-get :cursor-line options) output-line-no))
      clean)))

(defconst parinferlib--CURSORDX_MATCH
  "^\\\([[:blank:]]*\\\)^ cursorDx \\\(-?[0-9]+\\\)")

(defun parse-cursor-dx-line (options input-line-no output-line-no line)
  "Match line with cursorDx annotation; return t and set OPTIONS if annotation found"
  (save-match-data
    (when (string-match parinferlib--CURSORDX_MATCH line)
      (let ((cursor-x (alist-get :cursor-x options))
            (cursor-line (alist-get :cursor-line options)))
        (unless cursor-x
          (throw 'parinferlib-error (make-parse-error input-line-no
                                                      "cursor not found before cursorDx line")))
        (unless (= cursor-line (1- output-line-no))
          (throw 'parinferlib-error (make-parse-error input-line-no
                                                      "cursor not precedes cursorDx line")))
        (unless (= cursor-x (length (match-string 1 line)))
          (throw 'parinferlib-error (make-parse-error input-line-no
                                                      "cursor position does not match with cursorDx annotation")))
        (setf (alist-get :cursor-dx options)
              (string-to-number (match-string 2 line)))
        t))))
            

(defconst parinferlib--LOOSE_DIFF_MATCH
  "^[[:blank:]]*[-+]+[-+[:blank:]]*$")

(defconst parinferlib--DIFF_MATCH
  "^\\\(\\\([[:blank:]]*\\\)\\\(-*\\\)\\\(+*\\\)\\\)[[:blank:]]*$")

(defun make-change-struct ()
  (mapcar 'list '(:line-no :x :old-text :new-text)))

(defun make-diff-struct ()
  (mapcar 'list '(:end-line-char :start-line-char :code-line-no :input-line-no :code)))

(defun parse-diff-line (options input-line-no output-line-no line prev-line diff)
  "Test if LINE is a diff line and if so return changed version of PREV-LINE. OPTIONS is updated to preserve cursor data and cullect changes; DIFF is filled if needed"
  (save-match-data
    (when (string-match parinferlib--LOOSE_DIFF_MATCH line)
      (unless prev-line
        (throw 'parinferlib-error (make-parse-error input-line-no "Diff line must have preceding line")))
      (unless (string-match parinferlib--DIFF_MATCH line)
        (throw 'parinferlib-error (make-parse-error input-line-no "Diff chars must be adjancent with '-'s before '+'s")))
      (let* ((total (length (match-string 1 line)))
             (x (length (match-string 2 line)))
             (old-len (length (match-string 3 line)))
             (new-len (length (match-string 4 line)))
             (new-x (+ x old-len))
             (len (+ old-len new-len))
             (x-end (1- (+ x len)))
             (cursor-x (alist-get :cursor-x options))
             (change (make-change-struct))
             (end-line-char (when (= (length prev-line) x-end)
                              (aref line x-end))))
        (when (> total (1+ (length prev-line)))
          (throw 'parinferlib-error (make-parse-error input-line-no "Diff line longer then previous line with account of newline character")))
        (when (equal (alist-get :cursor-line options)
                     (1- output-line-no))
          (when (>= x-end cursor-x x)
            (throw 'parinferlib-error (make-parse-error input-line-no "Cursor cannot be over diff annotation")))
          (if (> x cursor-x)
              (setf x (1- x) new-x (1- new-x) x-end (1- x-end))
            (setf (alist-get :cursor-x options) (- cursor-x old-len))))
;; set diff fields
        (setf (alist-get :start-line-char diff) (aref line 0)
              (alist-get :end-line-char diff) end-line-char
              (alist-get :code-line-no diff) (1- output-line-no)
              (alist-get :input-line-no diff) input-line-no
              (alist-get :code diff)
              (concat (substring prev-line 0 x)
                      (substring prev-line new-x))
              (alist-get :line-no change) (1- output-line-no)
              (alist-get :x change) x
              (alist-get :old-text change)
              (concat (substring prev-line x (+ x old-len))
                      (when (equal end-line-char ?-) "\n"))
              (alist-get :new-text change)
              (concat (substring prev-line new-x (+ new-x new-len))
                      (when (equal end-line-char ?+) "\n")))
        (push change (alist-get :changes options))
        (alist-get :code diff)))))

(defun postprocess-diffs (options output-lines input-line-no output-line-no diff prev-diff)
  "Merge changes, lines and diffs as necessary. OUTPUT-LINES is a stack which top is last processed line. Return number of last output line"
  ;; check diffs
  (let ((prev-diff-line (alist-get :input-line-no prev-diff))
        (prev-diffed-line (alist-get :code-line-no prev-diff))
        (cur-diff-line (alist-get :code-line-no diff))
        (cur-diffed-line (alist-get :input-line-no diff))
        (prev-last-char (alist-get :end-line-char prev-diff))
        (cur-first-char (alist-get :start-line-char diff))
        (result output-line-no))
    (if (and prev-diffed-line cur-diffed-line)
        (if (> 2 (- cur-diffed-line prev-diffed-line))
            (throw 'parinferlib-error (make-parse-error cur-diffed-line "diff lines must be directly below a code line"))))
          ;; check if there is conflict
    (when (equal prev-last-char ?+)
        (if (equal cur-first-char ?-)
            (throw 'parinferlib-error (make-parse-error cur-diffed-line "diff line starts with '-', which cannot come after '+' which previous diff line ends with"))))
            ;; now chack for merge
    (when (and (equal prev-last-char ?-))
      (if (and cur-diff-line (= 2 (- cur-diff-line pref-diff-line)))
        (progn
          (setcar output-lines (concat (pop (cdr output-lines))
                                       (car output-lines)))
          (decf result)
          (if cur-first-char
             (let ((cur-change (pop (alist-get :changes options)))
                   (prev-change (pop (alist-get :changes options))))
               (setf (alist-get :old-text prev-change)
                     (concat (alist-get :old-text prev-change)
                             (alist-get :old-text cur-change))
                     (alist-get :new-text prev-change)
                     (concat (alist-get :new-text prev-change)
                             (alist-get :new-text cur-change)))
               (push prev-change (alist-get :changes options)))
           (let ((change (pop (alist-get :changes options))))
             (setf (alist-get :x change)
                   (+ (length (alist-get :code prev-diff))
                      (alist-get :x change))
                   (alist-get :line-no change) prev-diffed-line)
             (push change (alist-get :changes options)))))
        (when (= 2 (- input-line-no pref-diff-line))
          (let ((joined-line (pop (cdr output-lines))))
            (setcar (cdr output-lines)
                    (concat (cadr output-lines) joined-line))
            (decf result))))
      (unless (= result output-line-no)
        (when (equal (1+ prev-diffed-line) (alist-get :cursor-line options))
          (setf (alist-get :cursor-line options) prev-diffed-line
                (alist-get :cursor-x options)
                (+ (alist-get :cursor-x options) (length (alist-get :code prev-diff)))))))
    result))

(defun prepare-in (in)
  "Process IN, extracting all interesting data from text"
  (let* ((line-no (plist-get in :fileLineNo))
         (text (plist-get in :text))
         (in-lines (save-match-data
                     (split-string text parinferlib--LINE_ENDING_REGEX)))
         (options '((:cursor-x) (:cursor-dx) (:cursor-line) (:changes)))
         (out-lines '()))
    (loop for prev-line = nil then cur-line
          for cur-line in in-lines
          for i = 0 then (1+ i)
          with j = 0
          with out-line = nil
          with prev-diff = (make-diff-struct)
          for diff = (make-diff-struct) then (make-diff-struct)
          do
          ;; lines with cursordx annotation do not recieve other pocessing
          (unless (parse-cursor-dx-line options i j line)
            (setq cur-line (parse-cursor-from-line options i j cur-line))
            (setq out-line (parse-diff-line options i j cur-line prev-line diff))
            (if out-line
                (setf (car out-lines) out-line)
              (push cur-line out-lines)
              (incf j))
            (setq j (postprocess-diffs options out-lines i j diff prev-diff))
            (when (or (alist-get :input-line-no diff)
                      (not (alist-get :input-line-no prev-diff))
                      (> 1 (- i (alist-get :input-line-no prev-diff))))
              (setq prev-diff diff)))
          finally
          (postprocess-diffs options out-lines (+ i 2) j nil prev-diff))
    (list :options options :lines (reverse out-lines))))


(defun prepare-out (out)
  "Process OUT, extracting allinteresting data from text"
  nil)
    

(defun prepare-test (test)
  "Process TEST, transforming `text` field into plist with needed data"
  (let ((in (prepare-in (plist-get test :in)))
        (out (prepare-out (plist-get test :out))))
    (list :in in :out out)))

;;------------------------------------------------------------------------------
;; Load test files
;;------------------------------------------------------------------------------

(defconst indent-mode-tests
  (let ((test-str (get-string-from-file "tests/indent-mode.json"))
        (json-object-type 'plist))
    (json-read-from-string test-str)))

(defconst paren-mode-tests
  (let ((test-str (get-string-from-file "tests/paren-mode.json"))
        (json-object-type 'plist))
    (json-read-from-string test-str)))

(defconst smart-mode-tests
  (let ((test-str (get-string-from-file "tests/smart-mode.json"))
        (json-object-type 'plist))
    (json-read-from-string test-str)))

;;------------------------------------------------------------------------------
;; Test runner
;;------------------------------------------------------------------------------

(defvar num-tests-failed 0)
(defvar num-tests-ran 0)

(defun run-test (mode test)
  (let* ((mode-string (if (equal :indent mode) "Indent Mode" "Paren Mode"))
         (in (plist-get test :in))
         (out (plist-get test :out))
         (out-cursor (plist-get out :cursor))
         (out-error (plist-get out :error))
         (out-tabstops (plist-get out :tabStops))
         (test-id (number-to-string (plist-get in :fileLineNo)))
         (in-text (string-join (plist-get in :lines)))
         (expected-text (string-join (plist-get out :lines)))
         (in-cursor (plist-get in :cursor))
         (cursor-x (plist-get in-cursor :cursorX))
         (cursor-line (plist-get in-cursor :cursorLine))
         (cursor-dx (plist-get in-cursor :cursorDx))
         (preview-cursor-scope (plist-get in-cursor :previewCursorScope))
         (options (list :cursor-x cursor-x
                        :cursor-line cursor-line
                        :cursor-dx cursor-dx
                        :preview-cursor-scope preview-cursor-scope))
         (test-idempotence? (and (not out-error)
                                 (not out-tabstops)
                                 (not cursor-dx)))
         (test-cross-mode? (and (not out-error)
                                (not out-tabstops)
                                (not in-cursor)))
         (result-1 (if (equal :indent mode)
                     (parinferlib-indent-mode in-text options)
                     (parinferlib-paren-mode in-text options)))
         (out-text-1 (plist-get result-1 :text))
         (result-2 (if (equal :indent mode)
                     (parinferlib-indent-mode out-text-1 options)
                     (parinferlib-paren-mode out-text-1 options)))
         (out-text-2 (plist-get result-2 :text))
         (failed? nil))
    ;; in/out text equality
    (when (not (equal out-text-1 expected-text))
      (setq failed? t)
      (print-err (concat mode-string " In/Out Text failure: test id " test-id)))

    ;; check cursor-x
    (when (and in-cursor
               (not (equal (plist-get result-1 :cursor-x)
                           (plist-get out-cursor :cursorX))))
      (setq failed? t)
      (print-err (concat mode-string " cursorX failure: test id " test-id)))

    ;; check error output
    (when out-error
      (let ((result-error2 (convert-result-error (plist-get result-1 :error)))
            (out-error2 (convert-test-error out-error)))
        (when (or (plist-get result-1 :success)
                  (not (equal result-error2 out-error2)))
          (setq failed? t)
          (print-err (concat mode-string " Error Output failure: test id " test-id)))))

    ;; check tab stops
    (when out-tabstops
      (let ((result-tabstops (convert-result-tabstops (plist-get result-1 :tab-stops)))
            (out-tabstops2 (convert-test-tabstops out-tabstops)))
        (when (not (equal result-tabstops out-tabstops2))
          (setq failed? t)
          (print-err (concat mode-string " Tab Stops failure: test id " test-id)))))

    ;; idempotence
    (when test-idempotence?
      (when (not (equal out-text-2 expected-text))
        (setq failed? t)
        (print-err (concat mode-string " Idempotence failure: test id " test-id))))

    ;; cross-mode preservation
    (when test-cross-mode?
      (let* ((result-3 (if (equal :indent mode)
                         (parinferlib-paren-mode out-text-1 options)
                         (parinferlib-indent-mode out-text-1 options)))
             (out-text-3 (plist-get result-3 :text)))
        (when (not (equal out-text-3 expected-text))
          (setq failed? t)
          (print-err (concat mode-string " Cross-mode Preservation failure: test id " test-id)))))

    ;; increment the test counts
    (setq num-tests-ran (1+ num-tests-ran))
    (when failed?
      (setq num-tests-failed (1+ num-tests-failed)))))

;;------------------------------------------------------------------------------
;; Run the tests and print the result
;;------------------------------------------------------------------------------

(princ "\n\n")
(squiggly-line)
(println "Running Parinfer Tests...")
(squiggly-line)

(mapc (lambda (test) (run-test :indent test)) indent-mode-tests)
(mapc (lambda (test) (run-test :paren test)) paren-mode-tests)

(squiggly-line)
(let ((done-msg (if (equal 0 num-tests-failed) "SUCCESS! " "Done. ")))
  (println (concat done-msg
                   "Ran " (number-to-string num-tests-ran) " tests. "
                   (number-to-string num-tests-failed) " failures.")))
(squiggly-line)
(princ "\n\n")

;; exit with "1" on failure
;; NOTE: this is necessary for travis-ci
(when (not (equal num-tests-failed 0))
  (kill-emacs 1))
