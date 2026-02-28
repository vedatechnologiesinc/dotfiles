;;;; $DOOMDIR/+definitions.el -*- lexical-binding: t; -*-


;;; ralju

(defmacro λ (&rest args)
  "Define a lambda."
  `(lambda ,@args))

(defmacro def (&rest args)
  "Define a function."
  `(defun ,@args))

(defmacro defm (&rest args)
  "Define a macro."
  `(defmacro ,@args))

(defmacro defv (&rest args)
  "Define a special variable."
  `(defvar ,@args))


;;; datcme

(defvar alias-table
  '((yes-or-no-p . y-or-n-p)
    (ef  . expand-file-name)
    (rb  . revert-buffer)
    (cat . concat)
    (bb  . bury-buffer)
    (bm  . buffer-menu)
    (ro  . read-only-mode)
    (ow  . overwrite-mode)
    (rs  . replace-string)
    (rr  . replace-regexp)
    (qr  . query-replace)
    (qrr . query-replace-regexp)
    (af  . auto-fill-mode)
    (dc  . delete-char)
    (dr  . delete-region)
    (ib  . ispell-buffer)
    (id  . ispell-change-dictionary)
    (ap  . find-file-at-point)
    (tl  . transpose-lines)
    (rf  . rename-file)
    (fa  . find-alternate-file)
    (tr  . table-recognize)
    (tu  . table-unrecognize)
    (tir . table-insert-row)
    (tdr . table-delete-row)
    (dcr . downcase-region)
    (ucr . upcase-region)
    (ccr . capitalize-region)
    (bod . beginning-of-defun)
    (eod . end-of-defun)
    (pi  . package-install)
    (pl  . package-list-packages)
    (pr  . package-refresh-contents)
    (clhs . hyperspec-lookup)
    (save! . save-excursion)))

(def def-alias (symbol alias)
  "Define `alias' as alias for `symbol'."
  (defalias alias symbol))

(defm alias (alias fun)
  `(defalias ',alias ',fun))

(def def-aliases (table)
  "Define the aliases."
  (cl-loop for (key . val) in table do
           (defalias key val)))

(def-aliases alias-table)

(defmacro call! (&rest functions)
  "Call the functions in FUNCTIONS, interactively."
  `(progn
     ,@(cl-loop for function in functions
                collect `(call-interactively ',function))))


;;; kurji

(cl-defmacro defcmd (name (&rest args) docstring &rest body)
  `(def ,name (,@args)
     ,docstring
     (interactive)
     ,@body))

(defm def-interactive (name &rest body)
  "Define a interactive function"
  `(def ,name () (interactive) ,@body))

(defm def-find (name path)
  "Define an interactive function for finding files."
  `(def-interactive ,name (find-file (ef ,path))))

(def other-window-1 (&optional arg)
  "Switch to other window, backwards."
  (interactive "p")
  (other-window (- arg)))

(defcmd swap-windows (direction)
  "Swap windows to direction"
  (let ((win-list (window-list)))
    (when (>= (length win-list) 2)
      (let* ((window-1 (cl-first win-list))
             (window-2 (cl-ecase direction
                         ((up left) (cl-first (last win-list)))
                         ((down right) (cl-second win-list))))
             (buffer-1 (window-buffer window-1))
             (buffer-2 (window-buffer window-2))
             (start-1 (window-start window-1))
             (start-2 (window-start window-2))
             (point-1 (window-point window-1))
             (point-2 (window-point window-2)))
        (set-window-buffer window-1 buffer-2)
        (set-window-buffer window-2 buffer-1)
        (set-window-start window-1 start-2)
        (set-window-start window-2 start-1)
        (set-window-point window-1 point-1)
        (set-window-point window-2 point-2)
        (other-window (cl-ecase direction
                        ((up) -1)
                        ((down) 1)))))))

(defcmd swap-up ()
  "Swap windows up"
  (swap-windows 'up))

(defcmd swap-down ()
  "Swap windows down"
  (swap-windows 'down))

(def go-to-column (column)
  "Move to a column inserting spaces as necessary"
  (interactive "nColumn: ")
  (move-to-column column t))

(def insert-until-last (string)
  "Insert string until column"
  (let* ((end (save-excursion
                (forward-line)
                (end-of-line)
                (current-column)))
         (count (if (not (zerop (current-column)))
                    (- end (current-column))
                  end)))
    (dotimes (c count)
      (ignore c)
      (insert string))))

(defcmd insert-equals ()
  "Insert equals until the same column number as last line"
  (insert-until-last "="))

(defcmd insert-backticks ()
  "Insert three backticks for Markdown use"
  (if (region-active-p)
      (when (> (region-end) (region-beginning))
        (save-excursion (goto-char (region-beginning))
                        (insert "```\n"))
        (save-excursion (goto-char (region-end))
                        (insert "```"))
        (deactivate-mark))
    (progn
      (insert "``````")
      (backward-char 3))))

(defcmd insert-hyphens ()
  "Insert hyphens until the same column number as last line"
  (insert-until-last "-"))

(def insert-anchor ()
  (interactive "p")
  (insert "<a name=\"\"></a>")
  (backward-char 6))

(def zsh-path (path)
  (let ((dir (ef "~/Developer/etc/zsh/")))
    (concat dir path)))

(def org-path (path)
  (let ((dir (ef "~/org/")))
    (concat dir path)))

(def stumpwm-path (path)
  (let ((dir (ef "~/Developer/src/lisp/stumpo/")))
    (concat dir path)))

(def my-doom-path (path)
  (let ((dir (ef "~/Developer/etc/doom/")))
    (concat dir path)))


;;; fancu

(def paths (path)
  "Return all regular files under PATH."
  (directory-files (expand-file-name path) t "[^.|..]"))

(def directories (path)
  "Return all directories under PATH."
  (remove nil
          (mapcar (λ (path)
                    (when (file-directory-p path)
                      (file-name-as-directory path)))
                  (paths path))))

(def add-to-load-path (paths)
  "Add PATHS to load-path."
  (dolist (path paths)
    (add-to-list 'load-path path)))

(defcmd refresh-paths ()
  "Re-read the load path searching for new files and directories."
  (add-to-load-path (append (directories (doom-dir ""))
                            (directory-files (my-doom-path "lib/") t "[^.|..]"))))

;; (refresh-paths)

(def file-string (file)
  "File to string function"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(def compute-fill-characters ()
  "Return the amount of fill characters needed to create a comment line."
  (let ((comment-char-length (length sp-comment-char)))
    (- fill-column comment-char-length)))

(def get-fill-characters ()
  "Return the fill characters."
  (make-string (compute-fill-characters) ?—))

(def get-comment-line ()
  "Get the comment line for the current mode."
  (cat sp-comment-char (get-fill-characters)))

(def get-separator-lines ()
  "Get the separator lines for the current mode."
  (cat (get-comment-line) "
" sp-comment-char " "))

(def insert-comment-line ()
  "Insert an appropriate separator for the current major mode."
  (interactive)
  (let ((lisp-separator "
;;; ")
        (etc-separator (get-separator-lines)))
    (beginning-of-line)
    (open-line 1)
    (cl-case major-mode
      (lisp-mode (insert lisp-separator))
      (emacs-lisp-mode (insert lisp-separator))
      (t (insert etc-separator)))))

(def switch-theme (theme)
  "Show a completing prompt for changing the theme."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(def mark-thing (start end &optional arg)
  "Mark from point to anywhere"
  (interactive "p")
  (if (not mark-active)
      (progn
        (funcall start)
        (push-mark)
        (setq mark-active t)))
  (funcall end arg))

(def mark-line (&optional arg)
  "Mark the current line."
  (interactive "p")
  (mark-thing 'beginning-of-line 'end-of-line))

(def mark-line-forward (&optional arg)
  "Mark the current line then move forward."
  (interactive "p")
  (mark-thing 'beginning-of-line 'forward-line))

(def mark-line-backward ()
  "Mark the current line then move backward."
  (interactive "p")
  (cond ((and (region-active-p)
              (= (current-column) 0))
         (forward-line arg))
        ((and (not (region-active-p))
              (= (current-column) 0))
         (mark-thing 'point 'previous-line))
        (t (mark-thing 'end-of-line 'beginning-of-line))))

(def mark-to-bol ()
  "Create a region from point to beginning of line"
  (interactive "p")
  (mark-thing 'point 'beginning-of-line))

(def mark-to-eol ()
  "Create a region from point to end of line"
  (interactive "p")
  (mark-thing 'point 'end-of-line))

(def mark-defun-backward (&optional arg)
  "Mark the previous function definition."
  (interactive "p")
  (mark-defun (- arg)))

(def mark-defun-forward ()
  "Mark the next function definition."
  (interactive "p")
  (call! mark-defun exchange-point-and-mark))

(def indent-dwim ()
  "Indent, do what I mean."
  (interactive "p")
  (if (region-active-p)
      (call-interactively 'indent-region)
    (save-excursion
      (mark-line)
      (call-interactively 'indent-region))))

(def copy-function ()
  "Put the current function definition to the kill ring."
  (interactive "p")
  (save-excursion
    (call! mark-defun copy-region-as-kill)))

(def delete-function ()
  "Delete the current function definition without appending to the kill ring."
  (interactive "p")
  (save-excursion
    (call! mark-defun delete-region)))

(def delete-dwim (&optional arg)
  "Delete, do what I mean."
  (interactive "p")
  (if (region-active-p)
      (progn
        (delete-region (region-beginning) (region-end))
        (deactivate-mark t))
    (delete-char arg)))

(def change-sexp (&optional args)
  "Like SP-KILL-SEXP but go to insert mode afterwards."
  (interactive "p")
  (sp-kill-sexp (or args 1))
  (call-interactively 'evil-insert))

(def backward-change-sexp (&optional args)
  "Like SP-BACKWARD-KILL-SEXP but go to insert mode afterwards."
  (interactive "p")
  (sp-backward-kill-sexp (or args 1))
  (call-interactively 'evil-insert))

(def elisp-eval-dwim ()
  "Eval the current or region."
  (interactive "p")
  (if (region-active-p)
      (call-interactively 'eval-region)
    (call-interactively 'eval-defun)))

(defcmd find-version-file ()
  "Find the file `version.sexp' in the current directory."
  (let* ((directory (file-name-directory buffer-file-name))
         (version-file (cat directory "version.sexp")))
    (find-file version-file)))

(defcmd open-directory ()
  "Open the current directory in the file manager."
  (shell-command "o"))

(defcmd save-windows ()
  "Save the window configuration."
  (call-interactively 'burly-bookmark-windows))

(defcmd restore-windows ()
  "Restore the windown configuration."
  (call-interactively 'burly-open-bookmark))

(defcmd clear-buffer ()
  "Clear the current buffer according to the major mode."
  (cl-case major-mode
    (slime-repl-mode (call! slime-repl-clear-buffer))
    (sly-repl-mode (call! sly-repl-clear-buffer))))

(defcmd clear-output ()
  "Clear the output according to the major mode."
  (cl-case major-mode
    (slime-repl-mode (call! slime-repl-clear-output))
    (sly-repl-mode (call! sly-repl-clear-output))))

(def format-date (format)
  "Return a date string according to FORMAT."
  (let ((system-time-locale "eo.utf8"))
    (insert (format-time-string format))))

(defcmd insert-date-and-time ()
  "Insert the current date."
  (format-date "%Y-%m-%d %H:%M:%S %z"))

(defcmd insert-date ()
  "Insert the current date, only."
  (let ((fmt "%Y-%m-%d"))
    (cond ((and (eql major-mode 'org-mode)
                (= (current-column) 0))
           (insert "* ")
           (format-date fmt))
          (t (format-date fmt)))))

(defcmd insert-day-and-date ()
  "Insert the current day and date."
  (format-date "%a %Y-%m-%d"))

(defm def-pairs (pairs)
  "Define functions for pairing. PAIRS is an alist of (NAME . STRING) conses,
where NAME is the function name that will be created and STRING is a
single-character string that marks the opening character.

  (def-pairs ((paren . \"(\")
              (bracket . \"[\"))

defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,respectively."
  `(progn
     ,@(cl-loop for (key . val) in pairs
                collect
                `(def ,(read (concat "wrap-with-" (prin1-to-string key) "s"))
                     (&optional arg)
                   (interactive "p")
                   (sp-wrap-with-pair ,val)))))

(def-pairs ((paren        . "(")
            (bracket      . "[")
            (brace        . "{")
            (single-quote . "'")
            (double-quote . "\"")))

(defvar avoid-window-regexp "^[0-9]$")

(defcmd my-other-window (&optional arg)
  "Similar to `other-window', but try to avoid windows whose buffers
match `avoid-window-regexp'"
  (let* ((window-list (delq (selected-window)
                            (if (and arg (< arg 0))
                                (reverse (window-list))
                              (window-list))))
         (filtered-window-list
          (cl-remove-if
           (λ (w)
             (string-match-p avoid-window-regexp
                             (buffer-name (window-buffer w))))
           window-list)))
    (if filtered-window-list
        (select-window (car filtered-window-list))
      (and window-list
           (select-window (car window-list))))))

(defcmd my-other-window-1 ()
  "Like `my-other-window' but go backwards."
  (my-other-window -1))

(defcmd save-all-buffers ()
  "Save some buffers."
  (save-some-buffers t))

(defcmd switch-to-other-buffer ()
  "Switch to the other buffer."
  (switch-to-buffer (other-buffer)))

(defcmd yank-buffer-to-clipboard ()
  "Put the entire buffer to the system clipboard."
  (shell-command-on-region (point-min)
                           (point-max)
                           "pbcopy"))

(defcmd yank-region-to-clipboard ()
  "Put the region to the system clipboard."
  (shell-command-on-region (region-beginning)
                           (region-end)
                           "pbcopy"))

(defcmd yank-defun-to-clipboard ()
  "Put the function to the system clibboard."
  (save-excursion
    (call! mark-defun-backward yank-region-to-clipboard)))

(defcmd indent-marked-files ()
  "Indent the marked files in a dired bufffer. https://stackoverflow.com/questions/2551632/how-to-format-all-files-under-a-dir-in-emacs"
  (dolist (file (dired-get-marked-files))
    (find-file file)
    (indent-region (point-min) (point-max))
    (save-buffer)))

(def csv-to-org-table (fname)
  "Convert a CSV file to an ORG table. https://stackoverflow.com/questions/55598919/creating-org-tables-from-the-results-of-a-code-block"
  (interactive "fCSV to convert: ")
  (let ((result '("|-\n")))
    (with-temp-buffer
      (save-excursion (insert-file-contents-literally fname))
      (while (and (not (eobp)) (re-search-forward "^\\(.+\\)$" nil t nil))
        (push (concat "|" (replace-regexp-in-string "," "|" (match-string 1)) "|\n")
              result))
      (push '"|-\n" result))
    (concat (seq-mapcat #'identity (reverse result)))))


;;; pinka

(def block-comment (beg end)
  "Insert a block comment around region. See https://emacsninja.com/posts/forbidden-emacs-lisp-knowledge-block-comments.html"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (insert (format "#@%d " (+ (- end beg) 2)))
      (goto-char (point-max))
      (insert "\037"))))


;;; zoi gy.workspaces.gy.

(defcmd my-current-directory ()
  "Return the relative default directory."
  (file-name-directory
   (or (buffer-file-name)
       (expand-file-name "~/"))))
(def-alias 'my-current-directory 'my-cwd)

(defcmd with-new-workspace-cwd (&optional fn)
  "Call FN in a new workspace, from the current directory."
  (let ((dir (my-current-directory)))
    (+workspace/new)
    (switch-to-buffer "*scratch*")
    (cd dir)
    (when fn (funcall fn))))

(defcmd with-new-workspace-home (&optional fn)
  "Call FN in a new workspace, from the home directory."
  (let ((dir (expand-file-name "~/")))
    (+workspace/new)
    (switch-to-buffer "*scratch*")
    (cd dir)
    (when fn (funcall fn))))

(defcmd new-workspace-file ()
  "Create a workspace with a temporary file in it"
  (with-new-workspace-cwd
   (λ ()
     (let* ((directory (ef "~/Desktop/t/"))
            (temp (make-temp-name "scratch"))
            (path (cat directory temp)))
       (find-file path)))))

(defcmd my-vterm ()
  "Run a vterm in the current directory."
  (vterm t))

(defcmd my-doom-reload ()
  "Reload Doom in a new workspace."
  (with-new-workspace-home
   (λ ()
     (doom/reload)
     (+workspace/display))))

(defcmd my-delete-frame ()
  "Delete a frame without confirmation."
  (delete-frame (selected-frame) t))

(defcmd my-switch-workspace ()
  "Prompt switch to a workspace."
  (call! +workspace/switch-to)
  (+workspace/display))

(defcmd my-save-workspace ()
  "Save the current workspace."
  (let ((name persp-last-persp-name))
    (when name
      (+workspace/save name))))

(defcmd my-vterm-cwd ()
  "Open vterm for the current directory in a new workspace."
  (with-new-workspace-cwd
   (λ ()
     (+vterm/here t))))

(defcmd my-vterm-home ()
  "Open vterm for the home directory in a new workspace."
  (with-new-workspace-home
   (λ ()
     (+vterm/here t))))

(defmacro defcwd (name function)
  "Define a macro for calling functions with the current directory."
  `(defcmd ,name ()
     ""
     (let* ((file-name buffer-file-name)
            (directory (if file-name
                           (file-name-directory file-name)
                         nil)))
       (when directory
         (cd directory))
       (call! ,function))))

(defcwd my-find-file find-file)
(defcwd my-dirvish dirvish)

(defcmd my-find-file-cwd (&optional arg)
  "Find file from the current directory in a new workspace."
  (with-new-workspace-cwd
   (λ ()
     (if arg
         (find-file arg)
       (call! find-file)))))

(defcmd my-dired-cwd (&optional arg)
  "Call Dired in a new workspace."
  (with-new-workspace-cwd
   (λ ()
     (call! dirvish))))

(defcmd my-find-file-home ()
  "Find file from the home directory in a new workspace."
  (with-new-workspace-home
   (λ ()
     (call! find-file))))

(defcmd my-file-manager ()
  "Open the file manager for the current directory."
  (async-shell-command
   (format "open -a finder '%s'" default-directory)))

(defcmd my-lisp-repl ()
  "Open a Lisp buffer in a new workspace."
  (with-new-workspace-cwd
   (λ ()
     (sly))))

(defcmd my-other-lisp-repl ()
  "Open a Lisp buffer in a new workspace."
  (with-new-workspace-cwd
   (λ ()
     (sly "sbcl"))))

(defcmd my-kill-window-buffer ()
  "Kill current buffer and delete the window and workspace."
  (kill-current-buffer)
  (+workspace/close-window-or-workspace))

(defcmd my-kill-window-buffers ()
  "Kill all the buffers and delete the windows and workspaces."
  (let ((buffers (cl-mapcar 'window-buffer (window-list)))
        (last-workspace +workspace--last))
    (call! +workspace/kill)
    (cl-mapc 'kill-buffer buffers)
    (+workspace/switch-to last-workspace)))

(defcmd my-dired-workspace (arg)
  "Open Dired."
  (with-new-workspace-home
   (λ ()
     (dirvish-dwim arg))))

(defcmd my-dired-workspace-cwd (&optional arg)
  "Open Dired for the current buffer in a new workspace."
  (with-new-workspace-home
   (λ ()
     (dirvish-dwim default-directory))))

(defcmd my-dired-workspace-home (&optional arg)
  "Open Dired for the current buffer in a new workspace."
  (with-new-workspace-home
   (λ ()
     (dirvish-dwim (expand-file-name "~")))))

(def get-next-workspace ()
  "Return the name of the next workspace, relative to the current one."
  (let* ((workspace persp-last-persp-name)
         (workspaces persp-names-cache)
         (workspaces-length (length workspaces))
         (position (+ (cl-position workspace workspaces :test #'equal) 1)))
    (if (< position workspaces-length)
        (nth position workspaces)
      (nth (- position 2) workspaces))))

(def get-previous-workspace ()
  "Return the name of the previous workspace, relative to the current one."
  (let* ((workspace persp-last-persp-name)
         (workspaces persp-names-cache)
         (position (cl-position workspace workspaces :test #'equal)))
    (if (= position 1)
        workspace
      (nth (- position 1) workspaces))))

(defcmd my-workspace-kill-forward ()
  "Kill the current workspace, then move to the workspace on the right."
  (let ((next-workspace (get-next-workspace)))
    (call! +workspace/kill)
    (+workspace/switch-to next-workspace)))

(defcmd my-workspace-kill-backward ()
  "Kill the current workspace, then move to the workspace on the left. "
  (let ((previous-workspace (get-previous-workspace)))
    (call! +workspace/kill)
    (+workspace/switch-to previous-workspace)))

(def saved-workspace-names ()
  "Return all the saved workspaces names."
  (persp-list-persp-names-in-file
   (expand-file-name +workspaces-data-file persp-save-dir)))

(def workspace-names ()
  "Return all the workspace names."
  (let ((main +workspaces-main))
    (remove main (sort (saved-workspace-names)))))

(defvar *workspace-blacklist-regexp*
  "\\(\\(vterm\\|sly\\|workspaces\\|ttt\\)-*\\|.*scratch.*\\|#[0-9]*\\|.*-2\\)"
  "The regexp for workspaces that will not be loaded.")

(defvar *workspace-garbage-collect-regexp*
  "^\\(#[0-9]*\\|main\\)$"
  "The regexp for workspaces that can be GC'd.")

(defvar *workspace-garbage-collect-modes*
  '(vterm-mode)
  "The list of major modes that can be GC'd.")

(defcmd my-garbage-collect-workspaces ()
  "Remove the workspaces that can be GC'd."
  (dolist (workspace persp-names-cache)
    (when (string-match *workspace-garbage-collect-regexp* workspace)
      (+workspace-kill workspace t)))
  (+workspace/switch-to-final)
  (+workspace/display))

(defcmd my-load-all-workspaces ()
  "Load all saved workspaces."
  (let* ((list (workspace-names))
         (cache persp-names-cache)
         (workspaces (cl-set-difference list cache :test #'equal))
         (names (cl-remove-if (λ (workspace)
                                (string-match *workspace-blacklist-regexp* workspace))
                              workspaces)))
    (dolist (name names)
      (+workspace/load name))
    (my-garbage-collect-workspaces)))

(def current-workspace-names ()
  "Return all the workspaces that are currently in use, except those present in
the blacklist."
  (cl-remove-if (λ (workspace)
                  (string-match workspace *workspace-blacklist-regexp*))
                persp-names-cache))

(defcmd my-save-all-workspaces ()
  "Save all the open workspaces."
  (let ((current-workspace persp-last-persp-name))
    (cl-loop for workspace in (current-workspace-names)
             do (save-excursion
                  (+workspace-switch workspace)
                  (my-save-workspace))
             finally (+workspace-switch current-workspace))))

(defcmd my-new-workspace-cwd ()
  "Create a new workspace with the current directory."
  (with-new-workspace-cwd))

(defcmd my-new-workspace-home ()
  "Create a new workspace with the home directory."
  (with-new-workspace-home))

(defcmd my-goto-first-line ()
  "Run evil-goto-first-line."
  (if (and (= (line-number-at-pos) 1)
           (not (= (point) 0)))
      (goto-char 0)
    (call! evil-goto-first-line)))

(defcmd my-goto-line ()
  "Run evil-goto-line."
  (if (= (point) (point-max))
      (call! evil-scroll-line-to-bottom)
    (call! evil-goto-line)))

(defm def-opener (program type)
  "Define a command to open the TYPE-version of the current buffer."
  (let ((cmd-name (read (format "my-%s-%s-file" program type))))
    `(defcmd ,cmd-name ()
       ""
       (let* ((buffer-name (buffer-file-name))
              (name (file-name-sans-extension buffer-name))
              (path (format "%s.%s" name ,type)))
         (when (file-exists-p path)
           (async-shell-command (format "%s \"%s\"" ,program path)))))))

(def-opener "open" "pdf")
(def-opener "open" "odt")
(def-opener "open" "html")
(def-opener "open" "docx")
(def-opener "open" "rtf")

(def-opener "drop" "pdf")
(def-opener "drop" "odt")
(def-opener "drop" "html")
(def-opener "drop" "docx")
(def-opener "drop" "rtf")
(def-opener "drop" "org")
(def-opener "drop" "txt")

(defm def-converter (type)
  "Define a command to convert a file of TYPE to PDF."
  (let ((cmd-name (read (format "my-convert-%s-to-pdf" type))))
    `(defcmd ,cmd-name ()
       ""
       (let* ((file-name (buffer-file-name))
              (name (file-name-sans-extension file-name))
              (path (format "%s.%s" name ,type)))
         (when (file-exists-p file-name)
           (async-shell-command (format "pdf %s" path)))))))

(def-converter "odt")
(def-converter "docx")
(def-converter "html")
(def-converter "rtf")

(defcmd my-switch-to-sly-mrepl-window ()
  "Switch to the SLY window if present."
  (let ((window (get-buffer-window (sly-mrepl--find-create (sly-connection)))))
    (select-window window)))

(defcmd my-edit-histfile ()
  "Edit the HISTFILE."
  (find-file (getenv "HISTFILE")))

(def my-map-windows (fn)
  "Apply FN to all the active windows."
  (let ((win (selected-window)))
    (cl-loop for w in (window-list)
             do (progn
                  (select-window w)
                  (funcall fn)))
    (select-window win)))

(def my-goto-bottom ()
  "Go to the bottom part of the window."
  (goto-char (point-max))
  (recenter -1))

(defcmd my-scroll-all-windows-to-bottom ()
  "Move all windows to bottom."
  (my-map-windows
   (λ ()
     (my-goto-bottom))))

(defcmd my-expand-all-windows ()
  "Expand all Org windows."
  (my-map-windows
   (λ ()
     (cl-case major-mode
       ((org-mode)
        (call! org-show-all)))
     (my-goto-bottom))))

(defcmd my-collapse-all-windows ()
  "Collapse all Org windows."
  (my-map-windows
   (λ ()
     (cl-case major-mode
       ((org-mode)
        (+org/close-all-folds)))
     (goto-char (point-min)))))

(def my-auto-rename-workspace ()
  "Rename workspace to buffer name."
  (+workspace/rename (buffer-name)))

(defcmd my-rename-all-workspaces ()
  "Rename all workspaces."
  (my-map-windows
   (λ ()
     (my-auto-rename-workspace))))

(def my-map-buffers (fn)
  "Apply FN to all the active windows."
  (let ((buf (current-buffer)))
    (cl-loop for b in (buffer-list)
             do (progn
                  (switch-to-buffer b)
                  (funcall fn)))
    (switch-to-buffer buf)))

(defvar *kill-buffer-whitelist-regexp*
  "\\(\\(vterm\\|dired\\|sly-mrepl\\)-mode\\)"
  "The whitelist regexp for buffers to be GC'd.")

(def my-kill-this-buffer ()
  "Kill this buffer without prompting."
  (let ((kill-buffer-query-functions nil))
    (when (string-match *kill-buffer-whitelist-regexp*
                        (prin1-to-string major-mode))
      (kill-this-buffer))))

(defcmd my-garbage-collect-buffers ()
  "Kill all buffers that are part of the kill whitelist."
  (my-map-buffers (λ () (my-kill-this-buffer)))
  (+workspace/display))

(defcmd my-garbage-collect ()
  "Do an aggressive GC."
  (let ((buf (current-buffer)))
    (my-garbage-collect-workspaces)
    (my-garbage-collect-buffers)
    (switch-to-buffer buf)))

(def my-switch-or-load-workspace (name)
  "Switch to workspace or load it if it doesn't exist, yet."
  (interactive
   (list
    (completing-read "Switch to workspace: " (saved-workspace-names))))
  (if (member name persp-names-cache)
      (+workspace/switch-to name)
    (+workspace/load name)))

(defcmd my-org-capture ()
  "Do org-capture"
  (org-capture nil "a"))

(defcmd my-open-a-org ()
  "Open a.org file"
  (with-new-workspace-home
   (find-file (ef "~/org/a.org"))))

(defcmd my-comment-yank ()
  "Copy the region and comment it out."
  (when (region-active-p)
    (exchange-point-and-mark)
    (call! copy-region-as-kill comment-dwim)
    (forward-line 1)))

(defcmd my-yank-definition ()
  "Copy the definition to the kill ring and comment it out."
  (call! mark-defun my-comment-yank))

(defcmd my-yank-expression ()
  "Copy the expression to the kill ring and comment it out."
  (call! mark-sexp my-comment-yank))

(defcmd my-insert-tab ()
  "Insert a tab character."
  (insert "	"))

(defcmd my-flush-lines ()
  "Delete empty lines"
  (flush-lines "^$"))

(defcmd my-jump-item ()
  "Go to the matching pairing char."
  (cond ((looking-at "[[]")
         (forward-char 1)
         (if (save-excursion (end-of-line) (looking-at "[]]"))
             (sp-up-sexp 1)
           (progn (sp-up-sexp 1)
                  (backward-char 1))))
        ((looking-at "[]]")
         (sp-backward-up-sexp 1))
        (t (call! evil-jump-item))))

(defcmd my-dirvish ()
  "Open dirvish on the current directory"
  (dirvish-dwim default-directory))

(defcmd my-org-roam-capture ()
  "Run org-roam-capture in a new workspace."
  (with-new-workspace-cwd
   (call! org-roam-capture)))

(defcmd my-start-of-line ()
  "Go to the almost start of line. Used with Org."
  (beginning-of-line)
  (forward-char 2))

(defcmd my-end-of-line ()
  "Go to the almost end of line. Used with Org."
  (end-of-line))

(defcmd my-mark-org-line ()
  "Mark the current org line."
  (move-to-column 0)
  (mark-thing 'org-beginning-of-line 'org-end-of-line))

(defm def-wrapper (name string)
  "Define a command for wrapping."
  (let ((cmd-name (read (format "my-wrap-with-%s" name))))
    `(defcmd ,cmd-name ()
       "Wrap the region with a call to meh."
       (when (region-active-p)
         (wrap-with-parens)
         (insert (cat ,string " "))))))

(def-wrapper logical-and "∧")
(def-wrapper logical-or "∨")
(def-wrapper logical-not "¬")
(def-wrapper lambda "λ")

(defcmd my-open-lisp-scratch-file ()
  "Open the lisp scratch file."
  (with-new-workspace-home
   (find-file "~/Desktop/t.lisp")))

(defcmd my-delete-line ()
  "Delete the current line."
  (call! mark-line delete-region)
  (forward-line 1))

(defcmd my-kill-line ()
  "Kill the current line."
  (call! mark-line kill-region)
  (forward-line 1))

(defcmd my-find-file-new-workspace ()
  "Open the current file in a new workspace."
  (let ((file (buffer-file-name)))
    (my-find-file-cwd file)))

(def register-present-p (register)
  "Return true if `register' is present in the register alist."
  (assoc register register-alist))

(def delete-register (register)
  "Destructively remove `register' from the register alist."
  (setq register-alist
        (assq-delete-all register register-alist)))

(def register-name ()
  "Return the register name of the current workspace or window."
  (read (cat ":" persp-last-persp-name)))

(def workspace-delete-register (&rest args)
  "Advice to run before killing a workspace"
  (let ((register (register-name)))
    (when (register-present-p register)
      (delete-register register))))

(advice-add 'my-workspace-kill-forward :before #'workspace-delete-register)
(advice-add 'my-workspace-kill-backward :before #'workspace-delete-register)
(advice-add 'my-switch-or-load-workspace :after #'workspace-delete-register)

(defcmd my-toggle-zoom-window ()
  "Toggle the zoom state of a window."
  (let ((register (register-name)))
    (if (register-present-p register)
        (progn
          (jump-to-register register)
          (delete-register register))
      (progn
        (window-configuration-to-register register)
        (doom/window-maximize-buffer)))))

(defcmd my-load-etc-workspaces ()
  "Load the common `etc' workspaces."
  (dolist (w '("etc/doom" "etc/nix" "etc/zsh"))
    (my-switch-or-load-workspace w)))
(def-alias 'my-load-etc-workspaces 'lwe)

(defcmd my-load-org-workspaces ()
  "Load the common `org' workspaces."
  (dolist (w '("org/t" "org/lisp" "org/vedainc"))
    (my-switch-or-load-workspace w)))
(def-alias 'my-load-org-workspaces 'lwo)

(defcmd my-load-vedainc-workspaces ()
  "Load the common `vedainc' workspaces."
  (dolist (w '("vedainc/docs" "vedainc/projects" "vedainc/notes"))
    (my-switch-or-load-workspace w)))
(def-alias 'my-load-vedainc-workspaces 'lwv)

(defcmd my-dired-goto-desktop ()
  "Go to the Desktop directory."
  (dirvish-dwim "~/Desktop"))

(defcmd my-dired-goto-desktop ()
  "Go to the Desktop directory."
  (dirvish-dwim "~/Desktop"))

(defcmd my-dired-goto-downloads ()
  "Go to the Downloads directory."
  (dirvish-dwim "~/Downloads"))

(defcmd my-dired-goto-documents ()
  "Go to the Documents directory."
  (dirvish-dwim "~/Documents"))

(defcmd my-dired-goto-pictures ()
  "Go to the Pictures directory."
  (dirvish-dwim "~/Pictures"))

(defcmd my-dired-goto-movies ()
  "Go to the Movies directory."
  (dirvish-dwim "~/Movies"))

(defcmd my-dired-goto-music ()
  "Go to the Music directory."
  (dirvish-dwim "~/Music"))

(defcmd my-dired-goto-developer ()
  "Go to the Developer directory."
  (dirvish-dwim "~/Developer"))

(defcmd my-dired-goto-applications ()
  "Go to the Applications directory."
  (dirvish-dwim "~/Applications"))

(defcmd sly-lispworks ()
  "Run SLY with LispWorks."
  (sly "lispworks"))

(defcmd sly-sbcl ()
  "Run SLY with SBCL."
  (sly "sbcl"))

(defcmd my-org-sort-entries-by-schedule ()
  "Sort entries in a org buffer by schedule."
  (call! mark-whole-buffer)
  (org-sort-entries nil ?s)
  (call! my-flush-lines))

(defcmd my-org-sort-entries-by-deadline ()
  "Sort entries in a org buffer by deadline."
  (call! mark-whole-buffer)
  (org-sort-entries nil ?d)
  (call! my-flush-lines))

(defcmd my-org-sort-entries-by-reverse-schedule ()
  "Sort entries in a org buffer by reverse schedule."
  (call! mark-whole-buffer)
  (org-sort-entries nil ?S)
  (call! my-flush-lines))

(defcmd my-org-sort-entries-by-reverse-deadline ()
  "Sort entries in a org buffer by reverse deadline."
  (call! mark-whole-buffer)
  (org-sort-entries nil ?D)
  (call! my-flush-lines))

(defcmd my-window-split ()
  "Conditionally split window horizontally."
  (if current-prefix-arg
      (call! evil-window-split)
    (call! +evil/window-split-and-follow)))

(defcmd my-window-vsplit ()
  "Conditionally split window vertically."
  (if current-prefix-arg
      (call! evil-window-vsplit)
    (call! +evil/window-vsplit-and-follow)))

(defcmd my-insert-newline-above ()
  "Insert a newline above and move point to it."
  (let ((column (current-column)))
    (call! +evil/insert-newline-above)
    (unless (zerop column)
      (previous-line 1))
    (call! evil-insert)))

(defcmd my-insert-newline-below ()
  "Insert a newline below and move point to it."
  (let ((column (current-column)))
    (call! +evil/insert-newline-below)
    (next-line 1)
    (call! evil-insert)))

(defcmd my-org-table-fill ()
  "Fill the Org table"
  (call! org-table-convert ftable-fill org-table-convert))

(defcmd my-transpose-lines-backward ()
  "Transpose lines backward."
  (save! (transpose-lines 1))
  (previous-line 1))

(defcmd my-transpose-lines-forward ()
  "Transpose lines forward."
  (next-line 1)
  (save! (transpose-lines 1)))
