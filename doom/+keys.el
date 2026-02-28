;;; $DOOMDIR/+keys.el -*- lexical-binding: t; -*-


;;; kampu

(setq doom-localleader-key ",")

(def bind-mac-keys ()
  "Bind macOS keys."
  (setq mac-option-modifier 'none))

(defm map-company-keys (map)
  "Map tab to company-complete in MAP."
  `(map! :map ,map
         :i "<tab>" 'company-complete
         :i "TAB"   'company-complete))

(def bind-company-keys ()
  "Bind the company keys."
  (map-company-keys global-map))

(defm map-workspace-switch-keys (&rest maps)
  "Map the workspace switching keys in MAP."
  `(progn
     ,@(cl-loop
        for map in maps
        collect `(map! :map ,map
                       :g "s-1" '+workspace/switch-to-0
                       :g "s-2" '+workspace/switch-to-1
                       :g "s-3" '+workspace/switch-to-2
                       :g "s-4" '+workspace/switch-to-3
                       :g "s-5" '+workspace/switch-to-4
                       :g "s-6" '+workspace/switch-to-5
                       :g "s-7" '+workspace/switch-to-6
                       :g "s-8" '+workspace/switch-to-7
                       :g "s-9" '+workspace/switch-to-8
                       :g "s-0" '+workspace/switch-to-final))))

(def bind-workspace-switch-keys ()
  "Bind the workspace keys."
  (map-workspace-switch-keys global-map
                             vterm-mode-map
                             sly-mrepl-mode-map
                             treemacs-mode-map))

(defm map-workspace-keys (&rest keys)
  "Map `key' to workspace commands."
  `(progn
     ,@(cl-loop
        for key in keys
        collect `(map! :leader
                       (:prefix-map
                        (,key . "workspaces")
                        "d" '+workspace/delete
                        "g" 'my-garbage-collect-workspaces
                        "G" 'my-garbage-collect-buffers
                        "s" 'my-save-workspace
                        "S" 'my-save-all-workspaces
                        "k" 'my-switch-or-load-workspace
                        "K" 'my-load-all-workspaces
                        "o" '+workspace/other
                        "y" '+workspace/display
                        "D" '+workspace/delete
                        "," '+workspace/rename
                        "'" 'my-window-split
                        ";" 'my-window-vsplit
                        "x" 'my-workspace-kill-forward
                        "X" 'my-workspace-kill-backward
                        "w" '+workspace/close-window-or-workspace
                        "[" '+workspace/switch-left
                        "]" '+workspace/switch-right
                        "{" '+workspace/swap-left
                        "}" '+workspace/swap-right)))))

(def bind-workspace-keys ()
  "Bind the company keys."
  (map-workspace-keys "<tab>" "TAB"))

(def bind-toplevel-keys ()
  "Bind the toplevel keys."
  ;; general
  (map! :g "s-." 'doom/reload
        :g "s-d" 'my-dired-cwd
        :g "s-i" 'evil-insert-digraph
        :g "s-`" 'evil-switch-to-windows-last-buffer
        :g "s-r" 'replace-regexp
        :g "s-R" 'replace-string
        :g "s-n" 'org-roam-capture
        :g "s-t" 'my-new-workspace-cwd
        :g "s-T" 'my-new-workspace-home
        :g "s-e" 'my-collapse-all-windows
        :g "s-E" 'my-expand-all-windows
        :g "s-z" 'my-toggle-zoom-window
        :g "s-u" 'winner-undo
        :g "s-U" 'winner-redo
        :g "s-m" 'doom/window-maximize-buffer)
  ;; workspaces
  (map! :g "s-g" 'my-garbage-collect-workspaces
        :g "s-G" 'my-garbage-collect-buffers
        :g "s-s" 'my-save-workspace
        :g "s-S" 'my-save-all-workspaces
        :g "s-k" 'my-switch-or-load-workspace
        :g "s-K" 'my-load-all-workspaces
        :g "s-o" '+workspace/other
        :g "s-y" '+workspace/display
        :g "s-," '+workspace/rename
        :g "s-'" 'my-window-split
        :g "s-;" 'my-window-vsplit
        :g "s-x" 'my-workspace-kill-forward
        :g "s-X" 'my-workspace-kill-backward
        :g "s-w" '+workspace/close-window-or-workspace
        :g "s-[" '+workspace/switch-left
        :g "s-]" '+workspace/switch-right
        :g "s-{" '+workspace/swap-left
        :g "s-}" '+workspace/swap-right)
  ;; windows
  (map! :ig "s-<left>" 'evil-window-left
        :ig "s-<right>" 'evil-window-right
        :ig "s-<up>" 'evil-window-up
        :ig "s-<down>" 'evil-window-down)
  ;; main
  (map! :n "K" 'join-line ; was +lookup/documentation
        :nv "\\" 'repeat
        :n "%" 'my-jump-item
        :n "g g" 'my-goto-first-line
        :n "g c" 'evilnc-comment-operator
        :n "g C" 'evilnc-copy-and-comment-operator
        :n "G" 'my-goto-line
        :n "[ l" 'my-transpose-lines-backward
        :n "] l" 'my-transpose-lines-forward
        :n "[ <" 'beginning-of-buffer
        :n "] >" 'end-of-buffer
        :n "[ i" 'my-insert-newline-above
        :n "] i" 'my-insert-newline-below
        :n "?" 'evil-ex-search-backward
        :n "/" 'evil-ex-search-forward
        :n "(" 'sp-down-sexp
        :v "(" 'wrap-with-parens
        :n ")" 'sp-up-sexp
        :n "{" 'sp-backward-up-sexp
        :v "{" 'wrap-with-braces
        :n "}" 'sp-backward-down-sexp
        :v "[" 'wrap-with-brackets
        :v "∧" 'my-wrap-with-logical-and
        :v "∨" 'my-wrap-with-logical-or
        :v "¬" 'my-wrap-with-logical-not
        :v "λ" 'my-wrap-with-lambda
        :n "_" 'my-delete-line))

(def bind-prog-mode-keys ()
  (map! :map prog-mode-map
        :n "<DEL>" 'backward-delete-char
        :n "<RET>" 'newline-and-indent))
(add-hook! prog-mode #'bind-prog-mode-keys)

(def bind-main-keys ()
  "Bind the main leader keys."
  (map! :leader
        :desc "Execute extended command"  "<SPC>" 'execute-extended-command
        :desc "Search buffer"             "/" '+default/search-buffer

        :n "," nil
        :n "." nil
        :n "d" nil
        :n "," nil
        :n "*" nil

        :desc "Eval expression "          ":" 'pp-eval-expression
        :desc "Shell command"             "!" 'shell-command
        :desc "Shell command on region"   "|" 'shell-command-on-region
        :desc "Comment DWIM"              ";" 'comment-dwim
        :desc "Vterm toggle"              "'" 'my-vterm
        :desc "Org agenda"                "a" 'org-agenda
        :desc "Mark sexp"                 "v" 'mark-sexp
        :desc "Mark defun"                "V" 'mark-defun
        :desc "Mark buffer"               "C-v" 'mark-whole-buffer
        :desc "Select window"             "o" 'ace-window
        :desc "Delete frame"              "q f" 'delete-frame
        :desc "Maximize window"           "w m" 'doom/window-maximize-buffer
        :desc "Minimize window"           "w M" 'minimize-window
        :desc "Transpose frame"           "w t" 'transpose-frame)
  (map! :leader
        :desc "Switch to buffer"          "b b" 'consult-buffer
        :desc "Buffer menu"               "b B" 'buffer-menu
        :desc "Find file"                 "f f" 'my-find-file
        :desc "Dired"                     "f d" 'my-dirvish
        :desc "Dirvish"                   "f D" 'dirvish
        :desc "Delete file"               "f C-d" 'doom/delete-this-file
        :desc "Save all buffers"          "f S" 'save-all-buffers
        :desc "Write file"                "f w" 'write-file
        :desc "Find file at point"        "f ." 'find-file-at-point
        :desc "Find alternate file"       "f v" 'find-alternate-file
        :desc "Edit HISTFILE"             "f h" 'my-edit-histfile
        :desc "Rename uniquely"           "f r" 'rename-uniquely
        :desc "Find file literally"       "f l" 'find-file-literally
        :desc "Search project"            "p ." '+default/search-project-for-symbol-at-point
        :desc "Projectile replace regexp" "p %" 'projectile-replace-regexp
        :desc "Projectile replace"        "p $" 'projectile-replace
        :desc "Project TODO search"       "p t" 'hl-todo-occur
        :desc "Project TODO grep"         "p g" 'hl-todo-rgrep
        :desc "Find function"             "h F" 'find-function
        :desc "Find variable"             "h V" 'find-variable
        :desc "Describe face"             "h C-f" 'describe-face
        :desc "Custom variable"           "h C-v" 'doom/help-custom-variable)
  (map! :leader
        (:prefix-map
         ("<tab>" . "workspaces")
         "g" 'my-garbage-collect-workspaces
         "G" 'my-garbage-collect-buffers
         "s" 'my-save-workspace
         "S" 'my-save-all-workspaces
         "k" 'my-switch-or-load-workspace
         "K" 'my-load-all-workspaces
         "o" '+workspace/other
         "y" '+workspace/display
         "," '+workspace/rename
         "'" 'my-window-split
         ";" 'my-window-vsplit
         "x" 'my-workspace-kill-forward
         "X" 'my-workspace-kill-backward
         "w" '+workspace/close-window-or-workspace
         "[" '+workspace/switch-left
         "]" '+workspace/switch-right
         "{" '+workspace/swap-left
         "}" '+workspace/swap-right))
  (map! :leader
        (:prefix-map
         ("TAB" . "workspaces")
         "g" 'my-garbage-collect-workspaces
         "G" 'my-garbage-collect-buffers
         "s" 'my-save-workspace
         "S" 'my-save-all-workspaces
         "k" 'my-switch-or-load-workspace
         "K" 'my-load-all-workspaces
         "o" '+workspace/other
         "y" '+workspace/display
         "," '+workspace/rename
         "'" 'my-window-split
         ";" 'my-window-vsplit
         "x" 'my-workspace-kill-forward
         "X" 'my-workspace-kill-backward
         "w" '+workspace/close-window-or-workspace
         "[" '+workspace/switch-left
         "]" '+workspace/switch-right
         "{" '+workspace/swap-left
         "}" '+workspace/swap-right))
  (map! :leader
        (:prefix-map
         ("r" . "roam")
         :desc "org-roam-capture"                    "c"   'org-roam-capture
         :desc "org-roam-db-sync"                    "s"   'org-roam-db-sync
         :desc "org-roam-buffer-display-dedicated"   "R"   'org-roam-buffer-display-dedicated
         :desc "org-roam-node-insert"                "i"   'org-roam-node-insert
         :desc "org-roam-graph"                      "g"   'org-roam-graph
         :desc "org-roam-ref-find"                   "F"   'org-roam-ref-find
         :desc "org-roam-node-find"                  "f"   'org-roam-node-find

         :desc "org-roam-dailies-capture-yesterday"  "d y" 'org-roam-dailies-capture-yesterday
         :desc "org-roam-dailies-capture-today"      "d t" 'org-roam-dailies-capture-today
         :desc "org-roam-dailies-capture-tomorrow"   "d m" 'org-roam-dailies-capture-tomorrow

         :desc "org-roam-dailies-goto-yesterday"     "d Y" 'org-roam-dailies-goto-yesterday
         :desc "org-roam-dailies-goto-today"         "d T" 'org-roam-dailies-goto-today
         :desc "org-roam-dailies-goto-tomorrow"      "d M" 'org-roam-dailies-goto-tomorrow

         :desc "org-roam-dailies-capture-date"       "d d" 'org-roam-dailies-capture-date
         :desc "org-roam-dailies-goto-date"          "d D" 'org-roam-dailies-goto-date

         :desc "org-roam-dailies-find-directory"     "d -" 'org-roam-dailies-find-directory
         :desc "org-roam-dailies-goto-next-note"     "d f" 'org-roam-dailies-goto-next-note
         :desc "org-roam-dailies-goto-previous-note" "d b" 'org-roam-dailies-goto-previous-note)))

(def bind-emacs-keys ()
  "Bind Elisp keys."
  (map! :localleader
        :map emacs-lisp-mode-map
        :n "m" nil
        :desc "Macrostep"       :n "<RET>" 'macrostep-expand))

(def bind-window-keys ()
  "Bind winum keys."
  (map! :leader
        :desc "Select window 1" "1" 'winum-select-window-1
        :desc "Select window 2" "2" 'winum-select-window-2
        :desc "Select window 3" "3" 'winum-select-window-3
        :desc "Select window 4" "4" 'winum-select-window-4
        :desc "Select window 5" "5" 'winum-select-window-5
        :desc "Select window 6" "6" 'winum-select-window-6
        :desc "Select window 7" "7" 'winum-select-window-7
        :desc "Select window 8" "8" 'winum-select-window-8
        :desc "Select window 9" "9" 'winum-select-window-9
        :desc "Select window 1" "0" 'winum-select-window-0-or-10))

(def bind-smartparens-keys ()
  "Bind smartparens keys."
  (map! :leader
        (:prefix-map
         ("y" . "smartparens")
         :desc "Forward kill sexp"       "k" 'sp-kill-sexp
         :desc "Backward kill sexp"      "K" 'sp-backward-kill-sexp
         :desc "Forward change sexp"     "c" 'change-sexp
         :desc "Backward change sexp"    "C" 'backward-change-sexp
         :desc "Forward copy sexp"       "y" 'sp-copy-sexp
         :desc "Backward copy sexp"      "Y" 'sp-backward-copy-sexp
         :desc "Transpose sexp"          "t" 'sp-transpose-sexp
         :desc "Backward unwrap sexp"    "U" 'sp-backward-unwrap-sexp
         :desc "Forward unwrap sexp"     "u" 'sp-unwrap-sexp
         :desc "Emit sexp"               "e" 'sp-emit-sexp
         :desc "Raise sexp"              "r" 'sp-raise-sexp
         :desc "Forward slurp sexp"      "s" 'sp-forward-slurp-sexp
         :desc "Backward slurp sexp"     "S" 'sp-backward-slurp-sexp
         :desc "Wrap with parens"        "b" 'wrap-with-parens
         :desc "Wrap with braces"        "B" 'wrap-with-braces
         :desc "Forward barf sexp"       "f" 'sp-forward-barf-sexp
         :desc "Backward barf sexp"      "F" 'sp-backward-barf-sexp
         :desc "Absorb sexp"             "a" 'sp-absorb-sexp
         :desc "Split sexp"              "|" 'sp-split-sexp
         :desc "Join sexp"               "j" 'sp-join-sexp
         :desc "Kill whole line"         "l" 'sp-kill-whole-line
         :desc "Splice sexp"             "i" 'sp-splice-sexp
         :desc "Clone sexp"              "." 'sp-clone-sexp
         :desc "Convolute sexp"          "v" 'sp-convolute-sexp
         :desc "Add to previous sexp"    "<" 'sp-add-to-previous-sexp
         :desc "Add to next sexp"        ">" 'sp-add-to-next-sexp
         :desc "Wrap with parens"        "(" 'wrap-with-parens
         :desc "Wrap with braces"        "{" 'wrap-with-braces
         :desc "Wrap with brackets"      "[" 'wrap-with-brackets
         :desc "Wrap with ''"            "'" 'wrap-with-single-quotes
         :desc "Wrap with \"\""          "\"" 'wrap-with-double-quotes)))

(def bind-x-keys ()
  "Bind x-prefix keys."
  (map! :leader
        (:prefix-map
         ("x" . "x-menu")
         (:prefix-map ("c" . "convert")
          :desc "Convert to ODT to PDF"    "o" 'my-convert-odt-to-pdf
          :desc "Convert to DOCX to PDF"   "d" 'my-convert-docx-to-pdf
          :desc "Convert to HTML to PDF"   "h" 'my-convert-html-to-pdf
          :desc "Convert to RTF to PDF"    "r" 'my-convert-rtf-to-pdf)
         (:prefix-map ("o" . "open")
          "-" nil
          "a" nil "b" nil "c" nil "d" nil "e" nil "f" nil "g" nil "h" nil "i" nil "j" nil "k" nil "l" nil "m" nil "n" nil "o" nil "p" nil "q" nil "r" nil "s" nil "t" nil "u" nil "v" nil "w" nil "x" nil "y" nil "z" nil 
          "A" nil "B" nil "C" nil "D" nil "E" nil "F" nil "G" nil "H" nil "I" nil "J" nil "K" nil "L" nil "M" nil "N" nil "O" nil "P" nil "Q" nil "R" nil "S" nil "T" nil "U" nil "V" nil "W" nil "X" nil "Y" nil "Z" nil 
          :desc "Open PDF file"            "p" 'my-open-pdf-file
          :desc "Open ODT file"            "o" 'my-open-odt-file
          :desc "Open HTML file"           "h" 'my-open-html-file
          :desc "Open DOCX file"           "d" 'my-open-docx-file
          :desc "Open RTF file"            "r" 'my-open-rtf-file)
         (:prefix-map ("d" . "drop")
          :desc "Drop PDF file"            "p" 'my-drop-pdf-file
          :desc "Drop ODT file"            "o" 'my-drop-odt-file
          :desc "Drop HTML file"           "h" 'my-drop-html-file
          :desc "Drop DOCX file"           "d" 'my-drop-docx-file
          :desc "Drop RTF file"            "r" 'my-drop-rtf-file
          :desc "Drop ORG file"            "O" 'my-drop-org-file
          :desc "Drop TXT file"            "t" 'my-drop-txt-file)
         (:prefix-map
          ("e" . "clear")
          :desc "Clear space*"      "s" 'just-one-space
          :desc "Clear space"       "S" 'delete-horizontal-space
          :desc "Clear empty lines" "l" 'my-flush-lines
          :desc "Clear lines"       "L" 'flush-lines
          :desc "Clear region"      "r" 'delete-region
          :desc "Clear buffer"      "b" 'clear-buffer
          :desc "Clear output"      "o" 'clear-output
          )
         (:prefix-map
          ("w" . "wdired")
          :desc "Edit directory" "e" 'wdired-change-to-wdired-mode
          :desc "Complete edits" "c" 'wdired-finish-edit)
         (:prefix-map
          ("f" . "fold")
          :desc "Vimish quit"             "q" 'vimish-fold-delete-all
          :desc "Vimish avy fold"         "a" 'vimish-fold-avy
          :desc "Vimish delete fold"      "d" 'vimish-fold-delete
          :desc "Vimish delete all folds" "D" 'vimish-fold-delete-all
          :desc "Vimish create fold"      "c" 'vimish-fold
          :desc "Vimish toggle fold"      "t" 'vimish-fold-toggle
          :desc "Vimish toggle all folds" "T" 'vimish-fold-toggle-all)
         (:prefix-map
          ("s" . "sort")
          :desc "Sort lines"                   "l" 'sort-lines
          :desc "Org sort by schedule"         "s" 'my-org-sort-entries-by-schedule
          :desc "Org sort by deadline"         "d" 'my-org-sort-entries-by-deadline
          :desc "Org sort by reverse schedule" "S" 'my-org-sort-entries-by-reverse-schedule
          :desc "Org sort by reverse deadline" "D" 'my-org-sort-entries-by-reverse-deadline)
         (:prefix-map
          ("S" . "spell")
          :desc "Ispell buffer"   "b" 'ispell-buffer
          :desc "Ispell region"   "r" 'ispell-region
          :desc "Ispell language" "l" 'ispell-change-dictionary)
         (:prefix-map
          ("i" . "insert")
          "u" nil "f" nil "F" nil "i" nil "r" nil "s" nil "y" nil "p" nil
          :desc "Insert tab"            "<TAB>" 'my-insert-tab
          :desc "Insert comment line"   "l" 'insert-comment-line
          :desc "Insert from kill ring" "y" '+default/yank-pop
          :desc "Insert file"           "f" 'insert-file
          :desc "Insert char"           "c" 'insert-char
          :desc "Insert date and time"  "d" 'insert-date-and-time
          :desc "Insert date"           "D" 'insert-date
          :desc "Insert emoji"          "e" 'emojify-insert-emoji)
         (:prefix-map
          ("r" . "register")
          :desc "Point to register"     "." 'point-to-register
          :desc "Copy to register"      "c" 'copy-to-register
          :desc "Insert register"       "i" 'insert-register
          :desc "Jump to register"      "j" 'jump-to-register
          :desc "Rectangle to register" "r" 'copy-rectangle-to-register
          :desc "Show registers"        "s" 'evil-show-registers
          :desc "Window register"       "w" 'window-configuration-to-register
          :desc "Kill rectangle"        "k" 'kill-rectangle
          :desc "Delete rectangle"      "d" 'delete-rectangle
          :desc "Yank rectangle"        "y" 'yank-rectangle
          :desc "Paste rectangle"       "p" 'yank-rectangle))))

(def bind-org-keys ()
  "Bind org keys."
  (map! :map org-mode-map
        :desc "Org line above" :n "O" 'my-insert-newline-above
        :desc "Org line below" :n "o" 'my-insert-newline-below
        :desc "Org demote"     :n "S-<down>" 'org-do-demote
        :desc "Org promote"    :n "S-<up>"  'org-do-promote)
  (map! :map evil-org-mode-map
        :desc "Org line above" :n "O" 'my-insert-newline-above
        :desc "Org line below" :n "o" 'my-insert-newline-below
        :desc "Org demote"     :n "S-<down>" 'org-do-demote
        :desc "Org promote"    :n "S-<up>"  'org-do-promote)
  (map! :localleader
        :map org-mode-map
        :desc "Org meta return"     :n "<RET>" 'org-meta-return
        :desc "Org fill table"      :n "F" 'my-org-table-fill
        :desc "Org insert template" :n "S" 'org-insert-structure-template))

(def bind-sly-keys ()
  "Bind SLY keys."
  (map! :map sly-db-mode-map
        :n "Q" 'my-sly-db-quit)
  (map! :map sly-mrepl-mode-map
        :n "<up>" 'sly-mrepl-previous-input-or-button
        :n "<down>" 'sly-mrepl-next-input-or-button
        :i "<up>" 'sly-mrepl-previous-input-or-button
        :i "<down>" 'sly-mrepl-next-input-or-button
        :i "<tab>" 'sly-mrepl-indent-and-complete-symbol
        :i "TAB" 'sly-mrepl-indent-and-complete-symbol)
  (map! :map sly-mrepl-mode-map
        :i "<tab>" 'company-complete
        :i "TAB" 'company-complete)
  (map! :localleader
        :map lisp-mode-map
        :n "f" nil :n "l" nil :n "n" nil :n "r" nil :n "o" nil :n "m" nil
        :desc "Macrostep"                     :n "<RET>" 'macrostep-expand
        (:prefix ("g" . "goto")
         :desc "Go to package"                :n "p" 'sly-mrepl-sync
         :desc "Go to REPL"                   :n "r" 'my-switch-to-sly-mrepl-window)
        (:prefix ("e" . "eval")
         :n "b" nil :n "f" nil :n "F" nil
         :desc "Eval buffer"                  :n "b" 'sly-eval-buffer
         :desc "Eval & overlay definition"    :n "d" 'sly-overlay-eval-defun
         :desc "Eval definition"              :n "D" 'sly-eval-defun
         :desc "Eval region"                  :n "r" 'sly-eval-region
         :desc "Eval last expression"         :n "e" 'sly-eval-last-expression
         :desc "Eval & print last expression" :n "E" 'sly-eval-print-last-expression
         :desc "Undefine function"            :n "u" 'sly-undefine-function)
        (:prefix ("c" . "compile")
         :n "C" nil :n "n" nil :n "l" nil
         :desc "Compile & load file"          :n "f" 'sly-compile-and-load-file
         :desc "Compile definition"           :n "d" 'sly-compile-defun
         :desc "Compile region"               :n "r" 'sly-compile-region)
        (:prefix ("x" . "extras")
         :desc "Remove notes"                 :n "R" 'sly-remove-notes
         :desc "Load file"                    :n "l" 'sly-load-file
         :desc "Find file in Quicklisp"       :n "f" '+lisp/find-file-in-quicklisp)))

(def bind-multiple-cursor-keys ()
  "Bind multiple cursor keys."
  (map! :leader
        (:prefix-map
         ("m" . "multiple cursors")
         "m" 'evil-mc-make-all-cursors
         "q" 'evil-mc-undo-all-cursors

         "z" '+multiple-cursors/evil-mc-toggle-cursor-here
         "t" '+multiple-cursors/evil-mc-toggle-cursors
         "u" '+multiple-cursors/evil-mc-undo-cursor

         "k" 'evil-mc-make-and-goto-prev-cursor
         "j" 'evil-mc-make-and-goto-next-cursor

         "K" 'evil-mc-make-and-goto-first-cursor
         "J" 'evil-mc-make-and-goto-last-cursor

         "p" 'evil-mc-make-cursor-move-prev-line
         "n" 'evil-mc-make-cursor-move-next-line

         "C" 'evil-mc-skip-and-goto-prev-cursor
         "c" 'evil-mc-skip-and-goto-next-cursor

         "S" 'evil-mc-skip-and-goto-prev-match
         "s" 'evil-mc-skip-and-goto-next-match

         "D" 'evil-mc-make-and-goto-prev-match
         "d" 'evil-mc-make-and-goto-next-match)))

(def bind-dirvish-keys ()
  "Bind Dirvish keys."
  (map! :localleader
        :map dirvish-mode-map
        :desc "Go to Desktop"      :n "t" 'my-dired-goto-desktop
        :desc "Go to Downloads"    :n "d" 'my-dired-goto-downloads
        :desc "Go to Documents"    :n "u" 'my-dired-goto-documents
        :desc "Go to Pictures"     :n "p" 'my-dired-goto-pictures
        :desc "Go to Movies"       :n "m" 'my-dired-goto-movies
        :desc "Go to Music"        :n "c" 'my-dired-goto-music
        :desc "Go to Developer"    :n "r" 'my-dired-goto-developer
        :desc "Go to Applications" :n "a" 'my-dired-goto-applications))

(def bind-haskell-keys ()
  "Bind Haskell keys."
  (map! :localleader
        :map haskell-mode-map
        :n "b" nil :n "c" nil
        :desc "Load and run"    "r" 'haskell-load-and-run
        :desc "Load file"       "l" 'haskell-process-load-file
        :desc "Show type"       "t" 'haskell-process-do-type
        :desc "Show info"       "i" 'haskell-process-do-info
        :desc "Toggle hide"     "h" 'haskell-hide-toggle
        :desc "Toggle hide all" "H" 'haskell-hide-toggle-all))


;;; ralju

(bind-workspace-switch-keys)
(bind-workspace-keys)
(bind-mac-keys)
(bind-toplevel-keys)
(bind-main-keys)
(bind-emacs-keys)
(bind-window-keys)
(bind-x-keys)
(bind-multiple-cursor-keys)
(bind-dirvish-keys)
