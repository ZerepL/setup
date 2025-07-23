;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
 (setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)

(require 'dap-java)

(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (setq use-package-always-ensure t)
   (require 'use-package)))

(use-package projectile)
(use-package flycheck)
(use-package yasnippet :config (yas-global-mode))
(use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config (setq lsp-completion-enable-additional-text-edit nil))
(use-package hydra)
(use-package company)
(use-package lsp-ui)
(use-package which-key :config (which-key-mode))
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java)
(use-package helm-lsp)
;; (use-package helm
;;  :config (helm-mode))
(use-package lsp-treemacs)

;; GIT || MAGIT ==========================================
(defun my/magit-push-with-script ()
  "Run the custom push script to push changes."
  (interactive)
  (let ((default-directory (magit-toplevel)))  ;; Run in the Git root
    (shell-command "$UTILS/scripts/push.sh")))  ;; Path to your script
;;
(with-eval-after-load 'magit
  (transient-append-suffix 'magit-push "p"
    '("g" "Push to that horrible Gerrit" my/magit-push-with-script)))

;; AUTO SAVE || ETC ======================================
(setq auto-save-default t)
(setq auto-save-timeout 20)  ;; Save every 20 seconds
(setq auto-save-interval 200)  ;; Save after typing 200 characters

;; MAPS ====================================================


(map! :leader
      ;; Lsp Java Keys
      "c mjs" #'lsp-ui-sideline-apply-code-actions
      "c mji" #'lsp-goto-implementation
      "c mjm" #'lsp-java-extract-method
      ;; Dap Keys
      "c mjdm" #'dap-java-run-test-method
      "c mjdc" #'dap-java-run-test-class
      "c mjddm" #'dap-java-debug-test-method
      "c mjddc" #'dap-java-debug-test-class
      "c mjdt" #'dap-breakpoint-toggle
      ;; Find references
      "c u" #'lsp-find-references
     )

(map! :map local-leader-map
      :prefix "t"
      :desc "Run all component tests" "A"
      (lambda ()
        (interactive)
        (async-shell-command "mvnrct"))

      :desc "Run only the failed component tests" "f"
      (lambda ()
        (interactive)
        (async-shell-command "mvnfct")))


(map! :nv "M-," #'better-jumper-jump-backward
      :nv "M-." #'better-jumper-jump-forward)

;; Projectile ==============================================

;; Prevent projectile from addining new projects
(after! projectile
  (setq projectile-track-known-projects-automatically nil
        projectile-indexing-method 'alien
        projectile-cache-file (expand-file-name ".projectile.cache" user-emacs-directory)))

;; JAVA ====================================================

;; Lombok
(setq lsp-java-vmargs
      (list
         "-noverify"
         "-Xmx4G"
         "-XX:+UseG1GC"
         "-XX:+UseStringDeduplication"
         "-javaagent:$HOME/sandbox/lombok.jar"))

;; Format Java
(setq lsp-java-format-settings-url "file://$HOME/.utils/files/java.xml")
(setq lsp-java-format-settings-profile "GoogleStyle")
(setq lsp-java-format-on-type-enabled t)
(setq lsp-java-save-actions-organize-imports t)

;; Configure Java Debug Adapter
(setq dap-java-debug-port 8787)

;; Attaching to a remote debugging session
(dap-register-debug-template "Java Debug (Attach)"
  (list :type "java"
        :request "attach"
        :host "127.0.0.1"
        :port 8787
        :name "Java Debug (Attach)"))

(defun my/dap-yank-value-at-point (node)
  (interactive (list (treemacs-node-at-point)))
  (kill-new (message (plist-get (button-get node :item) :value))))


;; Ensure all required DAP features are enabled
(setq dap-auto-configure-features '(sessions locals breakpoints expressions))

;; Allow more time for DAP responses (if applicable)
(setq dap-inhibit-io nil)  ;; Ensure debugging messages are shown

;; Test local DBs ==========================================
(setq sql-connection-alist
      '((localPostgres
         (sql-product 'postgres)
         (sql-user "postgres")
         (sql-database "rprepo")
         (sql-server "localhost")
         (sql-port 5432))))

;; Music ===================================================
(use-package! smudge
  :bind-keymap ("C-c ." . smudge-command-map)
  :custom
  (smudge-oauth2-client-secret "")
  (smudge-oauth2-client-id "")
  ;; optional: enable transient map for frequent commands
  (smudge-player-use-transient-map t)
  :config
  ;; optional: display current song in mode line
  (global-smudge-remote-mode))

(setq smudge-transport-port 8888)

(after! smudge
  (define-key smudge-mode-map (kbd "C-c .") 'smudge-command-map)
  (map! :nv "C-c . t l" #'smudge-track-load-more
        :nv "C-c . p l" #'smudge-playlist-load-more))

;; Markdown ================================================
 (defvar nb/current-line '(0 . 0)
   "(start . end) of current line in current buffer")
 (make-variable-buffer-local 'nb/current-line)

 (defun nb/unhide-current-line (limit)
   "Font-lock function"
   (let ((start (max (point) (car nb/current-line)))
         (end (min limit (cdr nb/current-line))))
     (when (< start end)
       (remove-text-properties start end
                       '(invisible t display "" composition ""))
       (goto-char limit)
       t)))

 (defun nb/refontify-on-linemove ()
   "Post-command-hook"
   (let* ((start (line-beginning-position))
          (end (line-beginning-position 2))
          (needs-update (not (equal start (car nb/current-line)))))
     (setq nb/current-line (cons start end))
     (when needs-update
       (font-lock-fontify-block 3))))

 (defun nb/markdown-unhighlight ()
   "Enable markdown concealling"
   (interactive)
   (markdown-toggle-markup-hiding 'toggle)
   (font-lock-add-keywords nil '((nb/unhide-current-line)) t)
   (add-hook 'post-command-hook #'nb/refontify-on-linemove nil t))

(add-hook 'markdown-mode-hook #'nb/markdown-unhighlight)
