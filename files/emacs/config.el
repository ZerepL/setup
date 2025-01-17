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
;;(use-package helm
;;  :config (helm-mode))
(use-package lsp-treemacs)

;; (setq lsp-java-format-enabled nil)



;; (add-hook 'java-mode-hook
;;       (lambda ()
;;          (setq c-basic-offset 2)  ; Set indentation level to 2 spaces
;;          (setq tab-width 2)       ; Set tab width to 2 spaces
;;          (setq indent-tabs-mode nil))) ; Use spaces instead of tabs


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


;; AI ASSISTANCE ===========================================

;; OPTIONAL configuration
(setq
 gptel-model 'mistral:latest
 gptel-backend (gptel-make-ollama "Ollama"
                 :host "localhost:11434"
                 :stream t
                 :models '(codellama:7b)))

;; MAPS ====================================================

(map! :leader
      ;; Counsel Keys
;;      "c mcr" #'counsel-rg
      ;; "c mcr" #'comment-or-uncomment-region
      ;; "c mcl" #'comment-line
      ;; "c mcg" #'counsel-grep
      ;; "c mca" #'counsel-ag
      ;; ;; Livelo DCU Keys
      ;; "c mdl" #'dcu-pull-global
      ;; "c mds" #'dcu-push-global
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

;; Projectile ==============================================

;; Normalize to always by ~/sandbox/
;; (after! projectile
;;   (setq projectile-project-search-path '("~/sandbox/")
;;         projectile-project-root-files-functions
;;         '(projectile-root-local
;;           projectile-root-top-down
;;           projectile-root-bottom-up
;;           projectile-root-top-down-recurring)))

;; Prevent projectile from addining new projects
(after! projectile
  (setq projectile-track-known-projects-automatically nil
        projectile-indexing-method 'alien
        projectile-cache-file (expand-file-name ".projectile.cache" user-emacs-directory)))

;; Folders and files
;; (setq projectile-project-root-files '(".git")) ;; Should use .git as project root
;; (setq projectile-project-root-files-top-down '(".git"))


;; JAVA ====================================================

;; Lombok
(setq lsp-java-vmargs
      (list
         "-noverify"
         "-Xmx4G"
         "-XX:+UseG1GC"
         "-XX:+UseStringDeduplication"
         "-javaagent:/home/zerepl/sandbox/lombok.jar"))

;; Format Java
(setq lsp-java-format-settings-url "file:///home/zerepl/.utils/files/java.xml")
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


;; Test local DBs ==========================================
(setq sql-connection-alist
      '((localPostgres
         (sql-product 'postgres)
         (user "postgres")
         (database "rprepo")
         (server "localhost")
         (port 5432))))
