#!/bin/bash

#;;; Bash script

#;; This file can be executed directly as a "standalone" Emacs Matrix
#;; client that does not load any user configuration files.  The Bash
#;; process substitution trick above was inspired by:
#;; https://superuser.com/a/821624

# * Defaults

recipe_part=":fetcher github :repo \"jgkamat/matrix-client-el\""
custom_file_dir="~/.config/"

# * Functions

function die {
    echo "$@" >&2
    exit 1
}

function usage {
    cat <<EOF
This script launches matrix-client.el with a mostly clean Emacs configuration.

It's only "mostly" clean because it uses the local ELPA package
installation directory.  This allows you to, e.g. load themes you
already have installed, and it means that, after running this script,
matrix-client will be usable in your main Emacs config.

Options:

  --debug        Enable debug-on-error in Emacs
  --help         You're lookin' at it
  --local PATH   Use git repo at PATH instead of official one on GitHub (helpful for development)
  --upgrade      Upgrade matrix-client.el and required dependency upgrades before connecting
EOF
}

# * Args

while [[ $1 ]]
do
[[ $1 == --debug ]] && debug="(setq debug-on-error t) (setq matrix-log t)"
[[ $1 == --help ]] && usage && exit
[[ $1 == --local ]] && {
    shift
    [[ -d $1/.git ]] && [[ -r $1/.git ]] || die "Not a readable directory (should be a Git repo): $1"
    recipe_part=":fetcher git :url ,(expand-file-name \"$1\")"
}
[[ $1 == --upgrade ]] && upgrade="(setq upgrade-matrix-client t) (setq quelpa-update-melpa-p t)"
shift
done

# * Main

# Find end of bash script
bash_end_line=$((2 + $(grep -n -m 1 -x "exit" "$0" \
                            | grep -o -E '[[:digit:]]+')))

# NOTE: We set user-init-file to a temp file so Emacs will not balk at
# saving our standalone custom-file.  If we didn't do this, we
# wouldn't be able to save per-room settings.

# MAYBE: Use a persistent file instead of a temp file.

# Run Emacs
emacs -q --insert <(tail -n +$bash_end_line "$0") --eval="(progn
(defvar upgrade-matrix-client nil)
(defvar quelpa-update-melpa-p nil)

(setq user-init-file (expand-file-name \"matrix-client-standalone.el\" \"$custom_file_dir\"))
(when (file-readable-p user-init-file)
  (load user-init-file))

(setq recipe \`(matrix-client $recipe_part
	                  :files (:defaults \"logo.png\" \"matrix-client-standalone.el.sh\")))
$upgrade $debug
(eval-buffer))"

exit

;;; # * matrix-client-standalone.el

;; Misc settings
(setq load-prefer-newer t)
(setq tool-bar-mode nil)

;; Improve default completion
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq completion-cycle-threshold 1)
(setq completions-format 'vertical)

;; Switch buffer command
(global-set-key (kbd "C-<tab>") #'switch-to-buffer)

;;;# package.el

(require 'package)
(setq package-menu-async nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;;;# Quelpa

;; Install Quelpa if necessary.
(unless (require 'quelpa-use-package nil t)
  (defun install-quelpa ()
    (package-install 'quelpa-use-package)
    (require 'quelpa-use-package nil t))
  (add-hook 'package--post-download-archives-hook #'install-quelpa)
  (package-list-packages))

;; Install/upgrade Matrix Client. Quelpa makes it very difficult to force an upgrade.  It says if
;; you add ":upgrade t", it will, but it doesn't work.  So we have to bind `current-prefix-arg'.
(let ((current-prefix-arg upgrade-matrix-client))
  (quelpa recipe))

;;;# matrix-client

(use-package matrix-client
 :custom
 (matrix-client-show-images t)
 (matrix-client-show-room-avatars t)
 (matrix-client-mark-modified-rooms t))

;;;;# Connect

(add-hook 'matrix-client-setup-room-buffer-hook
          (lambda (&rest _ignore)
            (setq mode-line-format nil)))

(with-selected-frame
    (call-interactively #'matrix-client-frame)
  (delete-other-frames))

;; Local Variables:
;; eval: (aggressive-indent-mode -1)
;; End:
