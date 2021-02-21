# counsel-deft.el
Browse files in Emacs using ivy.

## Dependencies
This package depends on `ivy`, `f` and `eww`(Emacs Web Wowser, a built-in Emacs package).

## Install
Clone this repo and add the directory to `load-path`. If you use `use-package`, bellow is an example.

```elisp
;;;; counsel-deft
(use-package counsel-deft
  :load-path "/path/to/counsel-deft.el/"
  :when (file-exists-p "/path/to/counsel-deft.el/")
  :config (progn
            (setq counsel-deft-extensions '("org" "html" "pdf"))
            (setq counsel-deft-directory "/path/to/files")
            )
  )

```

## Usage
There are two commands, `counsel-deft` and `counsel-deft-interactive`, the first one allows you to browse files in `counsel-deft-directory` using ivy, and the second one allows you to select the directory to browse.
