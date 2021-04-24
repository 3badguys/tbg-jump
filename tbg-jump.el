;;; tbg-jump.el --- ▮ -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020-2027, by 3badguys

;; Author: 3badguys
;; Version: ▮
;; Created: ▮
;; Package-Requires: ▮
;; Keywords: ▮
;; License: GPL v3

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:


(defcustom tbg-jump-tags-file-name "TAGS"
  "Tags file name."
  :group 'tbg-jump
  :type 'string)

(defcustom tbg-jump-project-root-marks '(".git" ".svn")
  "The marks used to locate project root directory."
  :group 'tbg-jump
  :type '(repeat 'string))

(defcustom tbg-jump-project-root nil
  "Project root directory."
  :group 'tbg-jump
  :type 'string)

(defun tbg-jump-locate-project-root ()
  "Find the root of project."
  (let ((project-root (cl-some (apply-partially 'locate-dominating-file
                                                default-directory)
                               tbg-jump-project-root-marks)))
    (or tbg-jump-project-root (and project-root (file-name-as-directory project-root)))))

(defun tbg-jump-locate-tags-file ()
  "Find tags file, search parent directories."
  (let ((tags-dir (locate-dominating-file default-directory tbg-jump-tags-file-name)))
    (and tags-dir (file-truename (concat (file-name-as-directory tags-dir)
                                         tbg-jump-tags-file-name)))))

(defun tbg-jump-create-tags-file-async (src-root)
  "Create tags file async."
  (start-process-shell-command
   ""
   nil
   (format "ctags -f %s -e -R %s"
           (expand-file-name tbg-jump-tags-file-name (directory-file-name src-root))
           (directory-file-name src-root)))
  (message "created tags async through start-process-shell-command."))

(defun tbg-jump-tags-file-pretreat ()
  "Do some pretreat operations."
  (let ((tags-file (tbg-jump-locate-tags-file))
        src-root)
    (when (not tags-file)
      (setq src-root (read-directory-name "SrcCode root: " (tbg-jump-locate-project-root)))
      (tbg-jump-create-tags-file-async src-root))
    ;;
    ))

;;;###autoload
(defun tbg-jump-find-tag-at-point ()
  "Find tag using tagname at point. Use `pop-tag-mark' to jump back."
  (interactive)
  (let ()
    (tbg-jump-tags-file-pretreat)
    ;;
    ))

(provide 'tbg-jump)

;;; tbg-jump.el ends here
