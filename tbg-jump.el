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

(defun tbg-jump--locate-project-root ()
  "Find the root of project."
  (let (($project-root (cl-some (apply-partially 'locate-dominating-file
                                                default-directory)
                               tbg-jump-project-root-marks)))
    (or tbg-jump-project-root (and $project-root (file-name-as-directory $project-root)))))

(defun tbg-jump--locate-tags-file ()
  "Find tags file, search parent directories."
  (let (($tags-dir (locate-dominating-file default-directory tbg-jump-tags-file-name)))
    (and $tags-dir (file-truename (concat (file-name-as-directory $tags-dir)
                                         tbg-jump-tags-file-name)))))

(defun tbg-jump--create-tags-file-async (@src-root)
  "Create tags in @SRC-ROOT file async."
  (start-process-shell-command
   ""
   nil
   (format "ctags -f %s -e -R %s"
           (expand-file-name tbg-jump-tags-file-name (directory-file-name @src-root))
           (directory-file-name @src-root)))
  (message "created tags async through start-process-shell-command."))

(defun tbg-jump--tags-file-pretreat ()
  "Do some pretreat operations."
  (let (($tags-file (tbg-jump--locate-tags-file))
        $src-root)
    (when (not $tags-file)
      (setq $src-root (read-directory-name "SrcCode root: " (tbg-jump--locate-project-root)))
      (tbg-jump--create-tags-file-async $src-root))))

(defun tbg-jump--tag-at-point ()
  "Get the tag at point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (let (($bounds (bounds-of-thing-at-point 'symbol)))
      (and $bounds (buffer-substring-no-properties (car $bounds) (cdr $bounds))))))

(defun tbg-jump--read-file (@file)
  "Read @FILE content."
  (with-temp-buffer
    (insert-file-contents @file)
    (buffer-string)))

(defun tbg-jump--tag-search-regex (@tag)
  "Get the regex to search @TAG in tags file. And @TAG could be nil."
  (concat "\\([^\n]+\\)\\("
          (or @tag "[^\n]+")
          "\\)\\([0-9]+\\),\\([0-9]+\\)"))

(defun tbg-jump--search-tag-candidates (@file-content @tag)
  "Search from @FILE-CONTENT and return the candidates of @TAG."
  (let (($tag-re (tbg-jump--tag-search-regex @tag))
        ($cands '()))
    (with-temp-buffer
      (insert @file-content)
      (goto-char (point-min))
      (while (re-search-forward @tag nil "NOERROR")
        (beginning-of-line)
        (when (re-search-forward $tag-re (line-end-position) "NOERROR")
          (add-to-list '$cands
                       (list :text (match-string-no-properties 1)
                             :tag (match-string-no-properties 2)
                             :column (string-to-number (match-string-no-properties 3))
                             :position (string-to-number (match-string-no-properties 4))
                             :file (etags-file-of-tag t))
                       "APPEND"))))
    $cands))

(defun tbg-jump--current-date-time-string ()
  "Return current date-time string in this format 「2021-04-28T18:06:51+08:00」"
  (concat
   (format-time-string "%Y-%m-%dT%T")
   (funcall (lambda (x) (format "%s:%s" (substring x 0 3) (substring x 3 5))) (format-time-string "%z"))))

(defun tbg-jump--output-candidates (@tag @cands)
  "Output candidates of @TAG stored in @CANDS."
  (let (($buffer-name "*tbg-jump output*")
        $output-buffer)
    (when (get-buffer $buffer-name) (kill-buffer $buffer-name))
    (setq $output-buffer (generate-new-buffer $buffer-name))
    (switch-to-buffer-other-window $output-buffer)
    (princ
     (concat
      "-*- coding: utf-8; mode: tbg-jump-output -*-" "\n"
      "Datetime: " (tbg-jump--current-date-time-string) "\n"
      (format "Search tag: %s\n" @tag)
      "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n"
      ) $output-buffer)
    (mapc (lambda ($one-cand)
            (progn
              (princ $one-cand $output-buffer)
              (princ "\n\n" $output-buffer)))
          @cands)))

(defun tbg-jump--jump-one-candidate-location (@one-cand)
  "Jump to the location of one candidate @ONE-CAND."
  (let (($tag (plist-get @one-cand :tag))
        ($file (plist-get @one-cand :file))
        ($column (plist-get @one-cand :column)))
    (find-file $file)
    (goto-char (point-min))
    (forward-line (1- $column))
    (search-forward $tag (line-end-position) "NOERROR")))

(defun tbg-jump--search-tags-file (@tag)
  "Search @TAG in tags file."
  (let (($tags-file (tbg-jump--locate-tags-file))
        $cands)
    (or @tag (setq @tag (read-string "Enter tag name: ")))
    (when (and $tags-file (file-exists-p $tags-file))
      (setq $cands (tbg-jump--search-tag-candidates (tbg-jump--read-file $tags-file) @tag)))
    (cond
     ((not $cands) (message "No candidate found for tag(%s)." @tag))
     ((= 1 (length $cands)) (tbg-jump--jump-one-candidate-location (car $cands)))
     (t (tbg-jump--output-candidates @tag $cands)))))

;;;###autoload
(defun tbg-jump-find-tag-at-point ()
  "Find tag using tagname at point. Use `pop-tag-mark' to jump back."
  (interactive)
  (tbg-jump--tags-file-pretreat)
  (let (($tag (tbg-jump--tag-at-point)))
    (cond
     ($tag (tbg-jump--search-tags-file $tag))
     (t (message "No tag found at point.")))))

(provide 'tbg-jump)

;;; tbg-jump.el ends here
