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

(defcustom tbg-jump-snippet-before-context-lines 2
  "The count of lines before snippet."
  :group 'tbg-jump
  :type 'integer)

(defcustom tbg-jump-snippet-after-context-lines 3
  "The count of lines after snippet."
  :group 'tbg-jump
  :type 'integer)

(defcustom tbg-jump-header-separator
  "hh━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
  "A string as visual separator."
  :group 'tbg-jump)

(defcustom tbg-jump-snippet-separator
  "ss━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
  "A string as visual separator."
  :group 'tbg-jump)

(defcustom tbg-jump-tag-prefix
  "『"
  "A left-bracket string that marks matched tagname and navigate previous/next."
  :group 'tbg-jump)

(defcustom tbg-jump-tag-postfix
  "』"
  "A right-bracket string that marks matched tagname and navigate previous/next."
  :group 'tbg-jump)

(defcustom tbg-jump-filepath-prefix
  "〘"
  "A left-bracket string used to mark file path and navigate previous/next."
  :group 'tbg-jump)

(defcustom tbg-jump-filepath-postfix
  "〙"
  "A right-bracket string used to mark file path and navigate previous/next."
  :group 'tbg-jump)

(defface tbg-jump-tag-highlight
  '((t :foreground "black"
       :background "yellow"
       ))
  "Face for matched tag."
  :group 'tbg-jump)

(defface tbg-jump-file-path-highlight
  '((t :foreground "black"
       :background "pink"
       ))
  "Face of file path where a tag match is found."
  :group 'tbg-jump)


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
                             :line-number (string-to-number (match-string-no-properties 3))
                             :position (string-to-number (match-string-no-properties 4))
                             :file (etags-file-of-tag t))
                       "APPEND"))))
    $cands))

(defun tbg-jump--current-date-time-string ()
  "Return current date-time string in this format 「2021-04-28T18:06:51+08:00」"
  (concat
   (format-time-string "%Y-%m-%dT%T")
   (funcall (lambda (x) (format "%s:%s" (substring x 0 3) (substring x 3 5))) (format-time-string "%z"))))

(defun tbg-jump--insert-header (@tag @buffer-name)
  "Insert header into buffer named @BUFFER-NAME when you are searching @TAG."
  (princ
     (concat
      "-*- coding: utf-8; mode: tbg-jump -*-" "\n"
      "Datetime: " (tbg-jump--current-date-time-string) "\n"
      (format "Search tag: ❬%s❭\n" @tag)
      tbg-jump-header-separator) @buffer-name))

(defun tbg-jump--insert-one-source-snippet (@one-cand @buffer-name)
  "Insert source snippet via $ONE-CAND into the buffer named @BUFFER-NAME."
  (let (($tag (plist-get @one-cand :tag))
        ($file (plist-get @one-cand :file))
        ($line-number (plist-get @one-cand :line-number))
        $snippet-begin $match-line-begin $snippet-end
        $snippet-before-block $snippet-middle-block $snippet-after-block
        $tag-begin $tag-end)
    (with-temp-buffer
      (insert-file-contents $file)
      (cond
       ((> (1- $line-number) tbg-jump-snippet-before-context-lines)
        (progn
          (forward-line (- (1- $line-number) tbg-jump-snippet-before-context-lines))
          (setq $snippet-begin (point))
          (forward-line tbg-jump-snippet-before-context-lines)
          (setq $match-line-begin (point))))
       (t
        (progn
          (setq $snippet-begin (point))
          (forward-line (1- $line-number))
          (setq $match-line-begin (point)))))
      (forward-line (1+ tbg-jump-snippet-after-context-lines))
      (setq $snippet-end (point))

      (goto-char $match-line-begin)
      (when (search-forward $tag (line-end-position) "NOERROR")
        (setq $tag-begin (match-beginning 0))
        (setq $tag-end (match-end 0)))

      (setq $snippet-before-block (buffer-substring-no-properties $snippet-begin $tag-begin))
      (setq $snippet-after-block (buffer-substring-no-properties $tag-end $snippet-end))
      (setq $snippet-middle-block (concat
                                   tbg-jump-tag-prefix
                                   (buffer-substring $tag-begin $tag-end)
                                   tbg-jump-tag-postfix)))
    (with-current-buffer @buffer-name
      (insert
       (format "%s%s%s%s"
               $snippet-before-block
               $snippet-middle-block
               $snippet-after-block
               tbg-jump-snippet-separator
               )))))

(defun tbg-jump--output-candidates (@tag @cands)
  "Output candidates of @TAG stored in @CANDS."
  (let (($buffer-name "*tbg-jump output*")
        $output-buffer)
    (when (get-buffer $buffer-name) (kill-buffer $buffer-name))
    (setq $output-buffer (generate-new-buffer $buffer-name))
    (switch-to-buffer-other-window $output-buffer)
    (tbg-jump--insert-header @tag $output-buffer)
    (mapc (lambda ($one-cand)
            (tbg-jump--insert-one-source-snippet $one-cand $buffer-name))
          @cands)
    (tbg-jump-mode)))

(defun tbg-jump--switch-to-output (@buffer)
  "Switch to @BUFFER and highlight stuff."
  (switch-to-buffer @buffer)
  (tbg-jump-mode))

(defun tbg-jump--jump-one-candidate-location (@one-cand)
  "Jump to the location of one candidate @ONE-CAND."
  (let (($tag (plist-get @one-cand :tag))
        ($file (plist-get @one-cand :file))
        ($line-number (plist-get @one-cand :line-number)))
    (find-file $file)
    (goto-char (point-min))
    (forward-line (1- $line-number))
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


(setq tbg-jump-fock-lock-keyworks
      (let (
            (xTag (format "%s\\([^%s]+\\)%s" tbg-jump-tag-prefix tbg-jump-tag-postfix tbg-jump-tag-postfix))
            (xfPath (format "%s\\([^%s]+\\)%s" tbg-jump-filepath-prefix tbg-jump-filepath-postfix tbg-jump-filepath-postfix)))
        `(
          (,xTag . (1 'tbg-jump-tag-highlight))
          (,xfPath . (1 'tbg-jump-file-path-highlight)))))

(defvar tbg-jump-mode-map nil "Keybinding for `tbg-jump.el output'")
(progn
  (setq tbg-jump-mode-map (make-sparse-keymap))
  ;;
  )

(define-derived-mode tbg-jump-mode fundamental-mode "tbg-jump"
  "Major mode for reading output for tbg-jump commands."

  (setq font-lock-defaults '((tbg-jump-fock-lock-keyworks)))

  ;; actually no need
  (use-local-map tbg-jump-mode-map)

  :group 'tbg-jump)

(provide 'tbg-jump)

;;; tbg-jump.el ends here
