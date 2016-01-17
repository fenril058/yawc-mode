;;; yawc-mode.el --- yet another wc-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2016  ril

;; Author: ril
;; Created: 2016-01-16 12:00:00
;; Last Modified: 2016-01-17 10:10:15
;; Version: 1.1
;; Keywords: convenience, mode line
;; URL: https://github.com/fenril058/yawc-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A simple minor-mode to display the length of the buffer in the mode
;; line. This is deeply based on the wc-mode which made by Toby
;; Cubitt.  URL: http://www.dr-qubit.org/emacs.php

;;; Code:
(defgroup yawc nil
  "Display the total number of characters, words, and lines in
the mode-line."
  :group 'mode-line)

(defcustom yawc-mode-jp nil
  "non-nilならば、`yawc-mode-line-format-jp' によってモード
ラインに表示します。デフォルトの形式は%d文字%d行です。文字数に改
行は含みません。"
  :group 'yawc)

(make-variable-buffer-local 'yawc-mode-jp)

(defcustom yawc-use-disable-list nil
  "If nil, global-yawc-mode enables yawc-mode in the modes which
are the member of `yawc-enable-modes'. If non-nil, global-yawc-mode
enables yawc-mode in all modes except in `yawc-disable-modes'. "
  :group 'yawc)

(defvar yawc-enable-modes '(org-mode)
  "Major modes which `yawc-mode' can run on.")

(defvar yawc-disable-modes '(lisp-interaction-mode)
  "Major modes which `yawc-mode' can not run on.")

(defvar yawc-mode-line-format
      '(if (use-region-p)
           (format " %d,%d,%d"
                   (abs (- (point) (mark)))
                   (count-words-region (point) (mark))
                   (abs (+ (- (line-number-at-pos (point))
                              (line-number-at-pos (mark))) 1)))
         (format " %d,%d,%d"
                 (point-max)
                 (count-words-region (point-min) (point-max))
                 (line-number-at-pos (point-max)))))

(defvar yawc-mode-line-format-jp
  '(if (use-region-p)
       (format " %d文字%d行"
               (abs (- (point) (mark)))
               (abs (+ (- (line-number-at-pos (point))
                          (line-number-at-pos (mark))) 1)))
     (let* ((pmax (point-max))
            (lnap (line-number-at-pos pmax)))
       (format " %d文字%d行"
               (- pmax lnap)
               lnap
               )))
  )

(defvar yawc-mode-display-style
      `((yawc-mode
         (6 (:eval
             (if yawc-mode-jp
                 ,yawc-mode-line-format-jp
               ,yawc-mode-line-format
                 )))
     nil)
    ))

;;;###autoload
(define-minor-mode yawc-mode
  "Minor-mode which display the total number of characters,
words, and lines in the mode-line. If you use region, those in
the region are displayed."
  :group      'yawc
  :init-value nil
  :global     nil
  :lighter    ""
  (if yawc-mode
      (add-to-list 'mode-line-position yawc-mode-display-style t)
    (setq mode-line-position
          (assq-delete-all 'yawc-mode mode-line-position))
    ))

(defun yawc-mode-maybe ()
  "What buffer `yawc-mode' prefers."
  (when (and (not (minibufferp (current-buffer)))
             (if yawc-use-disable-list
                 (not (memq major-mode yawc-disable-modes))
               (memq major-mode yawc-enable-modes))
             (yawc-mode 1)
             )))

;;;###autoload
(define-global-minor-mode global-yawc-mode
  yawc-mode yawc-mode-maybe
  :group 'yawc)

(provide 'yawc-mode)
;;; yawc-mode.el ends here
