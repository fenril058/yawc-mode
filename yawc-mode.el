;;; yawc-mode.el --- yet another wc-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2016  ril

;; Author: ril
;; Created: 16 Jun 2016
;; Last Modified: 16 Sep 2016
;; Version: 1.0
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

(defcustom yawc-mode-jp-format nil
  "non-nilならば、`yawc-mode-line-format-jp' によってモード
ラインに表示します。デフォルトの形式は%d文字%d行です。文字数に改
行は含みません。"
  :group 'yawc)

(make-variable-buffer-local 'yawc-mode-jp-format)

(defvar yawc-mode-line-format
  '((yawc-mode
     (6 (:eval (if (use-region-p)
                   (format " %d,%d,%d"
                           (abs (- (point) (mark)))
                           (count-words-region (point) (mark))
                           (abs (+ (- (line-number-at-pos (point))
                                      (line-number-at-pos (mark))) 1)))
                 (format " %d,%d,%d"
                         (point-max)
                         (count-words-region (point-min) (point-max))
                         (line-number-at-pos (point-max))))))
     nil)))

(defvar yawc-mode-line-format-jp
  '((yawc-mode
     (6 (:eval (if (use-region-p)
                   (format " %d文字%d行"
                           (abs (- (point) (mark)))
                           (abs (+ (- (line-number-at-pos (point))
                                      (line-number-at-pos (mark))) 1)))
                 (let* ((pmax (point-max))
                        (lnap (line-number-at-pos pmax)))
                   (format " %d文字%d行"
                           (- pmax lnap)
                           lnap
                           )))))
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
      (setq mode-line-position
            (append mode-line-position
                    (if yawc-mode-jp-format
                        yawc-mode-line-format-jp
                      yawc-mode-line-format)))
    (setq mode-line-position
          (assq-delete-all 'yawc-mode mode-line-position))
    ))

(provide 'yawc-mode)
;;; yawc-mode.el ends here
