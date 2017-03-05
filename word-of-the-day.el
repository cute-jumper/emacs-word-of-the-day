;;; word-of-the-day.el --- Fetch word-of-the-day from multiple online sources  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords: extensions

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

;;

;;; Code:

(require 'url)
(require 'xml)
(require 'shr)

(defvar wotd--debug nil)
(defvar wotd--default-buf-name "*Word-of-The-Day*")

;; Steal from `elfeed'
(defun wotd-xml-parse-region (&optional beg end buffer parse-dtd _parse-ns)
  "Decode (if needed) and parse XML file. Uses coding system from
XML encoding declaration."
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))
  (setf (point) beg)
  (when (re-search-forward
         "<\\?xml.*?encoding=[\"']\\([^\"']+\\)[\"'].*?\\?>" nil t)
    (let ((coding-system (intern-soft (downcase (match-string 1)))))
      (when (ignore-errors (check-coding-system coding-system))
        (let ((mark-beg (make-marker))
              (mark-end (make-marker)))
          ;; Region changes with encoding, so use markers to track it.
          (set-marker mark-beg beg)
          (set-marker mark-end end)
          (set-buffer-multibyte t)
          (recode-region mark-beg mark-end coding-system 'raw-text)
          (setf beg (marker-position mark-beg)
                end (marker-position mark-end))))))
  (let ((xml-default-ns ()))
    (xml-parse-region beg end buffer parse-dtd 'symbol-qnames)))

(defmacro wotd--retrieve (url sync &rest body)
  (if sync
      `(with-current-buffer (url-retrieve-synchronously ,url)
         (goto-char url-http-end-of-headers)
         ,@body)
    `(url-retrieve ,url
                   (lambda (status)
                     (if (plist-get status :error)
                         (error "Error when retrieving %s" ,url)
                       (goto-char url-http-end-of-headers)
                       ,@body)))))

(defmacro wotd--def-xml-parser (buf-name url content &rest cleanups)
  `(wotd--retrieve
    ,url nil
    (let ((res-xml (wotd-xml-parse-region (point) (point-max))))
      (with-current-buffer (get-buffer-create ,buf-name)
        (wotd--display-and-cleanup
            ,content
          ,@cleanups)
        (let ((dom (libxml-parse-html-region (point-min) (point-max))))
          (erase-buffer)
          (shr-insert-document dom)))
      (display-buffer ,buf-name t))))

(defmacro wotd--display-and-cleanup (content &rest cleanups)
  (declare (indent 1))
  `(progn
     (erase-buffer)
     (insert ,content)
     ,@(apply #'append
              (mapcar (lambda (fn)
                        `((goto-char (point-min))
                          ,fn))
                      cleanups))))

(defun wotd--get-merriam-webster ()
  (wotd--def-xml-parser
   "*Merriam-Webster*"
   "https://www.merriam-webster.com/wotd/feed/rss2"
   (nth 2 (car (xml-get-children
                (car (xml-get-children
                      (car (xml-get-children (car res-xml) 'channel))
                      'item))
                'description)))
   (replace-string "&#149;" "&#8226;")))

(defun wotd--get-wiktionary ()
  (wotd--def-xml-parser
   "*Wiktionary*"
   "https://en.wiktionary.org/w/api.php?action=featuredfeed&feed=wotd"
   (nth 2 (car (xml-get-children
                (car (nreverse
                      (xml-get-children
                       (car (xml-get-children (car res-xml) 'channel))
                       'item)))
                'description)))
   (replace-string "href=\"//" "href=\"https://")
   (replace-string "href=\"/wiki" "href=\"https://en.wiktionary.org/wiki")))


(provide 'word-of-the-day)
;;; word-of-the-day.el ends here
