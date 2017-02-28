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
         ,@body)
    `(url-retrieve ,url
                   (lambda (status)
                     (if (plist-get status :error)
                         (error "Error when retrieving %s" ,url)
                       ,@body)))))

(defun wotd--get-merriam-webster ()
  (wotd--retrieve
   "https://www.merriam-webster.com/wotd/feed/rss2" nil
   (goto-char url-http-end-of-headers)
   (let ((res-xml (wotd-xml-parse-region (point) (point-max)))
         (res-buf (get-buffer-create "*Merriam-Webster*")))
     (with-current-buffer res-buf
       (erase-buffer)
       (insert (nth 2 (car (xml-get-children
                            (car (xml-get-children
                                  (car (xml-get-children (car res-xml) 'channel))
                                  'item))
                            'description))))
       (goto-char (point-min))
       (replace-string "&#149;" "&#8226;")
       (shr-render-buffer (current-buffer))))))

(provide 'word-of-the-day)
;;; word-of-the-day.el ends here
