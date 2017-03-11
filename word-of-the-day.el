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

(defvar wotd--enable-debug nil)
(defvar wotd--debug-buffer "*WOTD Debug*")
(defvar wotd--default-buf-name "*Word-of-The-Day*")

(defun wotd--debug (s)
  (if wotd--enable-debug
      (with-current-buffer (get-buffer-create wotd--debug-buffer)
        (erase-buffer)
        (insert (format "%s" s))))
  s)

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
  (declare (indent 3))
  `(wotd--retrieve
    ,url nil
    (let ((it (wotd-xml-parse-region (point) (point-max))))
      (with-current-buffer (get-buffer-create ,buf-name)
        (erase-buffer)
        (insert ,content)
        ,@(apply #'append
                 (mapcar (lambda (fn)
                           `((goto-char (point-min))
                             ,fn))
                         cleanups))
        (let ((dom (libxml-parse-html-region (point-min) (point-max))))
          (erase-buffer)
          (shr-insert-document dom)
          (goto-char (point-min))))
      (display-buffer ,buf-name t))))

(defmacro wotd--def-html-parser (buf-name url &rest body)
  (declare (indent 2))
  `(wotd--retrieve
    ,url nil
    (delete-region (point-min) (point))
    (let ((res (progn ,@body))
          dom)
      (wotd--debug res)
      (with-current-buffer (get-buffer-create ,buf-name)
        (set-buffer-multibyte t)
        (erase-buffer)
        (insert res)
        (setq dom (libxml-parse-html-region (point-min) (point-max)))
        (erase-buffer)
        (shr-insert-document dom)
        (goto-char (point-min)))
      (display-buffer ,buf-name t))))

(defun wotd--get-merriam-webster ()
  (wotd--def-xml-parser
      "*Merriam-Webster*"
      "https://www.merriam-webster.com/wotd/feed/rss2"
      (nth 2 (car (xml-get-children
                   (car (xml-get-children
                         (car (xml-get-children (car it) 'channel))
                         'item))
                   'description)))
    (replace-string "&#149;" "&#8226;")))

(defun wotd--get-wiktionary ()
  (wotd--def-xml-parser
      "*Wiktionary*"
      "https://en.wiktionary.org/w/api.php?action=featuredfeed&feed=wotd"
      (nth 2 (car (xml-get-children
                   (last (xml-get-children
                          (car (xml-get-children (car it) 'channel))
                          'item))
                   'description)))
    (replace-string "href=\"//" "href=\"https://")
    (replace-string "href=\"/wiki" "href=\"https://en.wiktionary.org/wiki")))

(defun wotd--get-macmillan ()
  (wotd--def-xml-parser
      "*Macmillan*"
      "http://www.macmillandictionary.com/wotd/wotdrss.xml"
      (let* ((entry (car (last (xml-get-children (car it) 'entry))))
             (title (nth 2 (car (xml-get-children entry 'title))))
             (href (cdr (nth 1 (cadr (car (xml-get-children entry 'link))))))
             (date (nth 2 (car (xml-get-children entry 'updated))))
             (summary (nth 2 (car (xml-get-children entry 'summary)))))
        (format "<h1><a href=\"%s\">%s</a></h1><p>%s</p><p>%s</p>"
                href
                title
                date
                summary))))

(defun wotd--get-wordsmith ()
  (wotd--def-xml-parser
      "*Wordsmith*"
      "https://www.wordsmith.org/awad/rss1.xml"
      (let* ((item (car (xml-get-children
                         (car (xml-get-children (car it) 'channel))
                         'item)))
             (title (nth 2 (car (xml-get-children item 'title))))
             (href (nth 2 (car (xml-get-children item 'link))))
             (description (nth 2 (car (xml-get-children item 'description)))))
        (format "<h1><a href=\"%s\">%s</a></h1><p>%s</p>" href title description))))

(defun wotd--get-free-dictionary ()
  (wotd--def-xml-parser
      "*Free Dictionary*"
      "http://www.thefreedictionary.com/_/WoD/rss.aspx"
      (let* ((item (car (xml-get-children
                         (car (xml-get-children (car it) 'channel))
                         'item)))
             (title (nth 2 (car (xml-get-children item 'title))))
             (href (nth 2 (car (xml-get-children item 'link))))
             (date (nth 2 (car (xml-get-children item 'pubDate))))
             (description (nth 2 (car (xml-get-children item 'description)))))
        (format "<h1><a href=\"%s\">%s</a></h1><p>%s</p><p>%s</p>"
                href title date description))))

(defun wotd--get-oxford-english-dictionary ()
  (wotd--def-xml-parser
      "*Oxford English Dictionary*"
      "http://www.oed.com/rss/wordoftheday"
      (let* ((item (car (last (xml-get-children
                               (car (xml-get-children (car it) 'channel))
                               'item))))
             (title (nth 2 (car (xml-get-children item 'title))))
             (href (nth 2 (car (xml-get-children item 'link))))
             (date (nth 2 (car (xml-get-children item 'pubDate))))
             (description (nth 2 (car (xml-get-children item 'description)))))
        (format "<h1><a href=\"%s\">%s</a></h1><p>%s</p><p>%s</p>"
                href title date description))))

(defun wotd--get-urban-dictionary ()
  (wotd--def-xml-parser
      "*Urban Dictionary*"
      "http://feeds.urbandictionary.com/UrbanWordOfTheDay"
      (let* ((item (car (xml-get-children
                         (car (xml-get-children (car it) 'channel))
                         'item)))
             (title (nth 2 (car (xml-get-children item 'title))))
             (href (nth 2 (car (xml-get-children item 'link))))
             (date (nth 2 (car (xml-get-children item 'pubDate))))
             (description (nth 2 (car (xml-get-children item 'description)))))
        (format "<h1><a href=\"%s\">%s</a></h1><p>%s</p><p>%s</p>"
                href title date description))))

(defun wotd--get-wordthink ()
  (wotd--def-xml-parser
      "*WordThink*"
      "http://www.wordthink.com/feed/"
      (let* ((item (car (xml-get-children
                         (car (xml-get-children (car it) 'channel))
                         'item)))
             (title (nth 2 (car (xml-get-children item 'title))))
             (href (nth 2 (car (xml-get-children item 'link))))
             (date (nth 2 (car (xml-get-children item 'pubDate))))
             (description (nth 2 (car (xml-get-children item 'description)))))
        (format "<h1><a href=\"%s\">%s</a></h1><p>%s</p><p>%s</p>"
                href title date description))))

(defun wotd--get-oxford-dictionaries ()
  (wotd--def-html-parser
      "*Oxford Dictionaries*"
      "https://en.oxforddictionaries.com/"
    (when (re-search-forward
           "Word of the Day.*?<a.*?>\\(.*?\\)</a>"
           nil
           t)
      (let* ((title (match-string 1))
             (href (format "https://en.oxforddictionaries.com/definition/%s" title)))
        (format "<h1>%s</h1><a href=\"%s\">See the definition and examples</a>" title href)))))

(defun wotd--get-cambridge-dictionary ()
  (wotd--def-html-parser
      "*Cambridge Dictionary*"
      "http://dictionary.cambridge.org/us/"
    (when (re-search-forward
           "<p class=\"h4 feature-w-big wotd-hw\">\\(.*?\\)</p><p>\\(.*\\)</p>"
           nil
           t)
      (let* ((title (match-string 1))
             (href (format "http://dictionary.cambridge.org/us/dictionary/british/%s"
                           (url-hexify-string title)))
             (description (match-string 2)))
        (format "<h1><a href=\"%s\">%s</a></h1><p>%s</p>" href title description)))))

(defun wotd--get-collins-dictionary ()
  (wotd--def-html-parser
      "*Collins Dictionary*"
      "https://www.collinsdictionary.com/dictionary/english"
    (replace-regexp "\n" "")
    (goto-char (point-min))
    (when (re-search-forward
           "Word of the day.*?\"promoBox-title\">\\(.*?\\)</div>.*?\"promoBox-description\">\\(.*?\\)</div>"
           nil
           t)
      (let* ((title (match-string 1))
             (href (format "https://www.collinsdictionary.com/dictionary/english/%s"
                           (url-hexify-string (replace-regexp-in-string " " "-" title))))
             (description (match-string 2)))
        (format "<h1><a href=\"%s\">%s</a></h1><p>%s</p>" href title description)))))

(defun wotd--get-learners-dictionary ()
  (wotd--def-html-parser
      "*Learners Dictionary*"
      "http://learnersdictionary.com/word-of-the-day"
    (let* ((beg (re-search-forward "<!--WOD content-->" nil t))
           (end (re-search-forward "<!--WOD Archive-->" nil t))
           (content (buffer-substring beg end)))
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (replace-regexp "[\r\n]" "")
        (goto-char (point-min))
        (replace-regexp "<span class = \"hpron_word voces_font\">/.*?/</span>\\|\
<!--headword: mobile view-->.*<!--hwpost-->" "")
        (string-join
         (delq nil
               (mapcar
                (lambda (ch) (encode-coding-char ch 'utf-8 'unicode))
                (buffer-string))))))))

(defun wotd--get-wordnik ()
  (wotd--def-html-parser
      "*Wordnik*"
      "https://www.wordnik.com/word-of-the-day"
    (let* ((beg (re-search-forward "<div class=\"word_of_the_day\">" nil t))
           (end (re-search-forward "<!-- Wordnik announcement -->" nil t)))
      (buffer-substring beg end))))


(provide 'word-of-the-day)
;;; word-of-the-day.el ends here
