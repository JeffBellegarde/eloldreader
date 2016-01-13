;;; eloldreader.el --- Emacs interface to oldreader.com -*- lexical-binding: t -*-

;; Copyright (C) 2016  Jeff Bellegarde

;; Author: Jeff Bellegarde <bellegar@gmail.com>
;; Version: 0.0.1

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

;;; Code:
(require 'request)
(require 'json)
(require 'hl-line)
(require 'dash)
(require 's)

(eval-when-compile
  (require 'cl))

(defun eloldreader-libxml-supported-p ()
  "Return non-nil if `libxml-parse-html-region' is available."
  (with-temp-buffer
    (insert "<html></html>")
    (and (fboundp 'libxml-parse-html-region)
         (not (null (libxml-parse-html-region (point-min) (point-max)))))))

(defvar eloldreader-root-id (intern "user/-/state/com.google/root")
  "The special id used to identify the root feed group.")

(defvar eloldreader-auth-token nil
  "The auth key to access your OldReader account.
DO NOT save this in a public place.")

(defun eloldreader-insert-html (html &optional base-url)
  "Converted HTML markup to a propertized string."
  (let ((parsed-html    (if (eloldreader-libxml-supported-p)
                            (with-temp-buffer
                              ;;(message "in temp buffer")
                              ;; insert <base> to work around libxml-parse-html-region bug
                              (insert (format "<base href=\"%s\">" base-url))
                              (insert html)
                              ;;(message "parsing")
                              (libxml-parse-html-region (point-min) (point-max) base-url))
                          ;;(message "can not insert")
                          '(i () "Eloldreader: libxml2 functionality is unavailable"))))
    (shr-insert-document parsed-html)))

(defvar eloldreader-feeds-mode-map
     (let ((map (make-sparse-keymap)))
       (prog1 map
         (suppress-keymap map)
         (define-key map "q" 'quit-window)
         (define-key map "g" 'eloldreader-feeds-update)
         ;;    (define-key map "G" 'elfeed-update)
         ;;  (define-key map (kbd "RET") 'elfeed-search-show-entry)
         (define-key map "n" 'next-line)
         (define-key map "p" 'previous-line)))
     "Keymap for eloldreader-feeds-mode.")

(defun eloldreader-feeds-mode ()
  "Major mode for listing elfeed feed entries.
\\{eloldread-feedsmode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map eloldreader-feeds-mode-map)
  (setq major-mode 'eloldreader-feeds-mode
        mode-name "eloldreader-feeds"
        truncate-lines t
        buffer-read-only t)
  (buffer-disable-undo)
  (hl-line-mode)
;;  (add-hook 'elfeed-update-hooks #'eloldreader-feeds-update)
  (run-hooks 'eloldreader-feeds-mode-hook))


(defun eloldreader-feeds-buffer ()
  (get-buffer-create "*eloldreader-feeds-view*"))

(defun eloldreader-feeds-insert-header-text (text)
  "Insert TEXT into buffer using header face."
  (insert (propertize text 'face '(widget-inactive italic))))

(defvar eloldreader-feeds-last-update 0)

(defvar eloldreader-subscriptions (make-hash-table :test 'equal)
  "All known subscritions. Updated by 'eloldreader-fetch-subscriptions'.")

(defvar eloldreader-unread-counts (make-hash-table :test 'equal)
  "Current Unread counts. Updated by 'eloldreader-fetch-unread-counts'.")

(defvar eloldreader-current-articles (make-hash-table :test 'equal)
  "The list of current articles")
(defun eloldreader-feeds-fetch ()
  ;; How to wait for succes?
  (eloldreader-fetch-subscriptions)
  (eloldreader-fetch-unread-counts))

(defun eloldreader-feeds-update ()
  "Update the elfeed-search buffer listing to match the database.
When FORCE is non-nil, redraw even when the database hasn't changed"
  (interactive)
  (with-current-buffer (eloldreader-feeds-buffer)
    (save-excursion
      (let ((inhibit-read-only t)
            (standard-output (current-buffer)))
        (erase-buffer)
        (setf eloldreader-feeds-last-update (float-time))
        (eloldreader-feeds-insert-header-text
         (format "Eloldreader update: %s" (format-time-string "%B %e %Y %H:%M:%S %Z")))
        (insert "\n")
        (if (or (eq 0 (hash-table-count eloldreader-unread-counts))
                  (eq 0 (hash-table-count eloldreader-subscriptions)))
            (insert "Still waiting for data.\n")
          (maphash (lambda (id _v)
                     (insert " ")
                     (lexical-let ((show-feed (lambda (_button)
                                                (eloldreader-show-headers id))))
                       (insert-text-button (format "%s %s %s" id (eloldreader-display-name-for-id id) (eloldreader-unread-count-for-id id))
                                           'action show-feed
                                           'follow-link t
                                           'help-echo "show feed"))
                     (insert "\n"))
                   eloldreader-unread-counts))
        ;;        (elfeed-search--update-list)
        ;; (dolist (entry elfeed-search-entries)
        ;;   (elfeed-search-print entry)
        ;;   (insert "\n"))
        (insert "End of entries.\n")))))



(defun eloldreader-process-subscriptions (data)
  (clrhash eloldreader-subscriptions)
  (let ((subs (cdr (assoc 'subscriptions data))))
    (dolist (entry subs)
      (let* ((id-entry (assoc 'id entry))
             (id (cdr id-entry)))
        (puthash id entry eloldreader-subscriptions)))))

(defun eloldreader-fetch-subscriptions ()
  (request "https://theoldreader.com/reader/api/0/subscription/list"
           :params '(("output" . "json"))
           :headers (eloldreader-authorization-headers)
           :parser (lambda ()
                     (let ((json-array-type 'list))
                       (json-read)))
           :success (function*
                     (lambda (&key data &allow-other-keys)
                       (eloldreader-process-subscriptions data)))))
;;(eloldreader-fetch-subscriptions)


(defvar eloldreader-feed-display-order nil
  )

(defun eloldreader--break-into-groups (n input-string)
  (let ((result ())
        (input-list (split-string input-string "" t)))
    (while input-list
      (let* ((parts (-split-at n input-list))
             (head (car parts))
             (tail (cadr parts)))
        (push (s-append  (s-join "" head) "feed/") result)
         (setq input-list tail)))
    (reverse result)))

(defun eloldreader--subscription-order-item-p (item)
  (eql 'subscription-ordering (cdr (assoc 'id item))))

(defun eloldreader-subscription-order (group)
  "pull out the subscription-ordering value of an group.

'(((value . true) (id . is-expanded)) ((value . 55ee6f07091452ec0200010655ee6f02091452ec02000071) (id . subscription-ordering)))
=> '(\"55ee6f07091452ec020001065\", \"5ee6f02091452ec02000071\")

Need to break it into a list.
"
  (eloldreader--break-into-groups 24 (symbol-name (cdr (assoc 'value (-first 'eloldreader--subscription-order-item-p group))))))

(defun eloldreader-fetch-preferences ()
  (request "https://theoldreader.com/reader/api/0/preference/stream/list"
           :params '(("output" . "json"))
           :headers (eloldreader-authorization-headers)
           :parser (lambda ()
                     (let ((json-array-type 'list))
                       (json-read)))
           :success (function*
                     (lambda (&key data &allow-other-keys)
                       ;;(message "I received data")
                       ;;(message "I received: %s" data)
                       (setq eloldreader-feed-display-order '())
                       (let* ((streamprefs  (cdr (assoc 'streamprefs data))))
                         (message "sp type %s:" (type-of streamprefs))
                          streamprefs)))))
;;(eloldreader-fetch-preferences)

(defun eloldreader-display-name-for-id (id)
  (let* ((entry (gethash id eloldreader-subscriptions '((title . id))))
         (entry-name (assoc 'title entry)))
    ;;(message "entry %s" entry)
    ;;(message "entry-count %s" entry-count)
    (cdr entry-name )))

(defun eloldreader-process-unread-counts (data)
  (clrhash eloldreader-unread-counts)
  (let ((subs (cdr (assoc 'unreadcounts data))))
    (dolist (entry subs)
      (let* ((id-entry (assoc 'id entry))
             (id (cdr id-entry)))
        (puthash id entry eloldreader-unread-counts)))))

(defun eloldreader-authorization-headers ()
  '(("Authorization" . ,(concat  "GoogleLogin auth="
                                 eloldreader-authorization-headers))))

(defun eloldreader-fetch-unread-counts ()
  (request "https://theoldreader.com/reader/api/0/unread-count?output=json"
           :params '(("output" . "json"))
           :headers (eloldreader-authorization-headers)
           :parser (lambda ()
                     (let ((json-array-type 'list))
                       (json-read)))
           :success (function*
                     (lambda (&key data &allow-other-keys)
                       (eloldreader-process-unread-counts data)))))
;;(eloldreader-fetch-unread-counts)

(defun eloldreader-unread-count-for-id (id)
  (let* ((entry (gethash id eloldreader-unread-counts '((count . 0))))
         (entry-count (assoc 'count entry)))
    ;;(message "entry %s" entry)
    ;;(message "entry-count %s" entry-count)
    (cdr entry-count )))

;;(eloldreader-unread-count-for-id "feed/514e26ca54cf82144501a934")


;;Show headers view
(defvar eloldreader-headers-view-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "q" 'quit-window)
    ;;  (define-key map "g" 'eloldreader-feeds-update)
  ;;    (define-key map "G" 'elfeed-update)
    ;;  (define-key map (kbd "RET") 'elfeed-search-show-entry)
      (define-key map (kbd "SPC") 'eloldreader-scroll-or-next-article)
      (define-key map "n" 'eloldreader-next-article)
      (define-key map "p" 'eloldreader-prev-article)))
  "Keymap for eloldreader-headers-view-mode.")

(defun eloldreader-headers-view-mode ()
  "Major mode for listing elfeed feed entries.
\\{eloldread-feedsmode-map}"
  (kill-all-local-variables)
  (use-local-map eloldreader-headers-view-mode-map)
  (setq major-mode 'eloldreader-headers-view-mode
        mode-name "eloldreader-headers-view"
        truncate-lines t
        buffer-read-only t)
  (buffer-disable-undo)
  (hl-line-mode)
;;  (add-hook 'elfeed-update-hooks #'eloldreader-fetch-headers)
  (run-hooks 'eloldreader-headers-view-mode-hook))

(defun eloldreader-show-headers (id)
  (interactive)
  (switch-to-buffer (eloldreader-headers-view-buffer))
  (unless (eq major-mode 'eloldreader-headers-view-mode)
    (eloldreader-headers-view-mode))
  (eloldreader-fetch-headers id))

;; (eloldreader-show-headers "feed/53c2dd58c70bc2da4b0005a6")

(defun eloldreader-current-articles-update (items)
  (clrhash eloldreader-current-articles)
  (dolist (item items)
    (puthash (cdr (assoc 'id item)) item eloldreader-current-articles))
  (message "saved %s articles" (hash-table-size eloldreader-current-articles))
  (eloldreader-current-articles-show))

(defvar eloldreader-current-article-id nil
  "The id of the currently selected article.
Update with 'eloldreader-show-article'.")

(defun eloldreader-article-hide-current ()
  ;;  (message "hiding %s" eloldreader-current-article-id)
  (add-to-invisibility-spec eloldreader-current-article-id))

(defun eloldreader-article-show-current ()
  ;;(message "showing %s" eloldreader-current-article-id)
  (remove-from-invisibility-spec eloldreader-current-article-id))

(defun eloldreader-scroll-or-next-article ()
  (interactive)
  (let* ((next-button-line (save-excursion
                            (forward-button 1)
                            (line-number-at-pos)))
         (next-page-line (+ (line-number-at-pos) (window-size))))
    (if (< next-button-line next-page-line)
        (eloldreader-next-article)
      (scroll-up))))
             
(defun eloldreader-show-article (id)
  ;;(message "showing article %s" id)
  (eloldreader-article-hide-current)
  (setq eloldreader-current-article-id id)
  (eloldreader-article-show-current))

(defun eloldreader-next-article ()
  (interactive)
  (forward-button 1)
  (push-button)
  (recenter 1)
  )

(defun eloldreader-prev-article ()
  (interactive)
  (backward-button 1)
  (push-button)
  (recenter 1)
  )

(defun eloldreader-current-articles-show ()
  (interactive)
  (with-current-buffer (eloldreader-headers-view-buffer)
    (setq buffer-invisibility-spec t)
    (save-excursion
      (let ((inhibit-read-only t)
            (standard-output (current-buffer)))
        (erase-buffer)
        (maphash (lambda (id item)
                   (lexical-let* ((title (or (cdr (assoc 'title item))
                                             "<No title>"))
                                  (content (cdr (assoc 'content (cdr (assoc 'summary item)))))
                                  (show-article (lambda (_button)
                                                  (eloldreader-show-article id)))
                                  ;; (hide-article (lambda (old-point new-point)
                                  ;;                 (message "hide article %s %s" id title)
                                  ;;                 ;;(eloldreader-show-article id)
                                  ;;                 ))
                                  ;; (label (propertize (format "%s" title )
                                  ;;                    'mouse-face 'highlight
                                  ;;                    'action show-article))
                                        )
                     ;;(insert "Id: %s" id)

                     (insert-text-button title
                                         'action show-article
                                         'follow-link t
                                         'help-echo "show article")
                     (insert "\n")
                     (let ((start (point)))
                       (insert "\n")
                       (eloldreader-insert-html content)
                       (let ((end (point)))
                         (add-text-properties start end (list 'invisible id))
                         (add-to-invisibility-spec id)))))
                 eloldreader-current-articles)
        (insert "End of entries.\n")))))

(defun eloldreader-fetch-headers (feed_id)
  (interactive "sFeed_id: ")
  ;;  (message "Requesting for %s" feed_id)
  (request "https://theoldreader.com/reader/api/0/stream/contents"
           :params (list `("output" . "json")
                         (cons "s"  feed_id)
                         '("r" . "o"))
           :headers (eloldreader-authorization-headers)
           :parser (lambda ()
                     (let ((json-array-type 'list))
                       (json-read)))
           :success (function*
                     (lambda (&key data &allow-other-keys)
                       (message "received headers")
                       (eloldreader-current-articles-update (cdr (assoc 'items data)))))))

(defun eloldreader-headers-view-buffer ()
  (get-buffer-create "*eloldreader-headers-view*"))


;;;###autoload
(defun eloldreader ()
  "Enter elfeed."
  (interactive)
  (switch-to-buffer (eloldreader-feeds-buffer))
  (unless (eq major-mode 'eloldreader-feeds-mode)
    (eloldreader-feeds-mode))
  (eloldreader-feeds-update))

(provide 'eloldreader)

;;; eloldreader.el ends here
