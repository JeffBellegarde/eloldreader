(require 'request)
(require 'hl-line)
(require 'el-mock)
(require 'eloldreader)

(eval-when-compile
  (require 'cl))
(defconst eloldreader-test-feeds-buffer-name "*eloldreader-test-buffer*")
(defmacro eloldreader-safe (&rest body)
  "Set up a safe environment for eloldreader tests."
  `(let ((eloldreader-feeds-last-update 0)
         (eloldreader-subscriptions (make-hash-table :test 'equal))
         (eloldreader-unread-counts (make-hash-table :test 'equal))
         (eloldreader-current-articles (make-hash-table :test 'equal))
         (eloldreader-feed-display-order nil)
         (eloldreader-current-article-id nil))
     (letf (((symbol-function 'eloldreader-feeds-buffer)
             (lambda () (get-buffer-create eloldreader-test-feeds-buffer-name))))
       (unwind-protect
           (with-temp-buffer
             ,@body)
         (when (get-buffer eloldreader-test-feeds-buffer-name)
           (kill-buffer eloldreader-test-feeds-buffer-name))))))

(defvar eloldreader-request-mock-retrieve-content ""
  "The content to be returned.
Set using `eloldreader-request-mock-retrieve`")
(defun* eloldreader-request-mock-retrieve (url &rest settings
                                               &key type data timeout response
                                               &allow-other-keys
                                               &aux headers)
  (let* ((buffer (get-buffer-create "*eloldreader-test-mock-response*")))
    (with-current-buffer buffer
      (setf (request-response--buffer response) buffer)
      (insert eloldreader-request-mock-retrieve-content)
      (setf (request-response-status-code response) "200 OK")
      (apply #'request--callback buffer settings))))

(defmacro eloldreader-with-fake-response (data &rest body)
  `(let ((eloldreader-request-mock-retrieve-content ,data))
     (mocklet ((request--choose-backend => 'eloldreader-request-mock-retrieve))
       ,@body)))

(ert-deftest eloldreader-libxml-supported-p ()
  (eloldreader-safe
   (should (eql (eloldreader-libxml-supported-p) t))))

(ert-deftest eloldreader-feeds-mode ()
  (eloldreader-safe
   (eloldreader-feeds-mode)
   (should (eql (current-local-map) eloldreader-feeds-mode-map))
   (should (eql major-mode 'eloldreader-feeds-mode))
   (should (eql truncate-lines t))
   (should (eql buffer-read-only t))
   (should (eql buffer-undo-list t))
   (should (eql hl-line-mode t))))

(ert-deftest eloldreader-feeds-update-no-data ()
  (eloldreader-safe
   (eloldreader-feeds-update)
   (with-current-buffer (eloldreader-feeds-buffer)
     (should (equal (buffer-name) eloldreader-test-feeds-buffer-name))
     (should (equal "Eloldreader update: " (buffer-substring (point-min) 21)))
     (forward-line)
     (should (equal "Still waiting for data." (buffer-substring (line-beginning-position) (line-end-position))))
     (forward-line)
     (should (equal "End of entries." (buffer-substring (line-beginning-position) (line-end-position)))))))


(defun eloldreader-test-create-feed (guid title unread-count)
  (let ((feed-id (format "feed/%s" guid)))
    (puthash feed-id
             (list '(iconUrl . "//s.theoldreader.com/system/uploads/feed/picture/50e2/ea2f/e721/eca4/bd00/icon_5c3e.ico")
                   '(htmlUrl . "http://www.avasdemon.com/")
                   '(url . "http://feeds.feedburner.com/AvasDemon")
                   '(firstitemmsec . "1437613621000")
                   (cons 'sortid guid)
                   '(categories)
                   (cons 'title title)
                   (cons 'id feed-id))
             eloldreader-subscriptions)
    (puthash feed-id
             (list '(newestItemTimestampUsec . "1437613621000000")
                   (cons 'count unread-count)
                   (cons 'id feed-id))
             eloldreader-unread-counts)))

(ert-deftest eloldreader-feeds-update-with-data ()
  (eloldreader-safe
   (let* ((guid "53c2dd58c70bc2da4b0005a6")
          (title "Ava's Demon")
          (unread-count 1)
          (feed-id (format "feed/%s" guid)))
     (eloldreader-test-create-feed guid title unread-count)
     (mocklet (((eloldreader-show-headers feed-id)))
       (eloldreader-feeds-update)
       (with-current-buffer (eloldreader-feeds-buffer)
         (should (equal (buffer-name) eloldreader-test-feeds-buffer-name))
         (should (equal "Eloldreader update: " (buffer-substring (point-min) 21)))
         (forward-line)
         (forward-char)
         (let* ((item-line (buffer-substring (line-beginning-position) (line-end-position)))
                (line-properties (text-properties-at (point))))
           (should (equal (format  " %s %s %s" feed-id title 1) item-line))
           (should (equal '(help-echo "show feed" follow-link t action nil category default-button button (t))
                          (lax-plist-put (append line-properties ()) 'action nil)))
           (button-activate (button-at (point))))
         (forward-line)
         (should (equal "End of entries." (buffer-substring (line-beginning-position) (line-end-position)))))))))

(ert-deftest eloldreader-fetch-subscriptions ()
  (eloldreader-safe
   (mocklet (((eloldreader-process-subscriptions '((subscriptions ((id . "feed/abc")))))))
     (eloldreader-with-fake-response "{\"subscriptions\":[{\"id\":\"feed\\/abc\"}]})"
      (eloldreader-fetch-subscriptions)))))

(ert-deftest eloldreader-process-subscriptions ()
  (eloldreader-safe
   (eloldreader-process-subscriptions  '((subscriptions ((other . "other") (id . "id")))))
   (should (equal 1 (hash-table-count eloldreader-subscriptions)))
   (should (equal '((other . "other") (id . "id")) (gethash "id" eloldreader-subscriptions)))))

(ert-deftest eloldreader-fetch-unread-counts ()
  (eloldreader-safe
   (mocklet (((eloldreader-process-unread-counts '((unreadcounts ((id . "feed/abc")))))))
     (eloldreader-with-fake-response "{\"unreadcounts\":[{\"id\":\"feed\\/abc\"}]})"
                                     (eloldreader-fetch-unread-counts)))))

(ert-deftest eloldreader-process-unread-counts ()
  (eloldreader-safe
   (eloldreader-process-unread-counts  '((unreadcounts ((other . "other") (id . "id")))))
   (should (equal 1 (hash-table-count eloldreader-unread-counts)))
   (should (equal '((other . "other") (id . "id")) (gethash "id" eloldreader-unread-counts)))))

(ert-deftest eloldreader ()
  (eloldreader-safe
   (mocklet (((eloldreader-feeds-mode))
             ((eloldreader-feeds-update)))
     (eloldreader))))

(ert-deftest eloldreader-show-headers ()
  (eloldreader-safe
   (let ((id "some-id"))
     (mocklet (((eloldreader-headers-view-mode))
               ((eloldreader-fetch-headers id)))
       (eloldreader-show-headers id)))))

(ert-deftest eloldreader-headers-view-mode ()
  (eloldreader-safe
   (eloldreader-headers-view-mode)
   (should (eql (current-local-map) eloldreader-headers-view-mode-map))
   (should (eql major-mode 'eloldreader-headers-view))
   (should (eql truncate-lines t))
   (should (eql buffer-read-only t))
   (should (eql buffer-undo-list t))
   (should (eql hl-line-mode t))))

(ert-deftest eloldreader-fetch-headers ()
  (eloldreader-safe
   (eloldreader-with-fake-response
    "{\"items\":[{\"id\":\"feed\\/abc\"}]}"
    (mocklet (((eloldreader-current-articles-update '(((id . "feed/abc"))))))
      (eloldreader-fetch-headers "id")))))

