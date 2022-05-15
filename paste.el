;;; paste.el -- Paste to pastebin-like services

;; Copyright (c) 2022 zerosign

;; Author: zerosign
;; Maintainer: zerosign
;; URL: https://github.com/zerosign/paste.el
;; Package-Version: 0.0.1
;; Version: 0.0.1
;; Keywords: pastebin, paste
;; Package-Requires: ((emacs "26") (request "")

(require 'request)
(require 'json)

;;;###autoload
(defgroup paste nil
  "Configuration group for paste.el where you could customize paste providers"
  :group 'paste)

(defcustom paste-provider-priority ()
  "Priority for the provider"
  :group 'paste
  :type '(repeat string))

(defcustom paste-encryption-algorithm ()
  "Encrypt the content before uploading to external service"
  :group 'paste
  ;; 'gpg 'pass nil
  :type 'symbol
  :default nil)

(defcustom paste-browse-url t
  "After paste to kill ring run xdg-open browser for given url"
  :group 'paste
  :type 'boolean)

(defcustom paste-pass-account-provider t
  "paste account provider"
  :group 'paste
  :type '(alist :value-type string))

(defun paste-region--column-at (point)
  "Return column number at POINT."
  (save-excursion
    (goto-char point)
    (current-column)))

(defun paste-buffer ()
  "paste current buffer"
  (interactive "b")
  (let* ((buffer-name buffer-file-name)
	 (content (buffer-substring-no-properties (point-min) (point-max))))
    (message "%S %S" buffer-name content)))

;; using GPG
(defun paste-encrypt-content (text-content)
  "encrypt paste content before uploading to service")

(defun paste-region ()
  "paste given region"
  (interactive "r")
  (let* ((buffer-name (if (eq buffer-file-name "")
			  ("scratch"))
		      (buffer-file-name))
	 (start (region-beginning))
	 (end   (region-end))
	 (cstart (paste-region--column-at start))
	 (cend   (paste-region--column-at end))
	 (lstart (line-number-at-pos start))
	 (lend   (line-number-at-pos  end))
	 (file-name (format "%s" buffer-name))
	 (content (buffer-substring-no-properties start end)))
    (message "%d, %d, %d, %d" cstart cend lstart lend)
    (message "%S %S" buffer-file-name content)))

;;
;; curl --request POST "https://gitlab.example.com/api/v4/snippets" \
;;      --header 'Content-Type: application/json' \
;;      --header "PRIVATE-TOKEN: <your_access_token>" \
;;      -d '{ "title" : "test.txt", "visibility" : "internal", "files" : [{ "content" : "test", "file_path" : "test.txt" }]}'
;;
;; url: "gitlab.com/api/v4/snippets"
;;
(defun paste-gitlab (url filename text-content)
  "paste given text-content to gitlab"
  (let* ((pass (auth-source-pass--read-entry url))
	 (request-url (format "https://%s" url))
	 (pass-header (format "Bearer %s" pass)))
    (request url
      :type "POST"
      :encoding 'utf-8
      :headers (list (cons "Content-Type" "application/json")
		     (cons "PRIVATE-TOKEN" pass))
      :data (json-encode (list (cons "title" filename)
			       (cons "visibility" "private")
			       (cons "files" (list (list (cons "content" text-content)
							 (cons "file_path" filename))))))
      :parser 'json-read
      :success
      (cl-function
       (lambda (&key data &allow-other-keys)
	 (let* ((target-url (format "%s" (assoc-default 'web_url data))))
	   (message "url: %S" target-url)
	   (kill-new target-url)))))))

;;
;; curl -H "Accept: application/vnd.github.v3+json" \
;;      -H "Authentication: <TOKEN>" \
;;      https://api.github.com/gists \
;;      -d '{ "public" : false, "files" : { "content" : "hello world" } }'
;;
;; url: api.github.com/gists
(defun paste-gist (url filename text-content)
  "paste "
  (let* ((pass (auth-source-pass--read-entry url))
	 (request-url (format "https://%s" url))
	 (pass-header (format "Bearer %s" pass)))
    (request request-url
      :type "POST"
      :encoding 'utf-8
      :headers (list (cons "Accept" "application/vnd.github.v3+json")
		     (cons "Authorization" pass-header))
      :data (json-encode (list (cons "public" :json-false)
			       (cons "files"
				     (list (cons "filename" (list (cons "content" text-content)))))))
      :parser 'json-read
      :success
      (cl-function
       (lambda (&key data &allow-other-keys)
	 (let* ((target-url (format "%s" (assoc-default 'html_url data))))
	   (message "url: %S" target-url)
	   (kill-new target-url)))))))

;;
;; curl -X POST paste.sr.ht/api/pastes -H "Authorization: token <TOKEN>" \
;;                                     -d '{"visibility": "access level", "files": [ { "filename": "filename" or null, (optional) "contents": "contents of this file"}]}'
;;
;; https://man.sr.ht/paste.sr.ht/api.md
(defun paste-sourcehut (url filename text-content)
  "paste into sourcehut paste.sr.ht"
  (let* ((pass (auth-source-pass--read-entry url))
	 (request-url (format "https://%s" url))
	 (pass-header (format "token %s" pass)))
    (request request-url
      :type "POST"
      :encoding 'utf-8
      :headers (list (cons "Authorization" pass-header))
      :data (json-encode (list (cons "visibility" "private")
			       (cons "files" (list (cons "filename" (list (cons "filename" filename)
									  (cons "content" text-content)))))))
      :parser 'json-read
      :success
      (cl-function
       (lambda (&key data &allow-other-keys)
	 (let* ((target-url (format "https://%s/api/pastes/%s" request-url (assoc-default 'sha data))))
	   (message "url: %S" target-url)
	   (kill-new target-url)))))))


;; curl -X POST -d 'api_dev_key=YOUR API DEVELOPER KEY' \
;;              -d 'api_paste_code=test' \
;;              -d 'api_option=paste'
;;              "https://pastebin.com/api/api_post.php"
;;
(defun paste-pastebin (url filename text-content)
  "")

;; echo "hello world" | ipfs files write --create --truncate --parents "/gists/$USER_NAME/$(date -I"minutes")-hello.txt"
;; ipfs files stat "/gists/$USER_NAME/2022-05-15T14:49+07:00-hello.txt" --hash
(defun paste-ipfs (url filename text-content)
  "paste content into your local ipfs")

(provide 'paste)
