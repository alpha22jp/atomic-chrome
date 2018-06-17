;;; atomic-chrome.el --- Edit Chrome text area with Emacs using Atomic Chrome

;; Copyright (C) 2016 alpha22jp <alpha22jp@gmail.com>

;; Author: alpha22jp <alpha22jp@gmail.com>
;; Package-Requires: ((emacs "24.3") (let-alist "1.0.4") (websocket "1.4"))
;; Keywords: chrome edit textarea
;; URL: https://github.com/alpha22jp/atomic-chrome
;; Version: 2.0.0

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 51
;; Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is the Emacs version of Atomic Chrome which is an extension for Google
;; Chrome browser that allows you to edit text areas of the browser in Emacs.
;;
;; It's similar to Edit with Emacs, but has some advantages as below with the
;; help of websocket.
;;
;; * Live update
;;   The input on Emacs is reflected to the browser instantly and continuously.
;; * Bidirectional communication
;;   You can edit both on the browser and Emacs, they are synced to the same.
;;
;; Firefox is supported via the GhostText browser addon.

;;; Code:

(eval-when-compile (require 'cl))
(require 'json)
(require 'let-alist)
(require 'subr-x)
(require 'websocket)

(defgroup atomic-chrome nil
  "Edit browser text area with Emacs using Atomic Chrome or GhostText."
  :prefix "atomic-chrome-"
  :group 'applications)

(defcustom atomic-chrome-extension-type-list '(atomic-chrome ghost-text)
  "List of browser extension type available."
  :type '(repeat (choice (const :tag "Atomic Chrome" atomic-chrome)
                         (const :tag "Ghost Text" ghost-text)))
  :group 'atomic-chrome)

(defcustom atomic-chrome-buffer-open-style 'split
  "Specify the style to open new buffer for editing."
  :type '(choice (const :tag "Open buffer with full window" full)
                 (const :tag "Open buffer with splitted window" split)
                 (const :tag "Open buffer with new frame" frame))
  :group 'atomic-chrome)

(defcustom atomic-chrome-buffer-frame-width 80
  "Width of editing buffer frame."
  :type 'integer
  :group 'atomic-chrome)

(defcustom atomic-chrome-buffer-frame-height 25
  "Height of editing buffer frame."
  :type 'integer
  :group 'atomic-chrome)

(defcustom atomic-chrome-server-ghost-text-port 4001
  "HTTP server port for Ghost Text."
  :type 'integer
  :group 'atomic-chrome)

(defcustom atomic-chrome-enable-auto-update t
  "If non-nil, edit on Emacs is reflected to the browser instantly, \
otherwise you need to type \"C-xC-s\" manually."
  :type 'boolean
  :group 'atomic-chrome)

(defcustom atomic-chrome-enable-bidirectional-edit t
  "If non-nil, you can edit both on the browser text area and Emacs, \
otherwise edit on browser is ignored while editing on Emacs."
  :type 'boolean
  :group 'atomic-chrome)

(defcustom atomic-chrome-default-major-mode 'text-mode
  "Default major mode for editing buffer."
  :type 'function
  :group 'atomic-chrome)

(defcustom atomic-chrome-url-major-mode-alist nil
  "Association list of URL (or, for GhostText, hostname) regexp \
and corresponding major mode which is used to select major mode \
for specified website."
  :type '(alist :key-type (regexp :tag "regexp")
                :value-type (function :tag "major mode"))
  :group 'atomic-chrome)

(defcustom atomic-chrome-edit-mode-hook nil
  "Customizable hook which run when the editing buffer is created."
  :type 'hook
  :group 'atomic-chrome)

(defcustom atomic-chrome-edit-done-hook nil
  "Customizable hook which run when the editing buffer is closed."
  :type 'hook
  :group 'atomic-chrome)

(defvar atomic-chrome-server-atomic-chrome nil
  "Websocket server connection handle for Atomic Chrome.")

(defvar atomic-chrome-server-ghost-text nil
  "Websocket server connection handle for Ghost Text.")

(defvar atomic-chrome-buffer-table (make-hash-table :test 'equal)
  "Hash table of editing buffer and its assciated data.
Each element has a list consisting of (websocket, frame).")

(defun atomic-chrome-get-websocket (buffer)
  "Lookup websocket associated with buffer BUFFER \
from `atomic-chrome-buffer-table'."
  (nth 0 (gethash buffer atomic-chrome-buffer-table)))

(defun atomic-chrome-get-frame (buffer)
  "Lookup frame associated with buffer BUFFER \
from `atomic-chrome-buffer-table'."
  (nth 1 (gethash buffer atomic-chrome-buffer-table)))

(defun atomic-chrome-get-buffer-by-socket (socket)
  "Lookup buffer which is associated to the websocket SOCKET \
from `atomic-chrome-buffer-table'."
  (let (buffer)
    (cl-loop for key being the hash-keys of atomic-chrome-buffer-table
             using (hash-values val)
             do (when (equal (nth 0 val) socket) (setq buffer key)))
    buffer))

(defun atomic-chrome-close-connection ()
  "Close client connection associated with current buffer."
  (let ((socket (atomic-chrome-get-websocket (current-buffer))))
    (when socket
      (remhash (current-buffer) atomic-chrome-buffer-table)
      (websocket-close socket))))

(defun atomic-chrome-send-buffer-text ()
  "Send request to update text with current buffer content."
  (interactive)
  (let ((socket (atomic-chrome-get-websocket (current-buffer)))
        (text (buffer-substring-no-properties (point-min) (point-max))))
    (when (and socket text)
      (websocket-send-text
       socket
       (json-encode
        (if (eq (websocket-server-conn socket) atomic-chrome-server-ghost-text)
            (list (cons "text" text))
          (list '("type" . "updateText")
                (cons "payload" (list (cons "text" text))))))))
    (set-buffer-modified-p nil)))

(defun atomic-chrome-set-major-mode (url)
  "Set major mode for editing buffer depending on URL.
`atomic-chrome-url-major-mode-alist' can be used to select major mode.
The specified major mode is used if URL matches to one of the alist,
otherwise fallback to `atomic-chrome-default-major-mode'"
  (funcall (or (and url (assoc-default url
                                       atomic-chrome-url-major-mode-alist
                                       'string-match))
               atomic-chrome-default-major-mode)))

(defun atomic-chrome-show-edit-buffer (buffer title)
  "Show editing buffer BUFFER by creating a frame with title TITLE, \
or raising the selected frame depending on `atomic-chrome-buffer-open-style'."
  (let ((edit-frame nil)
        (frame-params (list (cons 'name (format "Atomic Chrome: %s" title))
                            (cons 'width atomic-chrome-buffer-frame-width)
                            (cons 'height atomic-chrome-buffer-frame-height))))
    (when (eq atomic-chrome-buffer-open-style 'frame)
      (setq edit-frame
            (if (memq window-system '(ns mac))
                ;; Avoid using make-frame-on-display for Mac OS.
                (make-frame frame-params)
              (make-frame-on-display
               (if (eq system-type 'windows-nt) "w32" (getenv "DISPLAY"))
               frame-params)))
      (select-frame edit-frame))
    (if (eq atomic-chrome-buffer-open-style 'split)
        (pop-to-buffer buffer)
      (switch-to-buffer buffer))
    (raise-frame edit-frame)
    (select-frame-set-input-focus (window-frame (selected-window)))
    edit-frame))

(defun atomic-chrome-create-buffer (socket url title text)
  "Create buffer associated with websocket specified by SOCKET.
URL is used to determine the major mode of the buffer created,
TITLE is used for the buffer name and TEXT is inserted to the buffer."
  (let ((buffer (generate-new-buffer (if (string-empty-p title) "No title" title))))
    (with-current-buffer buffer
      (puthash buffer
             (list socket (atomic-chrome-show-edit-buffer buffer title))
             atomic-chrome-buffer-table)
      (atomic-chrome-set-major-mode url)
      (insert text))))

(defun atomic-chrome-close-edit-buffer (buffer)
  "Close buffer BUFFER if it's one of Atomic Chrome edit buffers."
  (let ((frame (atomic-chrome-get-frame buffer))
        (window (get-buffer-window buffer)))
    (with-current-buffer buffer
      (save-restriction
        (run-hooks 'atomic-chrome-edit-done-hook)
        (when frame (delete-frame frame))
        (if (and (eq atomic-chrome-buffer-open-style 'split)
                 window)
            (quit-window t window)
          (kill-buffer buffer))))))

(defun atomic-chrome-close-current-buffer ()
  "Close current buffer and connection from client."
  (interactive)
  (when (or (not (buffer-modified-p))
	    (yes-or-no-p "Buffer has not been saved, close anyway? "))
    (atomic-chrome-close-edit-buffer (current-buffer))))

(defun atomic-chrome-update-buffer (socket text)
  "Update text on buffer associated with SOCKET to TEXT."
  (let ((buffer (atomic-chrome-get-buffer-by-socket socket)))
    (when buffer
      (with-current-buffer buffer
        (erase-buffer)
        (insert text)))))

(defun atomic-chrome-on-message (socket frame)
  "Function to handle data received from websocket client specified by SOCKET, \
where FRAME show raw data received."
  (let ((msg (json-read-from-string
              (decode-coding-string
               (encode-coding-string (websocket-frame-payload frame) 'utf-8)
	       'utf-8))))
    (let-alist msg
      (if (eq (websocket-server-conn socket) atomic-chrome-server-ghost-text)
          (if (atomic-chrome-get-buffer-by-socket socket)
              (atomic-chrome-update-buffer socket .text)
            (atomic-chrome-create-buffer socket .url .title .text))
        (cond ((string= .type "register")
               (atomic-chrome-create-buffer socket .payload.url .payload.title .payload.text))
              ((string= .type "updateText")
               (when atomic-chrome-enable-bidirectional-edit
                 (atomic-chrome-update-buffer socket .payload.text))))))))

(defun atomic-chrome-on-close (socket)
  "Function to handle request from client to close websocket SOCKET."
  (let ((buffer (atomic-chrome-get-buffer-by-socket socket)))
    (when buffer (atomic-chrome-close-edit-buffer buffer))))

(defvar atomic-chrome-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-s") 'atomic-chrome-send-buffer-text)
    (define-key map (kbd "C-c C-c") 'atomic-chrome-close-current-buffer)
    map)
  "Keymap for minor mode `atomic-chrome-edit-mode'.")

(define-minor-mode atomic-chrome-edit-mode
  "Minor mode enabled on buffers opened by Emacs Atomic Chrome server."
  :group 'atomic-chrome
  :lighter " AtomicChrome"
  :init-value nil
  :keymap atomic-chrome-edit-mode-map
  (when atomic-chrome-edit-mode
    (add-hook 'kill-buffer-hook 'atomic-chrome-close-connection nil t)
    (when atomic-chrome-enable-auto-update
      (add-hook 'post-command-hook 'atomic-chrome-send-buffer-text nil t))))

(defun atomic-chrome-turn-on-edit-mode ()
  "Turn on `atomic-chrome-edit-mode' if the buffer is an editing buffer."
  (when (gethash (current-buffer) atomic-chrome-buffer-table)
    (atomic-chrome-edit-mode t)))

(define-global-minor-mode global-atomic-chrome-edit-mode
  atomic-chrome-edit-mode atomic-chrome-turn-on-edit-mode)

(defun atomic-chrome-start-websocket-server (port)
  "Create websocket server on port PORT."
  (websocket-server
   port
   :host 'local
   :on-message #'atomic-chrome-on-message
   :on-open nil
   :on-close #'atomic-chrome-on-close))

(defun atomic-chrome-start-httpd ()
  "Start the HTTP server for Ghost Text query."
  (interactive)
  (make-network-process
   :name "atomic-chrome-httpd"
   :family 'ipv4
   :host 'local
   :service atomic-chrome-server-ghost-text-port
   :filter 'atomic-chrome-httpd-process-filter
   :filter-multibyte nil
   :server t
   :noquery t))

(defun atomic-chrome-normalize-header (header)
  "Destructively capitalize the components of HEADER."
  (mapconcat #'capitalize (split-string header "-") "-"))

(defun atomic-chrome-httpd-parse-string (string)
  "Parse client http header STRING into alist."
  (let* ((lines (split-string string "[\n\r]+"))
         (req (list (split-string (car lines))))
         (post (cadr (split-string string "\r\n\r\n"))))
    (dolist (line (butlast (cdr lines)))
      (push (list (atomic-chrome-normalize-header (car (split-string line ": ")))
                  (mapconcat #'identity
                             (cdr (split-string line ": ")) ": "))
            req))
    (push (list "Content" post) req)
    (reverse req)))

(defun atomic-chrome-httpd-process-filter (proc string)
  "Process filter of PROC which run each time client make a request.
STRING is the string process received."
  (setf string (concat (process-get proc :previous-string) string))
  (let* ((request (atomic-chrome-httpd-parse-string string))
         (content-length (cadr (assoc "Content-Length" request)))
         (uri (cl-cadar request))
         (content (cadr (assoc "Content" request))))
    (if (and content-length
             (< (string-bytes content) (string-to-number content-length)))
        (process-put proc :previous-string string)
      (atomic-chrome-httpd-send-response proc))))

(defun atomic-chrome-httpd-send-response (proc)
  "Send an HTTP 200 OK response back to process PROC."
  (when (processp proc)
    (unless atomic-chrome-server-ghost-text
      (setq atomic-chrome-server-ghost-text
            (atomic-chrome-start-websocket-server 64293)))
    (let ((header "HTTP/1.0 200 OK\nContent-Type: application/json\n")
          (body (json-encode '(:ProtocolVersion 1 :WebSocketPort 64293))))
      (process-send-string proc (concat header "\n" body))
      (process-send-eof proc))))

;;;###autoload
(defun atomic-chrome-start-server ()
  "Start websocket server for atomic-chrome.  Fails silently if a \
server is already running."
  (interactive)
  (ignore-errors
      (progn
        (and (not atomic-chrome-server-atomic-chrome)
             (memq 'atomic-chrome atomic-chrome-extension-type-list)
             (setq atomic-chrome-server-atomic-chrome
                   (atomic-chrome-start-websocket-server 64292)))
        (and (not (process-status "atomic-chrome-httpd"))
             (memq 'ghost-text atomic-chrome-extension-type-list)
             (atomic-chrome-start-httpd))
        (global-atomic-chrome-edit-mode 1))))

;;;###autoload
(defun atomic-chrome-stop-server nil
  "Stop websocket server for atomic-chrome."
  (interactive)
  (when atomic-chrome-server-atomic-chrome
    (websocket-server-close atomic-chrome-server-atomic-chrome)
    (setq atomic-chrome-server-atomic-chrome nil))
  (when atomic-chrome-server-ghost-text
    (websocket-server-close atomic-chrome-server-ghost-text)
    (setq atomic-chrome-server-ghost-text nil))
  (when (process-status "atomic-chrome-httpd")
    (delete-process "atomic-chrome-httpd"))
  (global-atomic-chrome-edit-mode 0))

(provide 'atomic-chrome)

;;; atomic-chrome.el ends here
