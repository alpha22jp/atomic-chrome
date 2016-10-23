;;; emacs-chrome.el --- Edit Chrome text area with Emacs using Atomic Chrome

;; Copyright (C) 2016 alpha22jp <alpha22jp@gmail.com>

;; Author: alpha22jp <alpha22jp@gmail.com>
;; Package-Requires: ((let-alist "1.0.4") (websocket "1.4"))
;; Keywords: chrome edit textarea
;; Version: 0.1.0

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

;;; Code:

(require 'json)
(require 'let-alist)
(require 'websocket)

(defcustom emacs-chrome-buffer-open-style 'split
  "Specify the style to open new buffer for editing."
  :type '(choice (const :tag "Open buffer with full window" full)
                 (const :tag "Open buffer with splitted window" split)
                 (const :tag "Open buffer with new frame" frame))
  :group 'emacs-chrome)

(defcustom emacs-chrome-enable-auto-update t
  "If non-nil, edit on Emacs is reflected to Chrome instantly, \
otherwise you need to type \"C-xC-s\" manually."
  :type 'boolean
  :group 'emacs-chrome)

(defcustom emacs-chrome-enable-bidirectional-edit t
  "If non-nil, you can edit both on Chrome text area and Emacs, \
otherwise edit on Chrome is ignored while editing on Emacs."
  :type 'boolean
  :group 'emacs-chrome)

(defcustom emacs-chrome-edit-mode-hook nil
  "Customizable hook which run when the editing buffer is created."
  :type 'hook
  :group 'emacs-chrome)

(defvar emacs-chrome-ws-conn-list (make-hash-table :test 'equal))

(defvar emacs-chrome-buffer-ws nil)
(make-variable-buffer-local 'emacs-chrome-buffer-ws)

(defun emacs-chrome-close-connection ()
  "Close client connection associated with current buffer."
  (when emacs-chrome-buffer-ws
    (remhash (websocket-conn emacs-chrome-buffer-ws) emacs-chrome-ws-conn-list)
    (websocket-close emacs-chrome-buffer-ws)))

(defun emacs-chrome-send-buffer-text ()
  "Send request to update text with current buffer content."
  (interactive)
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (when (and emacs-chrome-buffer-ws text)
      (websocket-send-text emacs-chrome-buffer-ws
                           (json-encode
                            (list '("type" . "updateText")
                                  (cons "payload" (list (cons "text" text)))))))))

(defun emacs-chrome-create-buffer (ws title text)
  "Create buffer associated with WS named TITLE, and insert TEXT to the buffer."
  (let ((buffer (generate-new-buffer title)))
    (with-current-buffer buffer
      (setq emacs-chrome-buffer-ws ws)
      (puthash (websocket-conn ws) (buffer-name) emacs-chrome-ws-conn-list)
      (emacs-chrome-edit-mode)
      (add-hook 'kill-buffer-hook 'emacs-chrome-close-connection nil t)
      (when emacs-chrome-enable-auto-update
        (add-hook 'post-command-hook 'emacs-chrome-send-buffer-text nil t))
      (insert text))
    (cond ((eq emacs-chrome-buffer-open-style 'full) (switch-to-buffer buffer))
          ((eq emacs-chrome-buffer-open-style 'split) (switch-to-buffer-other-window buffer))
          ((eq emacs-chrome-buffer-open-style 'frame) (switch-to-buffer-other-frame buffer)))))

(defun emacs-chrome-update-buffer (ws text)
  "Update text on buffer associated with WS to TEXT."
  (let* ((buffer-name (gethash (websocket-conn ws) emacs-chrome-ws-conn-list))
         (buffer (if buffer-name (get-buffer buffer-name) nil)))
    (when buffer
      (with-current-buffer buffer
        (erase-buffer)
        (insert text)))))

(defun emacs-chrome-on-message (ws frame)
  "Function to handle data received from websocket client specified by WS, \
where FRAME show raw data received."
  (let ((msg (json-read-from-string
              (decode-coding-string
               (string-make-unibyte (websocket-frame-payload frame)) 'utf-8))))
    (let-alist msg
      (cond ((string= .type "register")
             (emacs-chrome-create-buffer ws .payload.title .payload.text))
            ((string= .type "updateText")
             (when emacs-chrome-enable-bidirectional-edit
               (emacs-chrome-update-buffer ws .payload.text)))))))

(defun emacs-chrome-on-close (ws)
  "Function to handle request from client to close connection specified by WS."
  (let* ((ws-conn (websocket-conn ws))
         (buffer-name (gethash ws-conn emacs-chrome-ws-conn-list))
         (buffer (if buffer-name (get-buffer buffer-name) nil)))
    (when buffer
      (with-current-buffer buffer (setq emacs-chrome-buffer-ws nil))
      (kill-buffer buffer))
    (remhash ws-conn emacs-chrome-ws-conn-list)))

(defvar emacs-chrome-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-s") 'emacs-chrome-send-buffer-text)
    (define-key map (kbd "C-c C-c") 'kill-buffer)
    map)
  "Keymap for minor mode `emacs-chrome-edit-mode'.")

(define-minor-mode emacs-chrome-edit-mode
  "Minor mode enabled on buffers opened by Emacs Chrome server."
  :group 'emacs-chrome
  :lighter " EmacsChrome"
  :init-value nil
  :keymap emacs-chrome-edit-mode-map
  (when (and (numberp arg) (> arg 0))
    (run-hooks 'emacs-chrome-edit-mode-hook)))

(defvar emacs-chrome-server-conn
      (websocket-server
       64292
       :host 'local
       :on-message #'emacs-chrome-on-message
       :on-open nil
       :on-close #'emacs-chrome-on-close))

(defadvice save-buffers-kill-emacs
      (before emacs-chrome-server-stop-before-kill-emacs)
      "Call `websocket-server-close' before closing Emacs to avoid users \
being prompted to kill the websocket server process."
      (websocket-server-close emacs-chrome-server-conn))

(provide 'emacs-chrome)

;;; emacs-chrome.el ends here
