# Atomic Chrome for Emacs [![MELPA](http://melpa.org/packages/atomic-chrome-badge.svg)](http://melpa.org/#/atomic-chrome) [![MELPA Stable](http://stable.melpa.org/packages/atomic-chrome-badge.svg)](http://stable.melpa.org/#/atomic-chrome)

This is the Emacs version of [Atomic Chrome](https://github.com/tuvistavie/atomic-chrome) which is an extension for Google Chrome browser that allows you to edit text areas of the browser in Emacs. It's similar to [Edit with Emacs](https://github.com/stsquad/emacs_chrome), but has some advantages as below with the help of websocket.

* The input on Emacs is reflected to the browser instantly and continuously.
* You can use both the browser and Emacs at the same time. They are updated to the same content bi-directionally.

__Since v2.0.0, Atomic Chrome for Emacs supports [Ghost Text](https://github.com/GhostText/GhostText) as browser extension, bringing compatibility with Firefox, too.__

## Screencast

![Screencast](https://github.com/alpha22jp/atomic-chrome/blob/master/images/screencast.gif)

## Requirements

* Emacs: 24.4 or later
* OS: Tested on Windows and Linux, reported to work on macOS

## Installation

### For Chrome

Install the [GhostText](https://chrome.google.com/webstore/detail/ghosttext/godiecgffnchndlihlpaajjcplehddca) Chrome extension.

### For Firefox

Install the [GhostText](https://addons.mozilla.org/en-US/firefox/addon/ghosttext/) Firefox extension.

### For Emacs

Atomic Chrome for Emacs can be installed via MELPA as below.

<kbd>M-x package-install [RET] atomic-chrome [RET]</kbd>

Then add the following lines to your `.emacs`.

``` emacs-lisp
(require 'atomic-chrome)
(atomic-chrome-start-server)
```

## Usage

1. Start Emacs.
2. Focus the text area you want to edit on the browser.
3. Press Atomic Chrome button on the tool bar.
4. Contet of the text area is opened in a new buffer of Emacs.
5. Edit content on Emacs buffer.
6. <kbd>C-c C-c</kbd> to finish editing, or the buffer killed if the browser closes the connection.

## Customization

### Set auto-update mode

Atomic Chrome for Emacs automatically reflects modifications to the browser by default as described above, but you can disable it by setting the variable below.

``` emacs-lisp
(setq atomic-chrome-enable-auto-update nil)
```

In this case, you can apply the modifications to the browser with <kbd>C-c C-s</kbd>

### Set major mode for editing buffer

The default major mode of editing buffer is `text-mode`. You can change the major mode manually. If you want to use a different major mode as the default, set `atomic-chrome-default-major-mode` like below.

``` emacs-lisp
(setq atomic-chrome-default-major-mode 'markdown-mode)
```

Additionally, you can use `atomic-chrome-url-major-mode-alist` to choose the major mode for a specific website based on the page URL (or, with GhostText, URL hostname) like below.

``` emacs-lisp
(setq atomic-chrome-url-major-mode-alist
      '(("github\\.com" . gfm-mode)
        ("redmine" . textile-mode)))
```

This is an association list of regexp and major mode function. If the page URL matches to on of the regexp, the corresponding major mode is selected.

### Select editing buffer style

You can select the style of opening the editing buffer by `atomic-chrome-buffer-open-style` like below.

``` emacs-lisp
(setq atomic-chrome-buffer-open-style 'frame)
```

Available values are the followings.

* `full`: Open in the selected window.
* `split`: Open in the new window by splitting the selected window (default).
* `frame`: Create a new frame and window in it.

`frame` is available only when you are using Emacs on some window system.

If you select `frame`, you can also set the width and height of the frame with `atomic-chrome-buffer-frame-width` and `atomic-chrome-buffer-frame-height`. Their default values are `80` and `25`.

### Select available browser extension

By default, Atomic Chrome for Emacs accepts the connection request from both Atomic Chrome and Ghost Text. If you never use one of them, you can disable it by setting `atomic-chrome-extension-type-list` like below.

``` emacs-lisp
(setq atomic-chrome-extension-type-list '(atomic-chrome))
```

### Set Server Port for Ghost Text

By default, Atomic Chrome for Emacs uses the default port 4001 for Ghost Text. If you need to set this to another port, you can do it by setting `atomic-chrome-server-ghost-text-port` like below.

``` emacs-lisp
(setq atomic-chrome-server-ghost-text-port 4002)
```

You may also need to update Ghost Text's port setting in Chrome Extensions page.

## History

version 2.0.0 (2016-11-08)

* Add support for Ghost Text

version 1.0.1 (2016-11-06)

* Fix updating text from Chrome doesn't work

version 1.0.0 (2016-11-05)

* Add support for automatic window focus (#6)
* Fix major mode issue (#1, #7)

version 0.1.0 (2016-10-23)

* Initial version
