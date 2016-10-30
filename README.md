# Atomic Chrome for Emacs [![MELPA](http://melpa.org/packages/atomic-chrome-badge.svg)](http://melpa.org/#/atomic-chrome)

This is the Emacs version of [Atomic Chrome](https://atom.io/packages/atomic-chrome) which is an extension for Google Chrome browser that allows you to edit text areas of the browser in Emacs. It's similar to [Edit with Emacs](https://www.emacswiki.org/emacs/Edit_with_Emacs), but has some advantages as below with the help of websocket.

* The input on Emacs is reflected to the browser instantly and continuously.
* You can use both the browser and Emacs at the same time. They are updated to the same content bi-directionally.

## Screencast

![Screencast](https://github.com/alpha22jp/atomic-chrome/blob/master/images/screencast.gif)

## Requirements

* Emacs: 24.3 or later
* OS: Tested on Windows and Linux

## Installation

### For Chrome

Atomic Chrome for Emacs shares [Atomic Chrome](https://atom.io/packages/atomic-chrome) as an extension on Chrome. Just install and setup it.

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
6. <kbd>C-c C-c</kbd> or `M-x kill-buffer` to finish editing, or the buffer automatically killed if the browser closes the connection.

## Customization

### Set major mode for editing buffer

The default major mode of editing buffer is `text-mode`. If you want to use a different major mode, set `atomic-chrome-default-major-mode` like below.

``` emacs-lisp
(setq atomic-chrome-default-major-mode 'markdown-mode)
```

Additionally, you can use `atomic-chrome-url-major-mode-alist` to choose the major mode for a specific website based on the page URL like below.

``` emacs-lisp
(setq atomic-chrome-url-major-mode-alist
      '(("github\\.com" . gfm-mode)
        ("redmine" . textile-mode)))
```

If the page URL matches the regexp specified, the corresponding major mode is selected.
