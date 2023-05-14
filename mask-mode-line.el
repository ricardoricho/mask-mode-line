;;; hide-mode-line.el --- minor mode that hides/masks your modeline -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018-2021 Henrik Lissner
;;
;; Author: Ricardo Arredondo <http://github/ricardoricho>
;; Maintainer: Ricardo Arredondo <ricardo.richo@gmail.com>
;; Created: May 14, 2023
;; Version: 0.0.1
;; Keywords: frames mode-line mask
;; URL: https://github.com/ricardoricho/mask-mode-line
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Based on https://github.com/hlissner/emacs-hide-mode-line
;;
;;; Code:

(defcustom mask-mode-line-format ""
  "The modeline format to use when `mask-mode-line-mode' is active."
  :type 'string
  :group 'mask-mode-line)

(defcustom mask-mode-line-face
  (mask-mode-line--guess-face)
  "Remap to this face when `mask-mode-line-mode' is active."
  :type 'list
  :group 'mask-mode-line)

(defun mask-mode-line--guess-face ()
  "Set default face to hide mode-line."
  (let ((background (face-background 'default))
        (foreground (face-foreground 'default)))
     (list :box nil :foreground foreground
           :background background :height 0.5)))

(defvar-local mask-mode-line--cookies nil
  "Storage for cookies when remaping mode-line and mode-line-inactive faces.")

(defcustom mask-mode-line-excluded-modes '(fundamental-mode)
  "List of major modes where `global-mask-mode-line-mode' won't affect."
  :type 'list
  :group 'mask-mode-line)

(defvar-local mask-mode-line--old-format nil
  "Storage for the old `mode-line-format', so it can be restored when
`mask-mode-line-mode' is disabled.")

(defun mask-mode-line--mask-mode-line ()
  "Mask mode-line.
Use `mask-mode-line-format' and `mask-mode-line-face'."
  (setq-local mask-mode-line--old-format mode-line-format)
  (setq-local mask-mode-line--cookies
              (list (face-remap-add-relative 'mode-line mask-mode-line-face)
                    (face-remap-add-relative 'mode-line-inactive mask-mode-line-face)))
  (setq mode-line-format mask-mode-line-format)
  (force-mode-line-update))

(defun mask-mode-line--unmask-mode-line ()
  "Unmask mode-line.  Revert to original mode-line."
  (setq mode-line-format mask-mode-line--old-format)
  (mapc 'face-remap-remove-relative mask-mode-line--cookies)
  (setq-local mask-mode-line--old-format nil)
  (force-mode-line-update)
  (unless mask-mode-line-format (redraw-display)))

;;;###autoload
(define-minor-mode mask-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  (if mask-mode-line-mode

      ;; Do not overwrite original mode line
      (unless mask-mode-line--old-format
        (add-hook 'after-change-major-mode-hook
                  #'mask-mode-line-reset nil t)
        (mask-mode-line--mask-mode-line))
    ;; else
    ;; check old-format to prevent setting mode-line-format to nil
    (when mask-mode-line--old-format
      (remove-hook 'after-change-major-mode-hook
                   #'mask-mode-line-reset t)
      (mask-mode-line--unmask-mode-line))))


;; Ensure major-mode or theme changes don't overwrite these variables
(put 'mask-mode-line--old-format 'permanent-local t)
(put 'mask-mode-line-mode 'permanent-local-hook t)

;;;###autoload
(define-globalized-minor-mode global-mask-mode-line-mode
  mask-mode-line-mode turn-on-mask-mode-line-mode
  (redraw-display))

;;;###autoload
(defun turn-on-mask-mode-line-mode ()
  "Turn on `mask-mode-line-mode'.
Unless in `fundamental-mode' or `mask-mode-line-excluded-modes'."
  (unless (memq major-mode mask-mode-line-excluded-modes)
    (mask-mode-line-mode +1)))

(provide 'mask-mode-line)
;;; mask-mode-line.el ends here
