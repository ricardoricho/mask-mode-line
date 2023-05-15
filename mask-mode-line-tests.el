;;; mask-mode-line-tests.el -- Test for mask-mode-line
;;; Commentary:
;;; Code:

(load-file "mask-mode-line.el")

(declare-function mask-mode-line-mode "mask-mode-line.el")
(defvar mask-mode-line-mode nil "Check for mode activation.")

(ert-deftest mask-mode-line--activate ()
  "Activate mask."
  (mask-mode-line-mode)
  (should mask-mode-line-mode))

(ert-deftest mask-mode-line--keep-format ()
  "Ensureg mask mode line preserve format."
  (let ((original-mode-line mode-line-format))
    (mask-mode-line-mode 1)
    (mask-mode-line-mode -1)
    (should (eq original-mode-line
                mode-line-format))))

(provide 'mask-mode-line-tests)
;;; mask-mode-line-tests.el ends here.
