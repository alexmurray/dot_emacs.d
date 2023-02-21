;;; apm-misc --- Miscellaneous functions

;;; Commentary:

;;; Code:

(defun apm-browse-lp-bug-at-point ()
  "Browse to the launchpad bug referenced by bug number at point."
  (interactive)
  (let ((base-uri "https://launchpad.net/bugs/")
        (id (substring-no-properties (thing-at-point 'symbol))))
    (when id
      (browse-url (concat base-uri id)))))

(defun apm-convert-cpuid-to-mcu (cpuid)
  "Convert CPUID to the equivalent MCU file name."
  (when (string-prefix-p "0x" cpuid)
    (setq cpuid (substring cpuid 2)))
  (let* ((id (string-to-number cpuid 16))
         (model (logior (logand (ash id -12) 240) (logand (ash id -4) 15)))
         (family (logand (ash id -8) 15))
         (stepping (logand id 15)))
    (format "%02x-%02x-%02x" family model stepping)))

(defun apm-convert-mcu-to-cpuid (mcu)
  "Convert MCU file name to the equivalent cpuid."
  (let* ((parts (split-string mcu "-"))
         (family (string-to-number (nth 0 parts) 16))
         (model (string-to-number (nth 1 parts) 16))
         (stepping (string-to-number (nth 2 parts) 16))
         (cpuid (logior (ash (logand model 240) 12)
                        (ash (logand family 15) 8)
                        (ash (logand model 15) 4)
                        (logand stepping 15))))
    (format "0x%08x" cpuid)))

(defun apm-convert-mcu-cpuid-at-point ()
  "Convert the MCU/CPUID at point and return the result."
  (interactive)
  (let ((symbol (substring-no-properties (thing-at-point 'symbol))))
    (cond ((string-search "-" symbol)
                     ;; might be a filename so ignore directories part
                     (apm-convert-mcu-to-cpuid (file-name-base symbol)))
                    (t
                     (apm-convert-cpuid-to-mcu symbol)))))

(defun apm-convert-mcu-cpuid-at-point-and-kill-ring-save ()
  (interactive)
  (kill-new (apm-convert-mcu-cpuid-at-point)))

(provide 'apm-misc)
;;; apm-misc.el ends here
