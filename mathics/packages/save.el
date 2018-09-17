(start-process-shell-command
 "mathics-process"
 "mathics-buffer"
 "source activate ~/anaconda3/envs/mathics-test && mathics")

(defvar ob-mathics-latest-output "")

;; (defun report ()
;;   (with-current-buffer "mathics-buffer"
;;     `((:point-min     ,(point-min))
;;       (:point         ,(point))
;;       (:point-max     ,(point-max))
;;       (:process-mark  ,(process-mark (get-process "mathics-process")))
;;       (:buffer-data   ,ob-mathics-buffer-data)
;;       (:latest-output ,ob-mathics-latest-output))))

(defun extracting-insertion-filter (proc string)
  (setq ob-mathics-latest-output "")
  (let ((pb (process-buffer proc)))
    (when (buffer-live-p pb)
      (with-current-buffer pb
        (save-excursion
          (goto-char (process-mark proc))
          (insert string) ; advances point
          (set-marker (process-mark proc) (point))))
      (goto-char (process-mark proc))
      (mapcar (lambda (w) (set-window-point w (process-mark proc)))
              (get-buffer-window-list pb)))
    (setq ob-mathics-latest-output string)))

(set-process-filter
 (get-process "mathics-process")
 #'extracting-insertion-filter)

(process-send-string "mathics-process" "420 / 3\n")

ob-mathics-latest-output"Out[4]= 42

In[5]:= "

(string-match ("Out\[[0-9]+\]= ") ob-mathics-latest-output)

(pp (match-data))

(type-of ob-mathics-latest-output)

(match-string-no-properties 0 ob-mathics-latest-output)

(match-end 0)1318
(string-match "\n\nIn\[[0-9]+\]:= " ob-mathics-latest-output)10
(match-end 0)1400
(length ob-mathics-latest-output)20

(process-send-string "mathics-process" "42 / 3\n")

(process-send-string "mathics-process" "42 ^ 2\n")

(process-send-string "mathics-process" "Sqrt[42^2]\n")
