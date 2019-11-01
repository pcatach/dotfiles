;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(defun display-fonts ()
  "Display a list of all monospace font faces."
  (interactive)
  (pop-to-buffer "*Monospace Fonts*")

  (erase-buffer)
  (dolist (font-family (font-family-list))
    (let ((str font-family))
      (newline)
      (insert
       (propertize (concat "The quick brown fox jumps over the lazy dog 1 l; 0 O o ("
    			   font-family ")\n") 'face `((:family ,font-family)))))))

;; Khmer OS System
;; Chandas
;; Dejavu sans mono
;; ubuntu, ubuntu mono
(font-family-list)
