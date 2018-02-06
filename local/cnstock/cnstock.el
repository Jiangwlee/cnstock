;;; cnstock.el --- An Emacs Lisp extension to demonstrate China stock information

;; Copyright (C) 2018-2028 Bruce.Li

;; Author: Bruce.Li <jiangwlee@163.com>
;; Maintainer: Bruce.Li <jiangwlee@163.com>
;; Version: 0.0.1
;; Created: 30 Jan 2018
;; Keywords: languages
;; Homepage: https://www.github.com/jiangwlee

;; This program is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;
;; Constants
;;
(defconst cnstock-buffer-name " *CnStock*")
(defconst cnstock-global--url "http://hq.sinajs.cn/?format=text&list=")

;;
;; Variables
;;
(defvar cnstock-global--window nil)
(defvar cnstock-global--buffer nil)
(defvar cnstock-global--current-quotation-string nil)
(defvar cnstock-global--current-quotation-list ())

(defvar cnstock-quotation--update-timer nil)
(defvar cnstock-quotation--display-timer nil)

(defvar tabulated-list-format nil "Buffer local variable for tabulated list")
(defvar tabulated-list-entries nil "Buffer local variable for tabulated list")
(defvar tabulated-list-sort-key nil "Buffer local variable for tabulated list")
(declare-function tabulated-list-init-header "tabulated-list" ())
(declare-function tabulated-list-print "tabulated-list"
                  (&optional remember-pos update))

;;
;; Customization
;;

(defgroup cnstock nil
  "Options for cnstock."
  :prefix "cnstock-")

(defcustom cnstock-window-fixed-size t
  "*If the cnstock windows is fixed, it won't be resize when rebalance windows."
  :type 'boolean
  :group 'cnstock)

(defcustom cnstock-tabulated-mode-enabled t
  "*If tabulated mode is enabled, the stock quotation will be displayed in a tabulated list"
  :type 'boolean
  :group 'cnstock)

(defcustom cnstock-stock-code-list "sh000001,sz399106,sz399006,sz000001,sz000002,sz300059,sh601939"
  "*Stock code list."
  :type '(string)
  :group 'cnstock)


;;
;; Macros
;;
(defmacro cnstock-util--to-bool (obj)
  "If OBJ is non-nil, return t, else return nil."
  `(and ,obj t))

(defmacro cnstock-global--with-buffer (&rest body)
  "Execute the forms in BODY with global CnStock buffer."
  (declare (indent 0) (debug t))
  `(let ((cnstock-buffer (cnstock-global--get-buffer)))
     (unless (null cnstock-buffer)
       (with-current-buffer cnstock-buffer
         ,@body))))

(defmacro cnstock-global--with-window (&rest body)
  "Execute the forms in BODY with global CnStock window."
  (declare (indent 0) (debug t))
  `(save-selected-window
     (cnstock-global--select-window)
     ,@body))

(defmacro cnstock-global--when-window (&rest body)
  "Execute the forms in BODY when selected window is CnStock window."
  (declare (indent 0) (debug t))
  `(when (eq (selected-window) cnstock-global--window)
     ,@body))

(defmacro cnstock-global--switch-to-buffer ()
  "Switch to CnStock buffer."
  `(let ((cnstock-buffer (cnstock-global--get-buffer)))
     (unless (null cnstock-buffer)
       (switch-to-buffer cnstock-buffer))))

(defmacro cnstock-buffer--with-editing-buffer (&rest body)
  "Execute BODY in CnStock buffer without read-only restriction."
  `(let (rlt)
     (cnstock-global--with-buffer
      (setq buffer-read-only nil)
      (setq rlt (progn ,@body))
      (setq buffer-read-only t))
     rlt))

(defmacro cnstock-buffer--with-resizable-window (&rest body)
  "Execute BODY in cnstock window without `window-size-fixed' restriction."
  `(let (ret)
     (cnstock-global--with-buffer
      (cnstock-buffer--unlock-width))
     (setq rlt (progn ,@body))
     (cnstock-global--with-buffer
      (cnstock-buffer--lock-width))
     rlt))

;;
;; Major mode definitions
;;

(defvar cnstock-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'cnstock-hide)
    map)
  "Keymap for `cnstock-mode'.")

(define-derived-mode cnstock-mode special-mode "CnStock"
  "A major mode for displaying the China stock quotation")

(define-derived-mode cnstock-tabulated-mode tabulated-list-mode "CnStock Tabulated"
  "Major mode for listing the CnStock quotation."
  (setq tabulated-list-format [("Code" 8 t)
                               ("Name" 8 t)
                               ("Open" 8 t)
                               ("Close" 8 t)
                               ("Now" 8 t)
                               ("Percentage" 8 cnstock-quotation--compare-float-entries)])
  (setq tabulated-list-sort-key (cons "Percentage" nil))
  (tabulated-list-init-header))

;;
;; Global methods
;;

(defun cnstock-global--get-url ()
  "Return the url to retrive stock quotation."
  (interactive)
  (concat cnstock-global--url cnstock-stock-code-list))

(defun cnstock-global--window-exists-p ()
  "Return non-nil if cnstock window exists."
  (and (not (null (window-buffer cnstock-global--window)))
       (eql (window-buffer cnstock-global--window) cnstock-global--buffer)))

(defun cnstock-global--select-window ()
  "Select the CnStock window."
  (interactive)
  (let ((window (cnstock-global--get-window)))
    (select-window window)))

(defun cnstock-global--get-window ()
  (unless (cnstock-global--window-exists-p)
    (setf cnstock-global--window nil))
  (setq cnstock-global--window
        (cnstock-global--create-window))
  cnstock-global--window)

(defun cnstock-global--get-buffer ()
  "Return the global cnstock buffer if it exists.
If the global cnstock buffer not exists, then create it"
  (unless (equal (buffer-name cnstock-global--buffer)
                 cnstock-buffer-name)
    (setf cnstock-global--buffer nil))
  (when (null cnstock-global--buffer)
    (save-window-excursion
      (setq cnstock-global--buffer
            (cnstock-buffer--create))))
  cnstock-global--buffer)

(defun cnstock-display-action (buffer _alist)
  "Display BUFFER to the right of the root window.
The root window is the root window of the selected frame.
_ALIST is ignored."
  (let ((window-pos 'right))
    (display-buffer-in-side-window buffer `((side . ,window-pos)))))

(defun cnstock-global--create-window ()
  "Create global cnstock window."
  (let ((window nil)
        (buffer (cnstock-global--get-buffer)))
    (setq window
          (select-window
           (display-buffer buffer '(cnstock-display-action))))
    (cnstock-window--init window buffer)
    window))

(defun cnstock-buffer--create ()
  "Create and switch to cnstock buffer."
  (switch-to-buffer
   (generate-new-buffer-name cnstock-buffer-name))
  (if cnstock-tabulated-mode-enabled (cnstock-tabulated-mode)
    (cnstock-mode))
  (current-buffer))

(defun cnstock-buffer--lock-width ()
  "Lock the width size for CnStock window."
  (if cnstock-window-fixed-size
      (setq window-size-fixed 'width)))

(defun cnstock-buffer--unlock-width ()
  "Unlock the width size for CnStock window."
  (setq window-size-fixed nil))

;;
;; Window methods
;;

(defun cnstock-window--init (window buffer)
  "Make WINDOW a CnStock window.
CnStock buffer is BUFFER"
  (cnstock-buffer--with-resizable-window
   (switch-to-buffer buffer)
   (set-window-dedicated-p window t))
  window)

;;
;; Stock quotation methods
;;

(defun cnstock-quotation--parse (response)
  "Parse stock quotation from Sina."
  (let ((rlt nil)
        (tmp nil)
        (current-quotation-list ())
        (quotation-list (split-string response "\n")))
    (dolist (element quotation-list rlt)
      (if (not (string-match "^[[:blank:]]*$" element))
          (progn
            (setq tmp (cnstock-quotation--parse-stock element))
            (setq rlt (concat rlt (cnstock-quotation--format-to-string tmp)))
            (setq current-quotation-list (cons tmp current-quotation-list)))))
    (setq cnstock-global--current-quotation-string rlt)
    (setq cnstock-global--current-quotation-list (reverse current-quotation-list))))

(defun cnstock-quotation--parse-stock (data)
  "Parse one stock's quotation data and format to a string"
  (let* (rlt
         (data-list (split-string (decode-coding-string data 'gbk) ","))
         (nth-float-value-from-datalist
          (lambda (idx) (string-to-number (nth idx data-list))))
         (id-name-str (split-string (car data-list) "="))
         (id (car id-name-str))
         (name (nth 1 id-name-str))
         (open (funcall nth-float-value-from-datalist 1))
         (close (funcall nth-float-value-from-datalist 2))
         (now (funcall nth-float-value-from-datalist 3))
         (high (funcall nth-float-value-from-datalist 4))
         (low (funcall nth-float-value-from-datalist 5))
         (percentage (* 100 (/ (- now close) close)))
         )
    (list id name open close now percentage)))
    ;;(format "%s %-8s %-6.3f %-6.3f %-6.3f %6.3f%%\n"
     ;;       id name open close now percentage)))

(defun cnstock-quotation--format-to-string (data)
  "Format one stock's quotation data to string."
  (format "%s %-8s %-8.3f %5.3f%%\n"
          (nth 0 data)
          (nth 1 data)
          (nth 4 data)
          (nth 5 data)
          ))
  ;;(format "%s %-8s %-7.3f %-7.3f %-7.3f %7.3f%%\n"
  ;;        (pop data)
  ;;        (pop data)
  ;;        (pop data)
  ;;        (pop data)
  ;;        (pop data)
  ;;        (pop data)))

(defun cnstock-quotation--url-retrive ()
  "Retrive stock quotation from Sina."
  (with-current-buffer
      (url-retrieve-synchronously (cnstock-global--get-url))
    (progn
      (goto-char (point-min))
      (re-search-forward "[\n\t\r]\\{2,\\}")
      (delete-region (point) (point-min))
      (cnstock-quotation--parse (buffer-string))
      (kill-buffer))))

(defun cnstock-quotation--display ()
  "Update stock quotation in cnstock-global--buffer."
  (if cnstock-global--current-quotation-string
      (cnstock-buffer--with-editing-buffer
       (if cnstock-tabulated-mode-enabled (cnstock-quotation--tabulated-display)
         (cnstock-quotation--normal-display)))))

(defun cnstock-quotation--normal-display ()
  "Dispaly CnStock quotation in cnstock-mode."
  (erase-buffer)
  (insert (format "%s" cnstock-global--current-quotation-string)))

(defun cnstock-quotation--tabulated-display ()
  "Display CnStock quotation in cnstock-tabulated-mode."
  (setq tabulated-list-entries nil)
  (dolist (element cnstock-global--current-quotation-list)
    (setq element (mapcar
                   (lambda (arg)
                     (if (floatp arg) (format "%-8.3f" arg) (format "%-8s" arg)))
                   element))
    (push (list element (vconcat element nil)) tabulated-list-entries))
  (tabulated-list-print))

(defun cnstock-quotation--compare-float-entries (arg1 arg2)
  "Compare two tabulated entries whoes value is a float string."
  (let ((fval1 (string-to-number (elt (nth -1 arg1) 5)))
        (fval2 (string-to-number (elt (nth -1 arg2) 5))))
    (> fval1 fval2)))
;;
;; Provided commands
;;

(defun cnstock-start-timers ()
  (setq cnstock-global--current-quotation-string nil)
  (when cnstock-quotation--update-timer (cancel-timer cnstock-quotation--update-timer))
  (when cnstock-quotation--display-timer (cancel-timer cnstock-quotation--display-timer))
  (cnstock-quotation--url-retrive)
  (cnstock-quotation--display)
  (setq cnstock-quotation--update-timer
        (run-at-time nil 5 'cnstock-quotation--url-retrive))
  (setq cnstock-quotation--display-timer
        (run-at-time nil 5 'cnstock-quotation--display)))

(defun cnstock-stop-timers ()
  (when cnstock-quotation--update-timer (cancel-timer cnstock-quotation--update-timer))
  (when cnstock-quotation--display-timer (cancel-timer cnstock-quotation--display-timer))
  (setq cnstock-global--current-quotation-string nil
        cnstock-quotation--update-timer nil
        cnstock-quotation--display-timer nil))

;;;###autoload
(defun cnstock-toggle ()
  "Toggle show the CnStock window."
  (interactive)
  (if (cnstock-global--window-exists-p)
      (cnstock-hide)
    (cnstock-show)))

;;;###autoload
(defun cnstock-show ()
  "Show the CnStock window."
  (interactive)
  (cnstock-global--select-window)
  (cnstock-start-timers))

;;;###autoload
(defun cnstock-hide ()
  "CLose the CnStock window."
  (interactive)
  (cnstock-stop-timers)
  (if (cnstock-global--window-exists-p)
      (delete-window cnstock-global--window)))

(provide `cnstock)

;;; {cnstock.el} ends here

