;;; dired-posframe.el --- Peep dired items using posframe  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1") (posframe "0.7"))
;; URL: https://github.com/conao3/dired-posframe.el

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

;;; Commentary:

;; Peep dired items using posframe.


;;; Code:

(require 'posframe)
(require 'dired)

(defgroup dired-posframe nil
  "Peep dired items using posframe."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/conao3/dired-posframe.el"))

(defcustom dired-posframe-file-size-limit (* 1 1024 1024)
  "File size limit in Bytes.
Large files are often binary files, resulting in slow performance.
The default value is 1MB."
  :group 'dired-posframe
  :type 'integer)

(defcustom dired-posframe-enable-modes '(image-mode)
  "Major mode automatically enabled.

The dired-posframe can enable major mode to displays the file with
the appropriate font lock and to display images as images rather
than characters.

You can enable it by listing `major-mode' in this variable."
  :group 'dired-posframe
  :type 'sexp)

(defcustom dired-posframe-style 'point
  "The style of dired-posframe."
  :group 'dired-posframe
  :type 'symbol)

(defcustom dired-posframe-font nil
  "The font used by dired-posframe.
When nil, Using current frame's font as fallback."
  :group 'dired-posframe
  :type '(choice (const :tag "inherit" nil)
                 string))

(defcustom dired-posframe-width nil
  "The width of dired-posframe."
  :group 'dired-posframe
  :type '(choice (const :tag "default" nil)
                 number))

(defcustom dired-posframe-height nil
  "The height of dired-posframe."
  :group 'dired-posframe
  :type '(choice (const :tag "default" nil)
                 number))

(defcustom dired-posframe-min-width nil
  "The width of dired-min-posframe."
  :group 'dired-posframe
  :type '(choice (const :tag "non-width" nil)
                 number))

(defcustom dired-posframe-min-height nil
  "The height of dired-min-posframe."
  :group 'dired-posframe
  :type '(choice (const :tag "non-width" nil)
                 number))

(defcustom dired-posframe-border-width 1
  "The border width used by dired-posframe.
When 0, no border is showed."
  :group 'dired-posframe
  :type '(choice (const :tag "non-width" nil)
                 number))

(defcustom dired-posframe-parameters nil
  "The frame parameters used by dired-posframe."
  :group 'dired-posframe
  :type '(choice (const :tag "no-parameters" nil)
                 number))

(defface dired-posframe
  '((t (:inherit default)))
  "Face used by the dired-posframe."
  :group 'dired-posframe)

(defface dired-posframe-border
  '((t (:inherit default :background "gray50")))
  "Face used by the dired-posframe's border."
  :group 'dired-posframe)


;;; Functions

(defvar dired-posframe-buffer " *dired-posframe-buffer*"
  "The posframe-buffer used by dired-posframe.")

(defvar dired-posframe--display-p nil
  "The status of `dired-posframe--display'.")

(defun dired-posframe--display (buf &optional poshandler)
  "Show BUF in posframe with POSHANDLER."
  (if (not (posframe-workable-p))
      (warn "`posframe' can not be workable in your environment")
    (posframe-show
     buf
     :font dired-posframe-font
     :position (point)
     :poshandler poshandler
     :background-color (face-attribute 'dired-posframe :background nil t)
     :foreground-color (face-attribute 'dired-posframe :foreground nil t)
     :internal-border-width dired-posframe-border-width
     :internal-border-color (face-attribute 'dired-posframe-border :background nil t)
     :override-parameters dired-posframe-parameters
     :height dired-posframe-height
     :width dired-posframe-width
     :min-height (or dired-posframe-min-height
                     ;; (let ((height (+ ivy-height 1)))
                     ;;   (min height (or dired-posframe-height height)))
                     )
     :min-width (or dired-posframe-min-width
                    ;; (let ((width (round (* (frame-width) 0.62))))
                    ;;   (min width (or dired-posframe-width width)))
                    ))))

(defun dired-posframe-display (buf)
  "Display BUF via `posframe' by `dired-posframe-style'."
  (let ((fn (intern (format "dired-posframe-display-at-%s" dired-posframe-style)))
        (defaultfn (intern (format "dired-posframe-display-at-%s" "point"))))
    (if (functionp fn)
        (funcall fn buf)
      (funcall defaultfn buf))))

(eval
 `(progn
    ,@(mapcar
       (lambda (elm)
         `(defun ,(intern (format "dired-posframe-display-at-%s" (car elm))) (buf)
            ,(format "Display BUF via `posframe' at %s" (car elm))
            (dired-posframe--display buf #',(intern (format "posframe-poshandler-%s" (cdr elm))))))
       '(;; (absolute-x-y             . absolute-x-y)
         (frame-bottom-center      . frame-bottom-center)
         (frame-bottom-left        . frame-bottom-left-corner)
         (frame-bottom-right       . frame-bottom-right-corner)
         (frame-center             . frame-center)
         (frame-top-center         . frame-top-center)
         (frame-top-left           . frame-top-left-corner)
         (frame-top-right          . frame-top-right-corner)
         (point-bottom-left        . point-bottom-left-corner)
         (point-bottom-left-upward . point-bottom-left-corner-upward)
         (point-top-left           . point-top-left-corner)
         (point                    . point-bottom-left-corner)
         (window-bottom-center     . window-bottom-center)
         (window-bottom-left       . window-bottom-left-corner)
         (window-bottom-right      . window-bottom-right-corner)
         (window-center            . window-center)
         (window-top-center        . window-top-center)
         (window-top-left          . window-top-left-corner)
         (window-top-right         . window-top-right-cornerl)))))

(defun dired-posframe--hide ()
  "Hide dired-posframe."
  (posframe-hide dired-posframe-buffer))

(defun dired-posframe--show ()
  "Show dired-posframe for current dired item."
  (let ((path (dired-get-filename nil 'noerror))
        hide)
    (with-current-buffer (get-buffer-create dired-posframe-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (cond
         ((not path)
          (setq hide t))
         ((file-directory-p path)
          (insert (with-current-buffer (dired-noselect path)
                    (buffer-substring-no-properties (point-min) (point-max)))))
         ((file-readable-p path)
          (let ((size (file-attribute-size (file-attributes path))))
            (if (< dired-posframe-file-size-limit size)
                (insert (format "Exceeds limit; %dMB < %dMB"
                                (/ dired-posframe-file-size-limit (* 1024 1024))
                                (/ size (* 1024 1024))))
              (insert-file-contents path)))))
        (when path
          (let* ((name (file-name-nondirectory path))
                 (mode (assoc-default name auto-mode-alist #'string-match)))
            (if (memq mode dired-posframe-enable-modes)
                (set-auto-mode-0 mode)
              (set-auto-mode-0 'fundamental-mode 'keep-mode-if-same))))))
    (if hide
        (dired-posframe--hide)
      (dired-posframe-display dired-posframe-buffer))))


;;; Advices

(defun dired-posframe--advice-show (fn &rest args)
  "Around advice for FN with ARGS."
  (apply fn args)
  (dired-posframe--show))

(defun dired-posframe--advice-hide (fn &rest args)
  "Around advice for FN with ARGS."
  ;; `keyboard-quit' also quit this function!
  ;; We exec anything before apply original function.
  (dired-posframe--hide)
  (apply fn args))


;;; Main

(defvar dired-posframe-advice-alist
  '((forward-char        . dired-posframe--advice-show)
    (backward-char       . dired-posframe--advice-show)
    (dired-next-line     . dired-posframe--advice-show)
    (dired-previous-line . dired-posframe--advice-show)
    (dired-find-file     . dired-posframe--advice-hide)
    (keyboard-quit       . dired-posframe--advice-hide))
  "Alist for dired-posframe advice.
See `dired-posframe--setup' and `dired-posframe--teardown'.")

(defun dired-posframe-setup ()
  "Setup dired-posframe-mode."
  (pcase-dolist (`(,sym . ,fn) dired-posframe-advice-alist)
    (advice-add sym :around fn)))

(defun dired-posframe-teardown ()
  "Setup dired-posframe-mode."
  (dired-posframe--hide)
  (pcase-dolist (`(,sym . ,fn) dired-posframe-advice-alist)
    (advice-remove sym fn)))

;;;###autoload
(define-minor-mode dired-posframe-mode
  "Enable dired-posframe-mode."
  :lighter " dired-pf"
  :group 'dired-posframe
  (if dired-posframe-mode
      (dired-posframe-setup)
    (dired-posframe-teardown)))

(provide 'dired-posframe)

;;; dired-posframe.el ends here
