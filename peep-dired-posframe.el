;;; peep-dired-posframe.el --- Peep dired items using posframe  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1") (posframe "0.7"))
;; URL: https://github.com/conao3/peep-dired-posframe.el

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

(defgroup peep-dired-posframe nil
  "Peep dired items using posframe."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/conao3/peep-dired-posframe.el"))

(defcustom peep-dired-posframe-style 'point
  "The style of peep-dired-posframe."
  :group 'peep-dired-posframe
  :type 'symbol)

(defcustom peep-dired-posframe-font nil
  "The font used by peep-dired-posframe.
When nil, Using current frame's font as fallback."
  :group 'peep-dired-posframe
  :type '(choice (const :tag "inherit" nil)
                 string))

(defcustom peep-dired-posframe-width nil
  "The width of peep-dired-posframe."
  :group 'peep-dired-posframe
  :type '(choice (const :tag "default" nil)
                 number))

(defcustom peep-dired-posframe-height nil
  "The height of peep-dired-posframe."
  :group 'peep-dired-posframe
  :type '(choice (const :tag "default" nil)
                 number))

(defcustom peep-dired-posframe-min-width nil
  "The width of peep-dired-min-posframe."
  :group 'peep-dired-posframe
  :type '(choice (const :tag "non-width" nil)
                 number))

(defcustom peep-dired-posframe-min-height nil
  "The height of peep-dired-min-posframe."
  :group 'peep-dired-posframe
  :type '(choice (const :tag "non-width" nil)
                 number))

(defcustom peep-dired-posframe-size-function #'peep-dired-posframe-get-size
  "The function which is used to deal with posframe's size."
  :group 'peep-dired-posframe
  :type 'function)

(defcustom peep-dired-posframe-border-width 1
  "The border width used by peep-dired-posframe.
When 0, no border is showed."
  :group 'peep-dired-posframe
  :type '(choice (const :tag "non-width" nil)
                 number))

(defcustom peep-dired-posframe-parameters nil
  "The frame parameters used by peep-dired-posframe."
  :group 'peep-dired-posframe
  :type '(choice (const :tag "no-parameters" nil)
                 number))

(defface peep-dired-posframe
  '((t (:inherit default)))
  "Face used by the peep-dired-posframe."
  :group 'peep-dired-posframe)

(defface peep-dired-posframe-border
  '((t (:inherit default :background "gray50")))
  "Face used by the peep-dired-posframe's border."
  :group 'peep-dired-posframe)

(defvar peep-dired-posframe-buffer " *peep-dired-posframe-buffer*"
  "The posframe-buffer used by peep-dired-posframe.")

(defvar peep-dired-posframe--display-p nil
  "The status of `peep-dired-posframe--display'.")

(defun peep-dired-posframe--display (str &optional poshandler)
  "Show STR in ivy's posframe with POSHANDLER."
  (if (not (posframe-workable-p))
      (warn "`posframe' can not be workable in your environment")
    (apply #'posframe-show
           peep-dired-posframe-buffer
           :font peep-dired-posframe-font
           :string str
           :position (point)
           :poshandler poshandler
           :background-color (face-attribute 'peep-dired-posframe :background nil t)
           :foreground-color (face-attribute 'peep-dired-posframe :foreground nil t)
           :internal-border-width peep-dired-posframe-border-width
           :internal-border-color (face-attribute 'peep-dired-posframe-border :background nil t)
           :override-parameters peep-dired-posframe-parameters
           (funcall peep-dired-posframe-size-function))))

(defun peep-dired-posframe-get-size ()
  "The default functon used by `peep-dired-posframe-size-function'."
  (list
   :height peep-dired-posframe-height
   :width peep-dired-posframe-width
   :min-height (or peep-dired-posframe-min-height
                   ;; (let ((height (+ ivy-height 1)))
                   ;;   (min height (or peep-dired-posframe-height height)))
                   0)
   :min-width (or peep-dired-posframe-min-width
                  (let ((width (round (* (frame-width) 0.62))))
                    (min width (or peep-dired-posframe-width width))))))

(defun peep-dired-posframe-display (str)
  "Display STR via `posframe' by `peep-dired-posframe-style'."
  (let ((func (intern (format "peep-dired-posframe-display-at-%s" peep-dired-posframe-style))))
    (if (functionp func)
        (funcall func str)
      (funcall (intern (format "peep-dired-posframe-display-at-%s" "point")) str))))

(eval
 `(progn
    ,@(mapcar
       (lambda (elm)
         `(defun ,(intern (format "peep-dired-posframe-display-at-%s" (car elm))) (str)
            ,(format "Display STR via `posframe' at %s" (car elm))
            (peep-dired-posframe--display str #',(intern (format "posframe-poshandler-%s" (cdr elm))))))
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


;;; Main

(defun peep-dired-posframe-setup ()
  "Setup peep-dired-posframe-mode.")

(defun peep-dired-posframe-teardown ()
  "Setup peep-dired-posframe-mode.")

;;;###autoload
(define-minor-mode peep-dired-posframe-mode
  "Enable peep-dired-posframe-mode."
  :lighter " peep-dired-pf"
  :group 'peep-dired-posframe
  (if peep-dired-posframe-mode
      (peep-dired-posframe-setup)
    (peep-dired-posframe-teardown)))

(provide 'peep-dired-posframe)

;;; peep-dired-posframe.el ends here
