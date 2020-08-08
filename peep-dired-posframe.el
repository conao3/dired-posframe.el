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
