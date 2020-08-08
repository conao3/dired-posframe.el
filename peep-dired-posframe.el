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
