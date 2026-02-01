;;; epkg-marginalia.el --- Show Epkg information in completion annotations  -*- lexical-binding:t -*-

;; Copyright (C) 2021-2026 Jonas Bernoulli

;; Author: Jonas Bernoulli <emacs.epkg-marginalia@jonas.bernoulli.dev>
;; Homepage: https://github.com/emacscollective/epkg-marginalia
;; Keywords: tools

;; Package-Version: 1.1.2
;; Package-Requires: (
;;     (emacs     "28.1")
;;     (compat    "30.1")
;;     (epkg       "4.1")
;;     (marginalia "2.7"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Out of the box marginalia enriches package completion with
;; information from the built-in package manager.  This packages
;; teaches it to additionally use the Epkgs database, and if the
;; Borg package manager is available, then it uses information
;; provided by that as well.

;;   (with-eval-after-load 'marginalia
;;     (setcar (alist-get 'package marginalia-annotators)
;;             #'epkg-marginalia-annotate-package))

;;; Code:

(require 'compat)

(require 'marginalia)
(require 'epkg)
(require 'borg nil t)

(eval-when-compile (require 'package))
(declare-function package-load-all-descriptors "package-activate" ())
(declare-function package-read-all-archive-contents "package" ())
(declare-function package--from-builtin "package" (bi-desc))
(declare-function package-version-join "package-activate" (vlist))
(declare-function package-installed-p "package-activate"
                  (package &optional min-version))
(defvar package--builtins)
(defvar package--initialized)
(defvar package-alist)
(defvar package-archive-contents)

(defgroup epkg-marginalia nil
  "Show Epkg information in completion annotations."
  :group 'epkg
  :group 'marginalia)

(defcustom epkg-marginalia-show-version nil
  "Whether to show version information.
This information comes from Package alone, so if you use Borg,
it may not be accurate.."
  :package-version '(epkg-marginalia . "0.1.0")
  :group 'transient
  :type 'boolean)

;;;###autoload
(defun epkg-marginalia-annotate-package (cand)
  "Annotate package CAND with its description summary.
Uses information provided by `borg', `epkg' and `package'."
  (require 'package)
  (unless package--initialized
    (setq package-alist nil)
    (package-load-all-descriptors)
    (package-read-all-archive-contents)
    (setq package--initialized t))
  (let* ((name (replace-regexp-in-string "-[[:digit:]\\.-]+\\'" "" cand))
         (epkg (epkg name))
         (symb (intern name))
         ;; Upstream 459f230 seems like overkill, considering its cost.
         (desc (or (car (alist-get symb package-alist))
                   (if-let (built-in (assq symb package--builtins))
                       (package--from-builtin built-in)
                     (car (alist-get symb package-archive-contents))))))
    (when (or epkg desc)
      (marginalia--fields
       ((if (and desc epkg-marginalia-show-version)
            (package-version-join (package-desc-version desc))
          "")
        :truncate (if (and desc epkg-marginalia-show-version) 16 0)
        :face 'marginalia-version)
       ((cond
          ((and desc (eq (package-desc-dir desc) 'builtin))
           (propertize "builtin" 'face 'marginalia-installed))
          ((and epkg
                (fboundp 'borg-drones)
                (member name (borg-drones)))
           (propertize "assimilated" 'face 'marginalia-installed))
          ((and epkg
                (fboundp 'borg-clones)
                (member name (borg-clones)))
           (propertize "cloned" 'face 'marginalia-installed))
          ((and desc (package-installed-p desc))
           (propertize "installed" 'face 'marginalia-installed))
          (epkg
           (propertize "mirror" 'face 'marginalia-archive))
          (desc
           (propertize (package-desc-archive desc) 'face 'marginalia-archive)))
        :truncate 12)
       ((or (and epkg (oref epkg summary))
            (and desc (package-desc-summary desc)))
        :truncate 1.0
        :face 'marginalia-documentation)))))

;;; _
(provide 'epkg-marginalia)
;; Local Variables:
;; indent-tabs-mode: nil
;; lisp-indent-local-overrides: ((cond . 0) (interactive . 0))
;; End:
;;; epkg-marginalia.el ends here
