:;;; init-projectile.el ---  -*- lexical-binding: t -*-

;; Author: Alex de Wit
;; Maintainer: Alex de Wit
;; Version: 
;; Package-Requires: ()
;; Homepage: 
;; Keywords: 


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This file initializes projectile.

;;; Code:

(use-package projectile
  :general
  (leader-def 'normal python-mode-map "p t" 'projectile-test-project))

(provide 'init-projectile)

;;; init-projectile.el ends here
