:;;; init-color-rg.el --- configures color-rg -*- lexical-binding: t -*-

;; Author: Alex de Wit
;; Maintainer: Alex de Wit
;; Version: 
;; Package-Requires: (dependencies)
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

;; This file loads and configures color-rg

;;; Code:

(use-package color-rg
  :load-path "~/.emacs.d/site-elisp/color-rg"
  :general
  (leader-def 'normal "/ /" 'color-rg-search-input-in-project'
	      "/ s" 'color-rg-search-symbol-in-project))
  
(provide 'init-color-rg)

;;; init-color-rg.el ends here
