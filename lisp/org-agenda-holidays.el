;;; org-agenda-holidays.el --- replace US holidays with Canadian ones

;; Copyright (c) 2022-2024 Daniel Hennigar

;; Author: Daniel Hennigar
;; URL: https://github.com/dhennigar/emacs-config
;; Package-Requires: ((emacs "29.4"))

;; This file is NOT part of GNU Emacs

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 or any later version.

;; This file is distributed in the hope that it will be useful,
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file. If not, see <https://www.gnu.org/licenses/>.
;;; Commentary:

;; Org-mode ships with US and religious holidays by default. This file contains
;; all the settings necessary to remove them and define Canadian statutory holidays
;; in their place. I have moved this code to its one script to reduce the length
;; of my init.el file.

;;; Code:

;; Define local holidays
(setq holiday-local-holidays
	'((holiday-fixed 1 1 "New Year's Day")
	  (holiday-float 2 1 3 "Family Day")
	  (holiday-easter-etc -2 "Good Friday")
	  (holiday-float 5 1 -2 "Victoria Day")
	  (holiday-fixed 6 21 "Indigenous Peoples Day")
	  (holiday-fixed 7 1 "Canada Day")
	  (holiday-float 8 1 1 "BC Day")
	  (holiday-fixed 9 30 "National Day for Truth and Reconcilliation")
	  (holiday-float 10 1 2 "Canadian Thanksgiving")
	  (holiday-fixed 10 31 "Halloween")
	  (holiday-fixed 11 11 "Rememberance Day")
	  (holiday-float 11 4 4 "American Thanksgiving")
	  (holiday-fixed 12 25 "Christmas")))

;; Remove US/religious holidays
(setq holiday-general-holidays nil)	
(setq holiday-christian-holidays nil)	
(setq holiday-islamic-holidays nil)	
(setq holiday-bahai-holidays nil)
(setq holiday-hebrew-holidays nil)
(setq holiday-oriental-holidays nil)

;; Use the holidays that I defined above
(setq calendar-holidays holiday-local-holidays)

(provide 'org-agenda-holidays)
;;; org-agenda-holidays.el ends here
