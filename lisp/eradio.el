;;; eradio.el --- A simple Internet radio player                       -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Olav Fosse; Copyright (C) 2024 Daniel Hennigar

;; Original author: Olav Fosse <mail@olavfosse.no>
;; URL: https://github.com/fossegrim/eradio

;; This program is free software; you can redistribute it and/or modify
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

;; A simple Internet radio player with added functions by Daniel Hennigar.

;;; Code:

;;;###autoload
(defcustom eradio-channels '()
  "Eradio's radio channels."
  :type '(repeat (cons (string :tag "Name") (string :tag "URL")))
  :group 'eradio)

(defcustom eradio-player '("vlc" "--no-video" "-I" "rc")
  "Eradio's player.
This is a list of the program and its arguments."
  :type '(choice
	  (const :tag "vlc"
		 ("vlc" "--no-video" "-I" "rc"))
	  (const :tag "vlc-mac"
		 ("/Applications/VLC.app/Contents/MacOS/VLC" "--no-video" "-I" "rc"))
	  (const :tag "mpv"
		 ("mpv" "--no-video" "--no-terminal")))
  :group 'eradio)

(defvar eradio--process nil "The process running the radio player.")
(defvar eradio-current-channel nil "The currently playing (or paused) channel.")
(defvar mode-line-eradio nil "Mode line indicator for current channel.")

(defun eradio--alist-keys (alist)
  "Get the keys from an ALIST."
  (mapcar #'car alist))

;;;###autoload
(defun eradio-stop ()
  "Stop the radio player."
  (interactive)
  (when eradio--process
    (delete-process eradio--process)
    (setq eradio--process nil)
    (setq mode-line-eradio nil)
    (force-mode-line-update)))

;;;###autoload
(defun eradio-toggle ()
  "Toggle the radio player."
  (interactive)
  (if eradio--process
      (eradio-stop)
    ;; If eradio-current-channel is nil, eradio-play will prompt the url
    (eradio-play eradio-current-channel)))

(defun eradio--play-low-level (url)
  "Play radio channel URL in a new process."
  (setq eradio--process
	(apply #'start-process
	       `("eradio--process" nil ,@eradio-player ,url))))

(defun eradio--get-url ()
  "Get a radio channel URL from the user."
  (let ((eradio-channel (completing-read
			 "Channel: "
			 (eradio--alist-keys eradio-channels)
			 nil nil)))
    (or (cdr (assoc eradio-channel eradio-channels)) eradio-channel)))

;;;###autoload
(defun eradio-play (&optional url)
  "Play a radio channel, do what I mean."
  (interactive)
  (let ((url (or url (eradio--get-url))))
    (eradio-stop)
    (setq eradio-current-channel url)
    (eradio--play-low-level url))
  (setq mode-line-eradio
	(format " (eradio: %s)"
 		(car (rassoc eradio-current-channel eradio-channels))))
  (force-mode-line-update))

(provide 'eradio)
;;; eradio.el ends here
