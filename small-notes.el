;;; small-notes.el --- Small notes in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2024-2024 Free Software Foundation, Inc.

;; Author: Víctor Muñoz Villarragut <victor.munoz@upm.es>
;; Maintainer: Víctor Muñoz Villarragut <victor.munoz@upm.es>
;; Created: 2024
;; Version: 1.0
;; Package-Requires: ((emacs "29.1") (transient "0.7.5")
;; Homepage: https://github.com/villarragut/small-notes
;; Keywords: notes

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a simple interface to create and link notes.

;;; Code:

(defcustom small-notes-folder "~/notes" "Default folder for notes.")
(defcustom small-notes-capture-char "n" "Char pressed to create a new note with org-capture.")
(defcustom small-notes-transient-binding "C-c n" "Key binding (kbd notation) added to org-mode-map to show the transient menu.")

(defun small-notes--write-note-link (destination)
  "Write a link to the note in the current buffer in the destination and conversely."
  ;; get origin title
  (save-excursion
    (beginning-of-buffer)
    (search-forward "+TITLE: ")
    (set-mark-command nil)
    (move-end-of-line nil)
    (let ((origin-title (buffer-substring (region-beginning) (region-end)))
	  (origin-buffer (buffer-name)))
      ;; write link in destination
      (find-file (expand-file-name destination small-notes-folder)) ; concatenate paths and file names with expand-file-name
      (delete-trailing-whitespace)
      (end-of-buffer)
      (insert
       (concat "  - [[file:" origin-buffer "][" origin-title "]]"))
      (save-buffer)
      ;; get destination title
      (beginning-of-buffer)
      (search-forward "+TITLE: ")
      (set-mark-command nil)
      (move-end-of-line nil)
      (let ((destination-title (buffer-substring (region-beginning) (region-end))))
	;; write link in origin
	(switch-to-buffer origin-buffer)
	(delete-trailing-whitespace)
	(end-of-buffer)
	(insert
	 (concat "  - [[file:" destination "][" destination-title "]]"))
	(save-buffer)))))

(defun small-notes--link-note ()
  "Link this note to another note and conversely."
  (interactive)
  (let ((selection (completing-read
		    "Link to this note: "
		    (mapcar
		     'file-name-nondirectory
		     (file-expand-wildcards
		      (expand-file-name "*.org" small-notes-folder))))))
    (small-notes--write-note-link selection)))

(defun small-notes--unlink-note ()
  "Unlink two notes."
  (interactive)
  (cond ((org-in-regexp org-link-bracket-re 1)
	 ;; delete link in destination
	 (save-excursion
	   (let ((origin-name (file-name-nondirectory (buffer-file-name))))
	     (org-open-at-point)
	     (flush-lines origin-name (point-min) (point-max))
	     (save-buffer)))
	 ;;delete link in current note
	 (kill-whole-line))
	(t (message "This is not a link!"))))

(defun small-notes--note-title-to-file-name ()
  "Set the note title, save it in small-notes--last-note-title, and return the corresponding file name."
  (setq small-notes--last-note-title (read-string "Title: "))
  (expand-file-name
   (concat
    (format-time-string "%Y%m%d")
    "_"
    (replace-regexp-in-string (regexp-quote " ") "_" (downcase small-notes--last-note-title) nil 'literal)
    ".org")
   small-notes-folder))

(defun small-notes--rename-note ()
  "Change a note's title and file name, together with all the links and images."
  (interactive)
  (beginning-of-buffer)
  (let ((old-title (and (search-forward "#+TITLE:")
			(string-replace "#+TITLE: " "" (string-trim-right (thing-at-point 'line t)))))
	(old-file-name (file-name-nondirectory (buffer-file-name)))
	(new-file-name (file-name-nondirectory (small-notes--note-title-to-file-name))))
    ;; Change title
    (beginning-of-buffer)
    (replace-regexp "+TITLE:.+" (concat "+TITLE: " small-notes--last-note-title))
    ;; Rename links in other notes and image file names
    (beginning-of-buffer)
    (org-next-link) ; go to first link if any
    (while (not org-link--search-failed) ; if a link was found, process link
      (let ((link (string-replace "file:" "" (org-element-property :raw-link (org-element-context)))))
	(cond ((string-match-p "images/" link) ; rename images
	       (rename-file link (string-replace (file-name-base old-file-name) (file-name-base new-file-name) link)))
	      (t ; rename links in other notes
	       (save-excursion
		 (org-open-at-point)
		 (beginning-of-buffer)
		 (replace-regexp old-file-name new-file-name)
		 (beginning-of-buffer)
		 (replace-regexp old-title small-notes--last-note-title)
		 (save-buffer))))
	(org-next-link))) ; try to search another link
    ;; Rename image links
    (beginning-of-buffer)
    (replace-regexp (file-name-base old-file-name) (file-name-base new-file-name))
    ;; Save this buffer with the new name
    (write-file new-file-name)
    ;; Delete the old file associated to this buffer
    (delete-file old-file-name)))

(defun small-notes--delete-note ()
  "Delete a note, together with all the links and images."
  (interactive)
  (when (yes-or-no-p "Do you really want to delete this note?")
    (let ((file-name (file-name-nondirectory (buffer-file-name))))
      ;; Delete links in other notes and image files
      (beginning-of-buffer)
      (org-next-link) ; go to first link if any
      (while (not org-link--search-failed) ; if a link was found, process link
	(let ((link (string-replace "file:" "" (org-element-property :raw-link (org-element-context)))))
	  (cond ((string-match-p "images/" link) ; delete images
		 (delete-file link))
		(t ; delete links in other notes
		 (save-excursion
		   (org-open-at-point)
		   (flush-lines file-name (point-min) (point-max))
		   (save-buffer)))))
	(org-next-link)) ; try to search for another link
      ;; Delete the file associated to this buffer
      (delete-file file-name)
      (kill-buffer (current-buffer)))))

(defun small-notes--delete-image ()
  "Delete an image."
  (interactive)
  (cond ((org-in-regexp org-link-bracket-re 1) ; check if the point is on a link
	 (let ((link (string-replace "file:" "" (org-element-property :raw-link (org-element-context)))))
	   (cond ((string-match-p "images/" link); check if the link corresponds to an image
		  (when (yes-or-no-p "Do you really want to delete this image?")
		    (delete-file link)
		    (kill-whole-line)))
		 (t (message "This link is not an image!")))))
	(t (message "This is not an image (or even a link)!"))))

(defun small-notes--org-download-image-with-file-picker ()
  "Insert an image by picking a file."
  (interactive)
  (let ((initial-folder "~/"))
    (org-download-image (read-file-name "Pick an image: " initial-folder))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transient interface ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'transient)

(transient-define-prefix small-notes--notes-transient ()
  "Transient for note-related actions."
  [["File"
    ("r" "Rename" small-notes--rename-note)
    ("dn" "Delete" small-notes--delete-note)
    ]
   ["Links"
    ("l" "Link" small-notes--link-note)
    ("u" "Unlink" small-notes--unlink-note)
    ]
   ["Images"
    ("c" "Capture" org-download-screenshot)
    ("i" "Insert" small-notes--org-download-image-with-file-picker)
    ("di" "Delete" small-notes--delete-image)
    ]
   ["View"
    ("tl" "Toggle links" org-toggle-link-display)
    ("ti" "Toggle images" org-toggle-inline-images) 
    ]
   ["LaTeX"
    ("s" "Show symbols" xenops-render)
    ("f" "Show font" xenops-reveal)
    ]])

(defun small-notes-transient-if-in-notes-folder ()
  "Invoke `small-notes--notes-transient` if the current buffer is in `small-notes-folder`."
  (interactive)
  (let ((file (buffer-file-name)))
  (if (and file (string-prefix-p (expand-file-name small-notes-folder)
				 (expand-file-name file)))
      (small-notes--notes-transient)
    (message "Not in notes folder!"))))

;; binding for transient menu
(define-key org-mode-map (kbd small-notes-transient-binding) 'small-notes-transient-if-in-notes-folder)

;; org-capture template
(add-to-list
 'org-capture-templates
 `(,small-notes-capture-char
   "Note"
   plain
   (file (lambda() (small-notes--note-title-to-file-name))) ; sets the variable small-notes--last-note-title
   "#+SETUPFILE: setup.org\n#+TITLE: %((lambda() small-notes--last-note-title))\n\n  %i%?\n\n* Links\n"))

(provide 'small-notes)
