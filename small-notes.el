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


;;;;;;;;;;;;;;;;;;;;;;
;; Custom variables ;;
;;;;;;;;;;;;;;;;;;;;;;

(defcustom small-notes-folder "~/notes" "Default folder for notes.")
(defcustom small-notes-capture-char "n" "Char pressed to create a new note with org-capture.")
(defcustom small-notes-transient-binding "C-c n" "Key binding (kbd notation) added to org-mode-map to show the transient menu.")


;;;;;;;;;;;
;; Links ;;
;;;;;;;;;;;

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
      (setq small-notes--destination-title (buffer-substring (region-beginning) (region-end)))
      (save-buffer)
      (switch-to-buffer origin-buffer)))
  ;; write link in origin
  (insert
   (concat "[[file:" destination "][" small-notes--destination-title "]]"))
  (save-buffer))

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


;;;;;;;;;;;;;;;;;;
;; File actions ;;
;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;
;; Images ;;
;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;
;; Backlink context ;;
;;;;;;;;;;;;;;;;;;;;;;

(defvar small-notes--context-overlays nil
  "List of overlays created by small-notes--add-paragraph-overlay-to-link.")

(defun small-notes--add-paragraph-overlay-to-link (link-element search-text)
  "Add an overlay showing a paragraph from the file in LINK-ELEMENT using SEARCH-TEXT."
  (when (and link-element (file-exists-p (org-element-property :path link-element)))
    ;; Open the linked file in a temporary buffer
    (with-temp-buffer
      (insert-file-contents (org-element-property :path link-element))
      (goto-char (point-min))
      (if (search-forward search-text nil t)
          (let* ((start (progn (beginning-of-line) (point)))
                 (end (progn (end-of-line) (point))))
            ;; Extract the paragraph content from the temp buffer
            (setq small-notes--context (buffer-substring-no-properties start end)))
        (message "Text not found in the file.")))
    ;; Now go back to the original buffer and add the overlay at the correct position
    (when small-notes--context
      (save-excursion
        ;; Move to the end of the link element in the original buffer
        (goto-char (org-element-property :end link-element))
        ;; Insert the overlay right after the link
        (let ((overlay (make-overlay (point) (point))))
          (overlay-put overlay 'after-string
                       (concat
                        "\n"
                        (mapconcat
                         'identity
                         (let ((text-chunks (split-string small-notes--context "\\(\\[\\[.*?\\]\\[\\|\\]\\]\\)"))
                               (counter 0)
                               (output '()))
                           ;; Loop through chunks and format them
                           (dolist (chunk text-chunks)
                             (if (eq (cl-rem counter 2) 0)
                                 (setq output (append output `(,chunk))) ;; Regular text
                               (setq output (append output
                                                    `(,(propertize chunk 'face '(:foreground "red")))))) ;; Red text for links
                             (setq counter (1+ counter)))
                           output)
                         "")
                        "\n"))
          ;; Add the overlay to the list of overlays
          (setq small-notes--context-overlays (cons overlay small-notes--context-overlays))
          (message "Overlay added with paragraph content right after the link."))))))

(defun small-notes--show-backlink-paragraphs ()
  "Add paragraph overlays to all links under the 'Backlinks' heading."
  (interactive)
  (save-excursion
    ;; Find the 'Backlinks' heading
    (goto-char (point-min))
    (if (re-search-forward "^\\*+ Backlinks" nil t)
        (let ((heading-pos (point)))  ;; Save the position of the Backlinks heading
          (org-narrow-to-subtree)      ;; Narrow to the Backlinks section
          ;; Find and process all links within the narrowed region
          (org-element-map (org-element-parse-buffer) 'link
            (lambda (link)
              (let* ((search-text (concat "file:" (file-name-nondirectory (buffer-file-name)))))
                (small-notes--add-paragraph-overlay-to-link link search-text))))
          (widen))  ;; Widen back to the full buffer
      (message "No 'Backlinks' heading found."))))

(defun small-notes--hide-all-paragraph-overlays ()
  "Remove all overlays stored in small-notes--context-overlays."
  (interactive)
  (mapc 'delete-overlay small-notes--context-overlays)
  (setq small-notes--context-overlays nil)
  (message "All backlink overlays removed."))

(defvar small-notes--backlink-context-shown nil
  "Whether backlink context is shown.")

(defun small-notes--toggle-backlink-context ()
  "Toggle backlink context."
  (interactive)
  (make-local-variable 'small-notes--backlink-context-shown)
  (cond (small-notes--backlink-context-shown
	 (small-notes--hide-all-paragraph-overlays)
	 (setq small-notes--backlink-context-shown nil))
	(t
	 (small-notes--show-backlink-paragraphs)
	 (setq small-notes--backlink-context-shown t))))


;;;;;;;;;;;
;; LaTeX ;;
;;;;;;;;;;;

(defvar small-notes--LaTeX-symbols-shown nil
  "Whether LaTeX symbols are shown.")

(defun small-notes--toggle-LaTeX-symbols ()
  "Toggle LaTeX symbols."
  (interactive)
  (make-local-variable 'small-notes--LaTeX-symbols-shown)
  (cond (small-notes--LaTeX-symbols-shown
	 (xenops-reveal)
	 (setq small-notes--LaTeX-symbols-shown nil))
	(t
	 (xenops-render)
	 (setq small-notes--LaTeX-symbols-shown t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-capture template ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list
 'org-capture-templates
 `(,small-notes-capture-char
   "Note"
   plain
   (file (lambda() (small-notes--note-title-to-file-name))) ; sets the variable small-notes--last-note-title
   "#+SETUPFILE: setup.org\n#+TITLE: %((lambda() small-notes--last-note-title))\n\n  %i%?\n\n* References\n\n* Backlinks\n"))


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
    ("c" "Toggle backlink context" small-notes--toggle-backlink-context)
    ]
   ["Images"
    ("s" "Screenshot" org-download-screenshot)
    ("i" "Insert" small-notes--org-download-image-with-file-picker)
    ("di" "Delete" small-notes--delete-image)
    ]
   ["View"
    ("k" "Toggle links" org-toggle-link-display)
    ("m" "Toggle images" org-toggle-inline-images) 
    ]
   ["LaTeX"
    ("x" "Toggle symbols" small-notes--toggle-LaTeX-symbols)
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


(provide 'small-notes)
