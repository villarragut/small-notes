;;; small-notes.el --- Small notes in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 Free Software Foundation, Inc.

;; Author: Víctor Muñoz Villarragut <victor.munoz@upm.es>
;; Maintainer: Víctor Muñoz Villarragut <victor.munoz@upm.es>
;; Created: 2024
;; Version: 1.1
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
	 (cond ((save-excursion
		  (re-search-backward "^\\* Backlinks" nil t))
		(message "Do not unlink notes from the Backlinks section!"))
	       (t
		;; Delete link in destination
		(let ((origin-name (file-name-nondirectory (buffer-file-name)))
		      (linked-file (org-element-property :path (org-element-context))))
		  ;; Open the linked file in the background
		  (with-current-buffer (find-file-noselect linked-file)
		    (re-search-forward "^\\*+ Backlinks" nil t)
		    (flush-lines origin-name (point) (point-max))
		    (save-buffer)))
		;; Delete link in current note
		(re-search-backward "\\[\\[") ; Go to the beginning of the link
		(when (looking-at org-link-bracket-re) ; Extract the description
		  (let ((description (match-string-no-properties 2)))
		    (replace-match (or description "") t t nil 0))))))
	(t (message "This is not a link!"))))

(defun small-notes--check-all-links-have-backlinks (temp-buffer)
  "Check that the forward links in all the notes have their corresponding backlinks."
  (with-current-buffer temp-buffer
    (insert "\nList of links without the corresponding backlinks:\n"))
  (let ((org-files (directory-files-recursively small-notes-folder "\\.org$"))) ;; Get all Org files in notes folder
    (dolist (file org-files)
      (with-current-buffer (find-file-noselect file)
        (beginning-of-buffer)
	(let ((backlinks-position (save-excursion (re-search-forward "^\\*+ Backlinks" nil t)))
	      (links (org-element-map (org-element-parse-buffer) 'link 'identity)))
	  (dolist (link links)
	    (when (and (string-match-p "file:" (org-element-property :raw-link link)) ; ignore web links
		       (not (string-match-p "images/" (org-element-property :raw-link link))) ; ignore images
		       (< (org-element-property :begin link) backlinks-position)) ; ignore backlinks
	      (let ((raw-link (string-replace "file:" "" (org-element-property :raw-link link))))
		(with-current-buffer (find-file-noselect raw-link)
		  (beginning-of-buffer)
		  (re-search-forward "^\\*+ Backlinks" nil t)
		  (unless (re-search-forward (file-name-nondirectory file) nil t)
		    (with-current-buffer temp-buffer
		      (insert (concat "  - Backlink in " raw-link " to " (file-name-nondirectory file) " missing.\n")))))))))))))

(defun small-notes--check-all-backlinks-have-links (temp-buffer)
  "Check that the backlinks in all the notes have their corresponding forward links."
  (with-current-buffer temp-buffer
    (insert "\nList of backlinks without the corresponding links:\n"))
  (let ((org-files (directory-files-recursively small-notes-folder "\\.org$"))) ;; Get all Org files in notes folder
    (dolist (file org-files)
      (with-current-buffer (find-file-noselect file)
        (beginning-of-buffer)
	(let ((backlinks-position (save-excursion (re-search-forward "^\\*+ Backlinks" nil t)))
	      (links (org-element-map (org-element-parse-buffer) 'link 'identity)))
	  (dolist (link links)
	    (when (and (string-match-p "file:" (org-element-property :raw-link link)) ; ignore web links
		       (not (string-match-p "images/" (org-element-property :raw-link link))) ; ignore images
		       (> (org-element-property :begin link) backlinks-position)) ; ignore forward links
	      (let ((raw-link (string-replace "file:" "" (org-element-property :raw-link link))))
		(with-current-buffer (find-file-noselect raw-link)
		  (beginning-of-buffer)
		  (re-search-forward "^\\*+ Backlinks" nil t)
		  (unless (re-search-backward (file-name-nondirectory file) nil t)
		    (with-current-buffer temp-buffer
		      (insert (concat "  - Forward link in " raw-link " to " (file-name-nondirectory file) " missing.\n")))))))))))))

(defun small-notes--check-link-integrity ()
  "Check the integrity of forward and back links."
  (interactive)
  (let ((temp-buffer (get-buffer-create "*Link integrity*")))
    (with-current-buffer temp-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq-local buffer-offer-save nil)
      (beginning-of-buffer)
      (insert "Integrity check\n"))
    (small-notes--check-all-links-have-backlinks temp-buffer)
    (small-notes--check-all-backlinks-have-links temp-buffer)
    (with-current-buffer temp-buffer
      (setq buffer-read-only t))
    (display-buffer temp-buffer)))


;;;;;;;;;;;;;;;;;;
;; File actions ;;
;;;;;;;;;;;;;;;;;;

(defun small-notes--note-title-to-file-name (&optional date-string)
  "Prompt for a note title and return the corresponding file name.

If DATE-STRING is provided, use it as the date prefix (e.g. \"20241015\").
Otherwise, use today's date as returned by `format-time-string'."
  (setq small-notes--last-note-title
        (read-string "Title: "))
  (let* ((date (or date-string (format-time-string "%Y%m%d")))
         (slug (replace-regexp-in-string
                (regexp-quote " ") "_" (downcase small-notes--last-note-title) nil 'literal))
         (filename (concat date "_" slug ".org")))
    (expand-file-name filename small-notes-folder)))

(defun small-notes--rename-note ()
  "Change a note's title and file name, together with all the links and images.
Preserves the original date prefix from the old file name."
  (interactive)
  (beginning-of-buffer)
  (let* ((old-title (and (search-forward "#+TITLE:" nil t)
                         (string-replace "#+TITLE: " "" (string-trim-right (thing-at-point 'line t)))))
         (old-file-name (file-name-nondirectory (buffer-file-name)))
         ;; Extract the date prefix
         (old-date (when (string-match "^\\([0-9]\\{8\\}[a-z]?\\)_" old-file-name)
                     (match-string 1 old-file-name)))
         (new-file-name (file-name-nondirectory
                         (small-notes--note-title-to-file-name old-date))))
    ;; Change the title line in this buffer
    (goto-char (point-min))
    (while (re-search-forward "^#\\+TITLE:.+" nil t)
      (replace-match (concat "#+TITLE: " small-notes--last-note-title) nil nil))
    ;; Rename links in other notes and image file names
    (let ((links (org-element-map (org-element-parse-buffer) 'link 'identity)))
      (dolist (link links)
        (let ((raw-link (string-replace "file:" "" (org-element-property :raw-link link))))
          (cond
           ((string-match-p "\\(https?://\\)" raw-link)
            (message "Ignoring web link..."))
           ((string-match-p "images/" raw-link)
            ;; rename image files if their name includes the note's basename
            (rename-file raw-link
                         (string-replace (file-name-base old-file-name)
                                         (file-name-base new-file-name)
                                         raw-link)
                         t))
           (t
            ;; update references in linked notes
            (with-current-buffer (find-file-noselect raw-link)
              (goto-char (point-min))
              (while (re-search-forward old-file-name nil t)
                (replace-match new-file-name nil nil))
              (goto-char (point-min))
              (while (re-search-forward (regexp-quote old-title) nil t)
                (replace-match small-notes--last-note-title nil nil))
              (save-buffer)))))))
    ;; Rename image references inside the current note
    (goto-char (point-min))
    (while (re-search-forward (regexp-quote (file-name-base old-file-name)) nil t)
      (replace-match (file-name-base new-file-name) nil nil))
    ;; Save buffer with the new name, delete the old one
    (write-file new-file-name)
    (delete-file old-file-name)
    (message "Renamed note → %s" new-file-name)))


(defun small-notes--delete-note ()
  "Delete a note, together with all the links and images."
  (interactive)
  (when (yes-or-no-p "Do you really want to delete this note?")
    (let ((title (progn
		   (beginning-of-buffer)
		   (search-forward "#+TITLE:")
		   (string-replace "#+TITLE: " "" (string-trim-right (thing-at-point 'line t)))))
	  (file-name (file-name-nondirectory (buffer-file-name))))
      ;; Remove links in other notes and delete images
      (let ((links (org-element-map (org-element-parse-buffer) 'link 'identity)))
	(dolist (link links)
	  (when (string-match-p "file:" (org-element-property :raw-link link)) ; ignore web links
	    (let ((raw-link (string-replace "file:" "" (org-element-property :raw-link link))))
              (cond ((string-match-p "images/" raw-link) ; delete images
		     (delete-file raw-link))
		    (t ; remove links in other notes
		     (with-current-buffer (find-file-noselect raw-link)
		       (beginning-of-buffer) ; delete backlinks
		       (re-search-forward "^\\*+ Backlinks" nil t)
		       (flush-lines file-name (point) (point-max))
                       (beginning-of-buffer) ; delete links that are not backlinks
		       (message " ---> %s" title)
		       (while (re-search-forward (concat "\\[\\[file:" file-name "\\]\\[.*?\\]\\]") nil t)
			 (replace-match title nil nil))
                       (save-buffer))))))))
      ;; Delete the file visited by this buffer
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
          (let* ((start (progn (backward-paragraph) (point)))
                 (end (progn (forward-paragraph) (point))))
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
   "#+SETUPFILE: setup.org\n#+TITLE: %((lambda() small-notes--last-note-title))\n\n  %i%?\n\n* Backlinks\n"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find backlink sequences (section programmed with ChatGPT) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun small-notes--collect-backlinks (directory)
  "Return an alist mapping each Org file in DIRECTORY to its backlinks."
  (let ((org-files (directory-files directory t "\\.org$"))
        backlinks)
    (dolist (file org-files backlinks)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (re-search-forward "^\\*+ Backlinks" nil t)
          (let (targets)
            (while (re-search-forward "\\[\\[file:\\([^]]+\\)\\]" nil t)
              (push (match-string 1) targets))
            (setq targets (delete-dups targets))
            (push (cons (file-name-nondirectory file) targets)
                  backlinks)))))))

(defun small-notes--dfs-backlinks (start map path results min-length visited depth-limit)
  "Recursive DFS through backlink MAP starting from START.
PATH is the current traversal path.
VISITED is a hash-table of globally visited nodes.
DEPTH-LIMIT prevents runaway recursion."
  (let ((nexts (cdr (assoc start map))))
    (puthash start t visited)
    (if (or (null nexts)
            (>= (length path) depth-limit))
        (when (>= (length path) min-length)
          (push (reverse path) results))
      (dolist (n nexts)
        (unless (member n path) ;; local cycle check
          (setq results
                (small-notes--dfs-backlinks n map (cons n path)
                                            results min-length visited depth-limit)))))
    results))

(defun small-notes-find-backlink-sequences (map min-length &optional depth-limit)
  "Return all backlink sequences of at least MIN-LENGTH in MAP.
DEPTH-LIMIT stops runaway recursion in cyclic graphs."
  (let ((visited (make-hash-table :test 'equal))
        sequences)
    (dolist (pair map sequences)
      (let ((start (car pair)))
        (unless (gethash start visited)
          (setq sequences
                (append sequences
                        (small-notes--dfs-backlinks start map (list start)
                                                    nil min-length visited
                                                    (or depth-limit 50)))))))))

(defun small-notes--normalize-sequence (seq)
  "Return a normalized version of SEQ so rotations of the same cycle are identical.
SEQ is a list of filenames (strings)."
  (let* ((rotations (cl-loop for i below (length seq)
                             collect (append (nthcdr i seq)
                                             (cl-subseq seq 0 i))))
         (stringified (mapcar (lambda (lst)
                                (mapconcat #'identity lst "→"))
                              rotations))
         (sorted (sort stringified #'string<)))
    (car sorted)))

(defun small-notes--dedupe-cyclic-sequences (sequences)
  "Remove duplicate cycles that differ only by rotation."
  (let ((seen (make-hash-table :test 'equal))
        deduped)
    (dolist (seq sequences)
      (let ((norm (small-notes--normalize-sequence seq)))
        (unless (gethash norm seen)
          (puthash norm t seen)
          (push seq deduped))))
    deduped))

(defun small-notes--filter-subchains (chains)
  "Remove CHAINs that are strict sublists of longer ones."
  (cl-remove-if
   (lambda (chain)
     (cl-some (lambda (other)
                (and (not (equal chain other))
                     (cl-subsetp chain other :test #'equal)))
              chains))
   chains))

(defun small-notes--show-backlink-sequences (&optional min-length)
  "Find and display backlink sequences of at least MIN-LENGTH notes.
Scans all .org files in ~/simple-notes/note-folder/ and prints results
in a new Org-mode buffer.
Handles loops safely, deduplicates cycles, and filters out shorter subchains."
  (interactive "nMinimum sequence length: ")
  (let* ((directory (expand-file-name small-notes-folder))
         (map (small-notes--collect-backlinks directory)))
    (message "Scanning notes in %s ..." directory)
    (let* ((raw-chains (small-notes-find-backlink-sequences map min-length 50))
           (deduped (small-notes--dedupe-cyclic-sequences raw-chains))
           (chains (small-notes--filter-subchains deduped))
           (buf (get-buffer-create "*Zettelkasten Backlink Sequences*")))
      (with-current-buffer buf
        (erase-buffer)
        (insert (format "#+title: Backlink Sequences (≥ %d links)\n\n" min-length))
        (if (null chains)
            (insert "No sequences found.\n")
          (cl-loop for seq in chains
                   for n from 1
                   do (progn
                        (insert (format "* Sequence %d (%d notes)\n" n (length seq)))
                        (dolist (f seq)
                          (insert (format "- [[file:%s][%s]]\n"
                                          (expand-file-name f directory)
                                          (file-name-base f))))
                        (insert "\n"))))
        (org-mode))
      (switch-to-buffer buf)
      (message "Found %d backlink sequences." (length chains)))))


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
    ("g" "Check integrity" small-notes--check-link-integrity)
    ("b" "Backlink sequences" small-notes--show-backlink-sequences)
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
