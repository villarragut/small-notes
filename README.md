# org-zk: Org Zettelkasten

This is a _very_ simple package that intends to replace org-roam in very specific use cases.

It is in the line of Protesilaos Stavrou's package [denote](https://github.com/protesilaos/denote) in spirit. I wouldn't have written this package if I'd known he was working on something probably better. Still this package suits my usage perfectly and I think it might as well serve others.

![screenshot1](https://github.com/user-attachments/assets/45c6c714-d44c-45c6-be9f-4718f683c384)

## Installation

Download `org-zk.el` and save it in a directory where Emacs finds it. Then, add something like this to your dot emacs file:

```
(use-package org-zk
  :after org-capture)
```

## Usage

- Download the `setup.org` file and place it in your notes folder (by default, `~/notes`). This file sets up some decent defaults when exporting to PDF.
- Call `org-capture` and press the capture char for notes (by default, `n`). Press `C-c C-c` when done.
- When visiting a note, press the transient menu binding (by default, `C-c n`) in order to:
    - Rename the note.
    - Delete the note.
    - Link this note to another note (the links are created symetrically).
    - Unlink this note from another note.
    - Take a screenshot and insert it.
    - Insert an image from a file.
    - Delete an image.
    - Toggle the visualization of Org links.
    - Toggle the visualization of image.
    - Show LaTeX symbols (requires xenops).
    - Show LaTeX font.

## Customization

You can choose what character is used by org-capture, what key binding is added to org-mode-map to launch the transient menu, and what is your notes folder. 

```
(use-package org-zk
  :after org-capture
  :custom
  (org-zk-notes-folder "~/Documents/notes")
  (org-zk-capture-char "n"
  (org-zk-transient-binding "C-c n")))
```
