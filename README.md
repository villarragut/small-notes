# small-notes: Create small notes in Emacs and link them

This is a _very_ simple package that makes it easy to write small notes and link them symetrically.

It is in the line of Protesilaos Stavrou's package [Denote](https://github.com/protesilaos/denote) in spirit, but a whole lot simpler. I wouldn't have written this package if I'd known he was working on Denote. Still this package suits my usage perfectly and I think it might as well serve others.

![screenshot1](https://github.com/user-attachments/assets/45c6c714-d44c-45c6-be9f-4718f683c384)
<p align="center">
    <img src="https://github.com/user-attachments/assets/5a42015e-0ebf-4a9b-bd8e-1a6a013db806"/>
</p>

## Some features

- It doesn't rely on a data base. It's all text (links are org links), which makes it easy to maintain and future proof.
- Note file names follow a predictable scheme.
- It's based on Org mode, but it adds a few helper functions that alleviate a lot of repetitive tasks and housekeeping:
    - create notes with a fixed structure and a file name according to a fixed pattern (it uses org-capture under the hood);
    - rename the notes (together with all the links to them in other notes and inline image file names);
    - delete the notes (together with all the links to them in other notes and inline image files);
    - link notes to one another (creating symmetrical links in both linked notes);
    - delete links among notes;
    - and carry out a few other minor tasks.
- A transient menu like the one in Magit with the helper functions is included.

## Installation

Download `small-notes.el` and save it in a directory where Emacs finds it. Then, add something like this to your dot emacs file:

```
(use-package small-notes
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
(use-package small-notes
  :after org-capture
  :custom
  (small-notes-folder "~/Documents/notes")
  (small-notes-capture-char "n"
  (small-notes-transient-binding "C-c n")))
```
