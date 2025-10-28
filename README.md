# small-notes: Create and link small notes in Emacs

**small-notes** is a _very_ simple Emacs package that makes it easy to write small, interconnected notes — a lightweight, text-only _opinionated_ Zettelkasten that stays completely within Org mode.

It is in the spirit of [Protesilaos Stavrou’s Denote](https://github.com/protesilaos/denote) — both aim to make plain text notes linkable and durable — but **small-notes** is much simpler. It does not rely on an external database or complex metadata. Every link and backlink lives in plain Org text.

This package grew out of my own needs and workflow. It may fit yours too.

---

## ✳️ Highlights

- **All plain text.** Notes and links are pure Org files and Org links — no external index, no database, no risk of format rot.  
- **Predictable file names.** Each note’s file name follows a consistent pattern like `YYYYMMDD_slug.org`.  
- **Automatic backlinks.** When you create a link from one note to another, a backlink is inserted automatically in the target note’s `* Backlinks` section.  
- **Symmetric linking.** Links are always two-way and kept consistent automatically.  
- **Automatic link updates.** When you rename a note, all links *and backlinks* pointing to it are updated, as are image names and references.  
- **Stable identifiers.** Renaming a note keeps the original date-based ID so your links stay human-readable and temporally meaningful.  
- **Wikipedia-style inline links.** Notes can link contextually within the text — no need for a separate “Links” section.  
- **Backlink sequences explorer.** A built-in command discovers long chains of backlinks (trains of thought) and shows them as clickable Org lists.  
- **Transient menu.** All main functions are available from a Magit-style transient (by default, `C-c n`).  
- **Completely future-proof.** Every note is a plain Org file; no hidden metadata or external dependencies.

---

## 🪶 Some capabilities in detail

- **Create notes** with a fixed structure and predictable name via `org-capture`.  
- **Rename notes** — updates all links, backlinks, and image filenames; keeps the original date prefix.  
- **Delete notes** — cleans up all links, backlinks, and attached images.  
- **Link notes symmetrically** — adds both the forward link and a backlink automatically.  
- **Show backlink contexts** — see each backlink in its surrounding sentence or paragraph.  
- **Explore long trains of thought** — find connected sequences of notes by following backlinks recursively.  
- **Manage images** — insert screenshots, attach image files, and rename or delete them when renaming notes.  
- **Visual toggles** — hide or show Org links, images, LaTeX symbols, and Xenops math rendering interactively.  

---

## 📦 Installation

Download `small-notes.el` and save it somewhere in your `load-path`.  
Then add this to your Emacs configuration:

```elisp
(use-package small-notes
  :after org-capture)
```

---

## 🧭 Usage

1. Download `setup.org` and place it in your notes folder (by default `~/notes`).  
   It defines good defaults for exporting notes to PDF.  
2. Call `org-capture` and use the capture key (default: `n`) to create a new note.  
   Press `C-c C-c` when done.  
3. While visiting a note, press `C-c n` to open the transient menu. From there you can:

   - Rename or delete the note (keeping all links consistent).  
   - Link this note to another note (and automatically insert a backlink).  
   - Remove a link and its corresponding backlink.  
   - Show backlink contexts.  
   - Explore backlink sequences (long connected chains).  
   - Insert or delete images or screenshots.  
   - Toggle visualization of links, images, and LaTeX symbols.

---

## ⚙️ Customization

You can configure the capture key, transient key, and your notes folder:

```elisp
(use-package small-notes
  :after org-capture
  :custom
  (small-notes-folder "~/Documents/notes")
  (small-notes-capture-char "n")
  (small-notes-transient-binding "C-c n"))
```

---

## 🧩 Philosophy

**small-notes** is built around the idea that _your notes should outlive any software_.
Everything is plain Org text, with no dependence on a database or a specific Emacs version.
Unlike large Zettelkasten frameworks, **small-notes** does not try to manage your thinking — it only helps you capture, connect, and refactor notes efficiently.

---

## 🔗 Discovering chains of thought

**small-notes** includes a unique command for exploring backlink sequences — long chains of interconnected notes that trace the development of an idea across time.
You can run this with:
```
M-x small-notes-show-backlink-sequences
```
It scans your notes and finds sequences of backlinks longer than a user-specified minimum length, presenting them as clickable Org lists.
This lets you see how ideas evolved — from one note to another, across branches of your thinking — without maintaining an artificial index.
It’s a way to surface organic connections in your Zettelkasten, helping you rediscover forgotten lines of reasoning and write from the structure of your own thought.

---

## 📸 Screenshots

(Screenshots not yet updated for the latest version.)

<p align="center"> <img src="https://github.com/user-attachments/assets/5a42015e-0ebf-4a9b-bd8e-1a6a013db806"/> </p>

---

## 🪴 A note on compatibility

**small-notes** works purely with Org mode and requires no external packages beyond the Emacs standard library.
It is compatible with any Org-based Zettelkasten or writing setup.
