# .emacs.d
My .emacs.d configurations.

## why Emacs? and in which situations should i use it?
my main concern are in my workflow are:
- efficiency
- speed

So i don't browse web, watch videos, read pdf books, see images with emacs. because they cause lags and slowness. if one day i found quick solutions, i'll definitely switch.
for now i'm still using these tools outside of emacs:
- brave --> browser
- mpv --> watch videos, listen to musics
- evince, foliate --> read books
- nomacs --> see images

But i'll definietly use it as:
- IDE
- editor
- file manager(dired)
- second brain(org-mode, org-roam)
- calculator(elisp, calc)

I still not sure should i use terminal inside emacs or not!

and maybe i use emacs in these domains also:
- as a mail client
- as torrent manager
- git client

## keybindings
- C-l r: restart emacs
- C-q: kill buffer.
- C-k: kill line.
- C-b: consult-buffer
  switch to buffers, bookmarks, etc..
- C-z: undo
- C-g C-z: redo
- C-a: beginning of line.
- C-e: endo of line.

- C-h ENTER: major topics manual.
- C-h b: describe-binding
  show all the keybindings and you can search through them with swiper)
- C-h C-h: help-for-help
  shows high-level help page.
- C-h C-q: help-quick-toggle
  shows most used keybidings.
- C-h k: describe-key
- C-h v: describe-variable

- C-x b: consult-buffer
  list all buffers, file, etc.
- C-x d: dired
  file-manager of emacs!
- C-x 0: only other.
  full screen other buffer.
- C-x 1: only this
  full screen current buffer.

- M-x eval-buffer: reload current buffer

### dired
- ^ or Shift + 6 --> jump back to parent directory
- +: create directory
- C-x C-f: create file
- g: refresh buffer
- d: mark for deletion
- D: delete marked items.
Notice that if you set this variable:
`(setq delete-by-moving-to-trash t)`
Dired, will put your file in ~/.local/share/Trash

### potential to change
- C-t: transpose-char

### Q&A
Q: how jump to the beginning of the file?
A: C-l b
  (global-unset-key (kbd "C-l"))
  (global-set-key (kbd "C-l b") 'beginning-of-buffer)

Q: how jump to the end of the file?
A: C-l e
  (global-unset-key (kbd "C-l"))
  (global-set-key (kbd "C-l e") 'end-of-buffer)

Q: how duplicate a line?
A: C-l d
  (global-set-key (kbd "C-l d") 'duplicate-dwim)

Q: how duplicate a block of lines?
A: C-l d
  (global-set-key (kbd "C-l d") 'duplicate-dwim)

Q: how bookmark files and directories?
A: create: C-x r m
  list: C-x r l
  delete: C-x r d
  help: C-h r

Q: how unzip zip/rar files?
A:

Q: how find/replace interactively?
A:

## License
![License](https://img.shields.io/github/license/LinArcX/.emacs.d.svg)
