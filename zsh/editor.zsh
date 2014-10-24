# == Prologue == {{{1

# Ignore this file on dumb terminals.
if [[ "$TERM" == 'dumb' || ! $- =~ i ]]; then
  return
fi

# Load terminfo to get key codes for special keys
zmodload zsh/terminfo

# == Array of key mappings == {{{1

typeset -A keycode
keycode=(
  'Backspace' "^?"
  'Delete'    "^[[3~"
  'Home'      "$terminfo[khome]"
  'End'       "$terminfo[kend]"
  'Up'        "$terminfo[kcuu1]"
  'Left'      "$terminfo[kcub1]"
  'Down'      "$terminfo[kcud1]"
  'Right'     "$terminfo[kcuf1]"
)

# == Load widgets == {{{1

# Enable command line editing through an external editor.
autoload -Uz edit-command-line
zle -N edit-command-line

# Activate url-quote-magic on all entries.
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

# == Key bindings == {{{1

bindkey -d  # Reset to default key bindings.
bindkey -v  # Set to vi mode.

# Motion keys.
bindkey -M viins "$keycode[Home]" beginning-of-line
bindkey -M viins "$keycode[End]" end-of-line
bindkey -M viins '^a' beginning-of-line
bindkey -M viins '^e' end-of-line
bindkey -M viins "$keycode[Delete]" delete-char
bindkey -M viins "$keycode[Backspace]" backward-delete-char
bindkey -M viins "$keycode[Left]" backward-char
bindkey -M viins "$keycode[Right]" forward-char

# Kill till start of line (don't use annoying vi-equivalent).
bindkey -M viins '^u' backward-kill-line

# Expand history on space.
bindkey -M viins ' ' magic-space

# Expand command name to full path.
for key in '\e'{E,e}; do
  bindkey -M viins "$key" expand-cmd-path
done

# Duplicate the previous word.
for key in '\e'{M,m}; do
  bindkey -M viins "$key" copy-prev-shell-word
done

# Use a flexible push-line which works on multiple lines.
for key in '^q' '\e'{Q,q}; do
  bindkey -M viins "$key" push-line-or-edit
done

# Display an indicator when completing.
bindkey -M viins '^i' expand-or-complete-with-indicator

# Prepend commands at the beginning of the line.
bindkey -M viins '^x^s' prepend-sudo
bindkey -M viins '^x^o' prepend-echo

# Expand ... to ../.., but not during incremental search.
bindkey -M viins '.' expand-dot-to-parent-directory-path
bindkey -M isearch '.' self-insert

# Go up a directory (Esc-.)
bindkey -M viins -s '\e.' '..\n'

# Editing command line in external editor.
bindkey -M vicmd 'v' edit-command-line
bindkey -M viins '^x^e' edit-command-line

# Undo and redo.
bindkey -M vicmd 'u' undo
bindkey -M vicmd '^r' redo

# History search.
bindkey -M vicmd '?' history-incremental-pattern-search-backward
bindkey -M vicmd '/' history-incremental-pattern-search-forward
bindkey -M viins '^r' fzf-history-widget

# Paste the selected file path(s) into the command line
bindkey -M viins '^t' fzf-file-widget

# History substring search (from zsh-history-substring-search plugin).
bindkey -M viins "^p" history-substring-search-up
bindkey -M viins "^n" history-substring-search-down
bindkey -M vicmd "k" history-substring-search-up
bindkey -M vicmd "j" history-substring-search-down
bindkey -M viins "$keycode[Up]" history-substring-search-up
bindkey -M viins "$keycode[Down]" history-substring-search-down

# Output of last command.
bindkey -M viins "^x^l" insert-last-command-output

# run-help
bindkey -M viins '\eh' run-help

# == Epilogue == {{{1

# Unset the variables used in the file.
unset key keycode

# vim: fdm=marker
