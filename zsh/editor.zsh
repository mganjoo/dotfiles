# == Prologue == {{{1

# Ignore this file on dumb terminals.
if [[ "$TERM" == 'dumb' || ! $- =~ i ]]; then
  return
fi

# Load terminfo to get key codes for special keys
zmodload zsh/terminfo

# Enable command line editing through an external editor.
autoload -Uz edit-command-line
zle -N edit-command-line

# Activate url-quote-magic on all entries.
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

# == editor-info and update to default ZLE widgets == {{{1

# Set indicators depending on the current editor state.
# We insert a call to editor-info in all the basic widgets.
function editor-info {
  unset editor_info
  typeset -gA editor_info

  if [[ "$KEYMAP" == 'vicmd' ]]; then
    zstyle -s ':prompt:indicator:keymap:command' format 'REPLY'
    editor_info[keymap]="$REPLY"
  else
    zstyle -s ':prompt:indicator:keymap:insert' format 'REPLY'
    editor_info[keymap]="$REPLY"
  fi
  unset REPLY

  # Cause prompt to be redisplayed using new styles.
  zle reset-prompt
  zle -R
}
zle -N editor-info

zle-keymap-select() {
  zle editor-info
}
zle -N zle-keymap-select

zle-line-init() {
  echoti smkx
  zle editor-info
}
zle -N zle-line-init

zle-line-finish() {
  echoti rmkx
  zle editor-info
}
zle -N zle-line-finish

overwrite-mode() {
  zle .overwrite-mode
  zle editor-info
}
zle -N overwrite-mode

vi-insert() {
  zle .vi-insert
  zle editor-info
}
zle -N vi-insert

vi-insert-bol() {
  zle .vi-insert-bol
  zle editor-info
}
zle -N vi-insert-bol

vi-replace() {
  zle .vi-replace
  zle editor-info
}
zle -N vi-replace

# == Custom widgets == {{{1

# Expand ... to ../..
expand-dot-to-parent-directory-path() {
  if [[ $LBUFFER = *.. ]]; then
    LBUFFER+='/..'
  else
    LBUFFER+='.'
  fi
}
zle -N expand-dot-to-parent-directory-path

# Displays an indicator when completing.
expand-or-complete-with-indicator() {
  local indicator
  zstyle -s ':editor:info:completing' format 'indicator'
  print -Pn "$indicator"
  zle expand-or-complete
  zle redisplay
}
zle -N expand-or-complete-with-indicator

# Inserts 'sudo ' at the beginning of the line.
prepend-sudo() {
  if [[ "$BUFFER" != su(do|)\ * ]]; then
    BUFFER="sudo $BUFFER"
    (( CURSOR += 5 ))
  fi
}
zle -N prepend-sudo

# Inserts 'echo ' at the beginning of the line.
prepend-echo() {
  if [[ "$BUFFER" != echo\ * ]]; then
    BUFFER="echo $BUFFER"
    (( CURSOR += 5 ))
  fi
}
zle -N prepend-echo

# Branch widget.
__bsel() {
  git for-each-ref --format='%(refname:short)' refs/heads/ 2>/dev/null | fzf -m | tr "\n" " "
}
fzf-branch-widget() {
  LBUFFER="${LBUFFER}$(__bsel)"
  zle redisplay
}
zle -N fzf-branch-widget

zmodload -i zsh/parameter
insert-last-command-output() {
  LBUFFER+="$(eval $history[$((HISTCMD-1))])"
}
zle -N insert-last-command-output

pb-yank-whole-line() {
  zle vi-yank-whole-line
  print -rn $CUTBUFFER | pbcopy
}
zle -N pb-yank-whole-line

# == Key bindings == {{{1

bindkey -d  # Reset to default key bindings.
bindkey -v  # Set to vi mode.

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

# Copy line
bindkey -M vicmd 'Y' pb-yank-whole-line

# Paste the selected branches into the command line
bindkey -M viins '^b' fzf-branch-widget

# Output of last command.
bindkey -M viins "^x^l" insert-last-command-output

# run-help
bindkey -M viins '^x^h' run-help

# == Epilogue == {{{1
# vim: fdm=marker
