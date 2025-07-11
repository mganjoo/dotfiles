# vim: foldmethod=marker:fen

# The following script is run only in interactive terminals.
if [[ "$TERM" == 'dumb' ]]; then
  return
fi

interactive_modules=(
  completion
  editor
  # BEGIN: the next three plugins must be loaded in order!
  syntax-highlighting
  history-substring-search
  autosuggestions
  # END
  fzf
  apps
  prompt
  tmux
)

for m in $interactive_modules; do
  source "$HOME/.zsh/${m}.zsh"
done
