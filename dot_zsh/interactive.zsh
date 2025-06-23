# vim: foldmethod=marker:fen
# Settings for interactive features (not for dumb terminals)

# == Guard == {{{1
if [[ "$TERM" == 'dumb' || ! $- =~ i  ]]; then
  return
fi

# == Load interactive modules == {{{1
source ~/.zsh/completion.zsh
source ~/.zsh/keybindings.zsh
source ~/.zsh/plugins.zsh
source ~/.zsh/prompt.zsh
