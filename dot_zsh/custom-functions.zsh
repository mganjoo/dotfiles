
fpath=(~/.zsh/functions(-/FN) $fpath)

function is-true {
  [[ -n "$1" && "$1" == (1|[Yy]([Ee][Ss]|)|[Tt]([Rr][Uu][Ee]|)|[Oo]([Nn]|)) ]]
}

# Autoload all functions in the directory
# (exclude dotfiles and temp files)
local function_glob='^([.]*|*~)(-.N:t)'
for pfunction in ~/.zsh/functions/$~function_glob; do
  autoload -Uz "$pfunction"
done
