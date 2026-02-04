
fpath=(~/.zsh/functions(-/FN) $fpath)

# Some common helper functions
function is-true {
  [[ -n "$1" && "$1" == (1|[Yy]([Ee][Ss]|)|[Tt]([Rr][Uu][Ee]|)|[Oo]([Nn]|)) ]]
}

function in_dir {
  pushd "$1" > /dev/null && eval "${@:2}" && popd > /dev/null
}

# Autoload all functions in the directory
# (exclude dotfiles and temp files)
local function_glob='^([.]*|*~)(-.N:t)'
for pfunction in ~/.zsh/functions/$~function_glob; do
  autoload -Uz "$pfunction"
done
