# Setup fzf
# ---------

# Locate the directory holding fzf's shell-integration scripts. The path
# differs across package sources (Homebrew vs apt vs MacPorts vs vendored).
typeset _fzf_shell_dir=
for candidate in \
  /opt/homebrew/opt/fzf/shell \
  /usr/share/doc/fzf/examples \
  /usr/local/opt/fzf/shell ; do
  if [[ -f "$candidate/key-bindings.zsh" ]]; then
    _fzf_shell_dir=$candidate
    break
  fi
done

# Homebrew installs fzf's binary outside the default PATH; apt-installed fzf
# is already on PATH so this is a no-op there.
if [[ -d /opt/homebrew/opt/fzf/bin && ! "$PATH" == */opt/homebrew/opt/fzf/bin* ]]; then
  PATH="${PATH:+${PATH}:}/opt/homebrew/opt/fzf/bin"
fi

# Auto-completion
[[ -n $_fzf_shell_dir && $- == *i* ]] && source "$_fzf_shell_dir/completion.zsh" 2>/dev/null

# Key bindings
[[ -n $_fzf_shell_dir ]] && source "$_fzf_shell_dir/key-bindings.zsh"

unset _fzf_shell_dir
