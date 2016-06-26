# == Pre-load user customizations ==
if [ -d ~/.zsh.before/ ]; then
  for config_file (~/.zsh.before/*.zsh); do
    source $config_file
  done
fi

if [ -d $HOME/bin/ ]; then
  path=(
    $HOME/bin
    $path
  )
fi

# == Modularized configuration ==

# Source large configuration modules.
for config_file (~/.zsh/*.zsh); do
  source $config_file
done

# Load file with sensitive information that shouldn't be checked in
if [ -e ~/.secrets ]; then
  source ~/.secrets
fi

# == Appearance ==

# Prompt.
fpath=( ~/.zsh/prompt $fpath )
autoload -Uz promptinit
promptinit
prompt 'mganjoo'

# Base 16 Shell configuration
BASE16_SHELL="$HOME/.external/base16-shell/base16-tomorrow.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

# Colors

RCol='\e[0m'    # Text Reset

# Regular           Bold                Underline           High Intensity      BoldHigh Intens     Background          High Intensity Backgrounds
Bla='\e[0;30m';     BBla='\e[1;30m';    UBla='\e[4;30m';    IBla='\e[0;90m';    BIBla='\e[1;90m';   On_Bla='\e[40m';    On_IBla='\e[0;100m';
Red='\e[0;31m';     BRed='\e[1;31m';    URed='\e[4;31m';    IRed='\e[0;91m';    BIRed='\e[1;91m';   On_Red='\e[41m';    On_IRed='\e[0;101m';
Gre='\e[0;32m';     BGre='\e[1;32m';    UGre='\e[4;32m';    IGre='\e[0;92m';    BIGre='\e[1;92m';   On_Gre='\e[42m';    On_IGre='\e[0;102m';
Yel='\e[0;33m';     BYel='\e[1;33m';    UYel='\e[4;33m';    IYel='\e[0;93m';    BIYel='\e[1;93m';   On_Yel='\e[43m';    On_IYel='\e[0;103m';
Blu='\e[0;34m';     BBlu='\e[1;34m';    UBlu='\e[4;34m';    IBlu='\e[0;94m';    BIBlu='\e[1;94m';   On_Blu='\e[44m';    On_IBlu='\e[0;104m';
Pur='\e[0;35m';     BPur='\e[1;35m';    UPur='\e[4;35m';    IPur='\e[0;95m';    BIPur='\e[1;95m';   On_Pur='\e[45m';    On_IPur='\e[0;105m';
Cya='\e[0;36m';     BCya='\e[1;36m';    UCya='\e[4;36m';    ICya='\e[0;96m';    BICya='\e[1;96m';   On_Cya='\e[46m';    On_ICya='\e[0;106m';
Whi='\e[0;37m';     BWhi='\e[1;37m';    UWhi='\e[4;37m';    IWhi='\e[0;97m';    BIWhi='\e[1;97m';   On_Whi='\e[47m';    On_IWhi='\e[0;107m';

# == Plugins ==

# virtualenvwrapper
if (( $+commands[virtualenvwrapper.sh] )); then
  export WORKON_HOME="$HOME/.virtualenvs"
  export PROJECT_HOME="$HOME/workspace"
  VIRTUAL_ENV_DISABLE_PROMPT=1
  source $commands[virtualenvwrapper.sh]
fi

# zmv
autoload -U zmv

# fasd
eval "$(fasd --init auto)"

# tmuxifier
export TMUXIFIER_LAYOUT_PATH="$HOME/.tmuxifier-layouts"
eval "$($HOME/.external/tmuxifier/bin/tmuxifier init -)"

# fzf
export FZF_TMUX=0
export FZF_CTRL_T_COMMAND='(git ls-files --exclude-standard -co ||
  find * -name ".*" -prune -o -type f -print -o -type l -print) 2>/dev/null'
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# ghc
export GHC_DOT_APP="/Applications/ghc-7.10.2.app"
if [ -d "$GHC_DOT_APP" ]; then
  export PATH="${HOME}/.local/bin:${HOME}/.cabal/bin:${GHC_DOT_APP}/Contents/bin:${PATH}"
fi

# run-help
autoload run-help
if command -v brew > /dev/null 2>&1; then
  HELPDIR=$(brew --prefix)/share/zsh/help
fi

# == Post-load user customizations ==
if [ -d ~/.zsh.after/ ]; then
  for config_file (~/.zsh.after/*.zsh); do
    source $config_file
  done
fi

# zsh-users plugins (order of loading matters)
source ~/.zsh/external/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.zsh/external/zsh-history-substring-search/zsh-history-substring-search.zsh

# History substring search (from zsh-history-substring-search plugin).
bindkey -M viins "^p" history-substring-search-up
bindkey -M viins "^n" history-substring-search-down
bindkey -M vicmd "k" history-substring-search-up
bindkey -M vicmd "j" history-substring-search-down
bindkey -M viins "$keycode[Up]" history-substring-search-up
bindkey -M viins "$keycode[Down]" history-substring-search-down

