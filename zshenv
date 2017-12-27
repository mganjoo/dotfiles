# == Settings == {{{1

# History configuration

HISTFILE="$HOME/.zhistory"  # Path to the history file.
HISTSIZE=10000              # Max number of events to store in session.
SAVEHIST=10000              # Max number of history events to save.

# Editor and viewers

# / and = are not considered part of a word.
WORDCHARS='*?_-.[]~&;!#$%^(){}<>'

export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'
export LESS='-F -g -i -M -R -S -w -X -z-4'

# == Functions == {{{1

# Configure version of Java used.
if [[ "$OSTYPE" == darwin* ]]; then
  java_use() {
    export JAVA_HOME=$(/usr/libexec/java_home -v $1)
  }
fi

# Generalized diff.
diff() {
  if (( $+commands[colordiff] )); then
    command diff --unified "$@" | colordiff
  elif (( $+commands[git] )); then
    git --no-pager diff --color=auto --no-ext-diff --no-index "$@"
  else
    command diff --unified "$@"
  fi
}

# Word-based diff.
wdiff() {
  if (( $+commands[wdiff] )); then
    command wdiff \
      --avoid-wraps \
      --start-delete="$(print -n $FG[red])" \
      --end-delete="$(print -n $FG[none])" \
      --start-insert="$(print -n $FG[green])" \
      --end-insert="$(print -n $FG[none])" \
      "$@" \
      | sed 's/^\(@@\( [+-][[:digit:]]*,[[:digit:]]*\)\{2\} @@\)$/;5;6m\10m/g'
  elif (( $+commands[git] )); then
    git --no-pager diff --color=auto --no-ext-diff --no-index --color-words "$@"
  else
    command wdiff "$@"
  fi
}

# Python information.
python-info() {
  local virtualenv_format
  local virtualenv_formatted
  local condaenv_format
  local condaenv_formatted

  # Clean up previous $python_info.
  unset python_info
  typeset -gA python_info

  # Format virtualenv and conda environment.
  if [[ -n "$VIRTUAL_ENV" ]]; then
    zstyle -s ':prompt:virtualenv' format 'virtualenv_format'
    zformat -f virtualenv_formatted "$virtualenv_format" "v:${VIRTUAL_ENV:t}"
    python_info[virtualenv]="$virtualenv_formatted"
  fi

  if [[ -n "$CONDA_PREFIX" ]]; then
    zstyle -s ':prompt:condaenv' format 'condaenv_format'
    zformat -f condaenv_formatted "$condaenv_format" "v:${CONDA_PREFIX:t}"
    python_info[condaenv]="$condaenv_formatted"
  fi
}

# Pager for jq.
jqp() {
  jq -C "$@" | less -R
}

# Format stdin as JSON.
json_escape() {
  python -c 'import json, sys; print(json.dumps(sys.stdin.read()))'
}

# == zmv == {{{1

autoload -U zmv

# == Secrets == {{{1

if [ -e ~/.secrets ]; then
  source ~/.secrets
fi

# == Epilogue == {{{1
# vim: fdm=marker
