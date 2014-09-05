# Configure version of Java used.
if [[ "$OSTYPE" == darwin* ]]; then
  java_use() {
    export JAVA_HOME=$(/usr/libexec/java_home -v $1)
  }
fi

# Generalized diff.
diff() {
  if (( $+commands[colordiff] )); then
    command diff --unified "$@" | colordiff --difftype diffu
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

  # Clean up previous $python_info.
  unset python_info
  typeset -gA python_info

  # Format virtualenv.
  if [[ -n "$VIRTUAL_ENV" ]]; then
    zstyle -s ':prompt:virtualenv' format 'virtualenv_format'
    zformat -f virtualenv_formatted "$virtualenv_format" "v:${VIRTUAL_ENV:t}"
    python_info[virtualenv]="$virtualenv_formatted"
  fi
}