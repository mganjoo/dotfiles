#!/bin/bash
# From https://github.com/catppuccin/tmux/issues/426#issuecomment-2770107301

set -euo pipefail

CATPPUCCIN_PLUGIN_DIR="$HOME/.tmux/plugins/catppuccin"

# Unset all catppuccin options (tmux options that begin with @)
rg -Io 'set\s+-[aFgopqsuUw]+\s+"?@([^\s]+(\w|_))"?' -r '@$1' $CATPPUCCIN_PLUGIN_DIR/**/*.conf | uniq | xargs -n1 -P0 tmux set -Ugq

status_dir="$CATPPUCCIN_PLUGIN_DIR/status"
status_utils_file="$CATPPUCCIN_PLUGIN_DIR/utils/status_module.conf"

# Get all status modules in status_dir
modules=()
for filepath in "$status_dir"/*.conf; do
  [ -e "$filepath" ] || continue
  filename="$(basename "$filepath" .conf)"
  modules+=("$filename")
done

# Unset status module options for each module
for module in "${modules[@]}"; do
  conf_file="${status_dir}/${module}.conf"

  rg -Io 'set\s+-[aFgopqsuUw]+\s+"?@([^\s]+(\w|_))"?' -r '@$1' "$conf_file" | sed "s/\${MODULE_NAME}/$module/g" | uniq | xargs -n1 -P0 tmux set -Ugq
  rg -Io 'set\s+-[aFgopqsuUw]+\s+"?@([^\s]+(\w|_))"?' -r '@$1' "$status_utils_file" | sed "s/\${MODULE_NAME}/$module/g" | uniq | xargs -n1 -P0 tmux set -Ugq
done
