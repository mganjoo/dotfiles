#!/bin/bash
# From https://github.com/catppuccin/tmux/issues/426#issuecomment-2770107301

set -euo pipefail

CATPPUCCIN_PLUGIN_DIR="$HOME/.tmux/catppuccin"

status_dir="$CATPPUCCIN_PLUGIN_DIR/status"
status_utils_file="$CATPPUCCIN_PLUGIN_DIR/utils/status_module.conf"

# Unset all theme options (beginning with '@') so we can set them again
unset_theme_options() {
  local file_pattern="$1"
  local module_name="${2:-}"

  rg -Io 'set\s+-[aFgopqsuUw]+\s+"?@([^\s]+(\w|_))"?' -r '@$1' \
    $file_pattern | while IFS= read -r line; do
    # If module_name is passed in, replace it in matched lines, else skip
    if [[ "$line" == *'${MODULE_NAME}'* ]]; then
      if [[ -n "$module_name" ]]; then
        echo "$line" | sed "s/\${MODULE_NAME}/$module_name/g"
      fi
    else
      echo "$line"
    fi
  done | uniq | xargs -n1 -P0 tmux set -Ugq
}

unset_theme_options "$CATPPUCCIN_PLUGIN_DIR/**/*.conf"

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

  unset_theme_options "$conf_file" "$module"
  unset_theme_options "$status_utils_file" "$module"
done
