#!/bin/bash
# From https://github.com/catppuccin/tmux/issues/426#issuecomment-2770107301

set -euo pipefail

CATPPUCCIN_PLUGIN_DIR="$HOME/.tmux/catppuccin"

status_dir="$CATPPUCCIN_PLUGIN_DIR/status"
status_utils_file="$CATPPUCCIN_PLUGIN_DIR/utils/status_module.conf"

# Custom status modules that are not part of base cappuccin distribution
custom_status_dir="$HOME/.tmux/status"

# Collect all theme options (beginning with '@') to unset
read_theme_options() {
  local file_pattern="$1"
  local module_name="${2:-}"

  rg -Io 'set\s+-[aFgopqsuUw]+\s+"?@([^\s]+(\w|_|\}))"?' -r '@$1' \
    $file_pattern | while IFS= read -r line; do
    # If module_name is passed in, replace it in matched lines, else skip
    if [[ "$line" == *'${MODULE_NAME}'* ]]; then
      if [[ -n "$module_name" ]]; then
        echo "$line" | sed "s/\${MODULE_NAME}/$module_name/g"
      fi
    else
      echo "$line"
    fi
  done
}

collect_theme_options() {
  while IFS= read -r option; do
    all_options+=("$option")
  done < <(read_theme_options "$@")
}

# Collect all options to unset
all_options=()

# Get options from all conf files
collect_theme_options "$CATPPUCCIN_PLUGIN_DIR/**/*.conf"

# Get all status modules in status_dir and custom_status_dir
filepaths=()
for filepath in "$status_dir"/*.conf "$custom_status_dir"/*.conf; do
  [ -e "$filepath" ] || continue
  filepaths+=("$filepath")
done

# Collect options from status module files
for conf_file in "${filepaths[@]}"; do
  module="$(basename "$conf_file" .conf)"

  collect_theme_options "$conf_file" "$module"
  collect_theme_options "$status_utils_file" "$module"
done

# Unset all collected options
printf '%s\n' "${all_options[@]}" | sort -u | \
  xargs -n1 -P0 tmux set -Ugq
