#!/usr/bin/env bash
# stack.sh — stacked PR helper with smart defaults, guard rails, status/ship
#
# Commands:
#   create [<BASE>] [<PREFIX>]       Create <PREFIX>-N branches for commits on BASE..HEAD (append-only); saves prefix
#   rebase [<BASE>]                  Rebase current branch onto origin/BASE; auto-clean merged branches
#   push [<PREFIX>]                  Delete merged <PREFIX>-* branches, push remaining with --force-with-lease
#   pr [<PREFIX>] [<BASE>] [--ready] Push & create/update PRs stacked on BASE → each other (draft by default)
#   list [<BASE>]                    Show commits BASE..HEAD in stack order
#   status [<BASE>] [<PREFIX>]       Show stack branches, PRs, bases, CI summaries
#   ship [<PREFIX>]                  Enable auto-merge (squash) for the bottom PR in the stack
#   prefix (show|set <P>|clear)      Manage remembered prefix
#
# Defaults:
#   - BASE defaults to origin's default branch (usually 'main'); 'origin/<BASE>' is accepted
#   - PREFIX defaults to: explicit arg > inferred from current branch (…-[0-9]+) > git config stack.prefix
set -euo pipefail

die() { echo "error: $*" >&2; exit 1; }
warn() { echo "warn: $*" >&2; }

need_clean() {
  if ! git diff --quiet || ! git diff --cached --quiet; then
    die "working tree not clean; commit/stash first"
  fi
}

ensure_not_detached() {
  local cur
  cur=$(git branch --show-current || true)
  [[ -n "$cur" ]] || die "detached HEAD; checkout a branch"
}

ensure_origin_github_if_needed() {
  # Only warn; don't hard-fail. PR/ship/status need GitHub CLI + GitHub remote.
  local url
  url=$(git remote get-url origin 2>/dev/null || true)
  if [[ -z "$url" ]]; then warn "origin remote not found"; return; fi
  if [[ ! "$url" =~ github.com[:/].+/.+ ]]; then
    warn "origin does not look like GitHub (${url}); PR/ship/status may fail"
  fi
}

default_remote_base() {
  git symbolic-ref refs/remotes/origin/HEAD 2>/dev/null | sed 's@^refs/remotes/origin/@@' || echo main
}

normalize_base() { local b="${1#origin/}"; echo "${b}"; }

# ----- Prefix helpers -----
get_prefix_from_config() { git config --get stack.prefix || true; }

get_prefix_from_current() {
  local cur; cur=$(git branch --show-current || true)
  if [[ "$cur" =~ -[0-9]+$ ]]; then
    echo "${cur%-[0-9]*}"
  else
    echo ""
  fi
}

get_prefix() {
  local explicit="${1:-}"
  if [[ -n "$explicit" ]]; then echo "$explicit"; return; fi
  local inferred; inferred=$(get_prefix_from_current)
  if [[ -n "$inferred" ]]; then echo "$inferred"; return; fi
  local cfg; cfg=$(get_prefix_from_config)
  if [[ -n "$cfg" ]]; then echo "$cfg"; return; fi
  die "No prefix found. Pass one (e.g. 'milind/my-feature') or set:  git config stack.prefix <prefix>"
}

# ----- Core helpers -----
revlist_rev() { git rev-list --reverse "$1"..HEAD; }

branches_for_prefix() {
  git for-each-ref --format='%(refname:short)' "refs/heads/$1-*" | sort -V
}

is_merged_patch() {
  local branch="$1" base="$2"
  git cherry "$base" "$branch" | grep -q '^-' && return 0 || return 1
}

cleanup_merged_branches() {
  local prefix="$1" base="$2"
  local deleted_any=false
  for br in $(branches_for_prefix "$prefix"); do
    if is_merged_patch "$br" "$base"; then
      echo "🗑  $br already merged into $base (patch match) — deleting..."
      git branch -D "$br"
      if git ls-remote --exit-code --heads origin "$br" >/dev/null 2>&1; then
        git push origin --delete "$br"
      else
        echo "   (remote already gone)"
      fi
      deleted_any=true
    fi
  done
  git fetch -p >/dev/null 2>&1 || true
  $deleted_any && echo "Cleanup complete." || echo "No merged branches to clean."
}

commit_subject() { git log -1 --pretty=%s "$1"; }
commit_body() { git log -1 --pretty=%b "$1"; }

ensure_gh() {
  command -v gh >/dev/null 2>&1 || die "Install GitHub CLI: https://cli.github.com/"
  gh auth status >/dev/null 2>&1 || die "gh not authenticated. Run: gh auth login"
}

# Guard rail: warn if a branch has != 1 commit vs its intended base
warn_if_not_one_commit() {
  local prev_base="$1"; shift
  for br in "$@"; do
    local counts left right
    counts=$(git rev-list --left-right --count "origin/$prev_base...$br" 2>/dev/null || echo "0 0")
    read -r left right <<< "$counts"
    if [[ "${right:-0}" -ne 1 ]]; then
      warn "branch '$br' has $right commits vs base '$prev_base' (expected 1)"
    fi
    prev_base="$br"
  done
}

cmd=${1:-}

case "$cmd" in
  create)
    # create [<BASE>] [<PREFIX>]
    ensure_not_detached
    BASE=$(normalize_base "${2:-$(default_remote_base)}")
    PREFIX=$(get_prefix "${3:-}")
    git rev-parse --verify "$BASE" >/dev/null 2>&1 || die "base branch '$BASE' not found"
    need_clean

    # Fetch base but DO NOT switch branches
    if git ls-remote --exit-code --heads origin "$BASE" >/dev/null 2>&1; then
      git fetch origin "$BASE"
      base_target="origin/$BASE"
    else
      base_target="$BASE"
    fi

    # Commits to stack are from base_target..HEAD on your CURRENT branch
    commits_all=$(git rev-list --reverse "$base_target"..HEAD)
    [[ -n "${commits_all}" ]] || die "no commits to stack (nothing on $BASE..HEAD) — are you on your feature branch?"

    # Smart update: compare existing branches with current stack and update as needed
    existing_branches=$(branches_for_prefix "$PREFIX" || true)
    
    # Convert commits to array for easier processing
    readarray -t commits_array <<< "$commits_all"
    
    i=1
    updated_count=0
    created_count=0
    
    for commit in "${commits_array[@]}"; do
      [[ -z "$commit" ]] && continue
      br="$PREFIX-$i"
      
      if git rev-parse --verify "$br" >/dev/null 2>&1; then
        # Branch exists, check if commit has changed
        existing_commit=$(git rev-parse "$br")
        if [[ "$existing_commit" != "$commit" ]]; then
          git branch -f "$br" "$commit"
          echo "updated: $br -> $(git rev-parse --short "$commit") (was $(git rev-parse --short "$existing_commit"))"
          updated_count=$((updated_count + 1))
        else
          echo "unchanged: $br -> $(git rev-parse --short "$commit")"
        fi
      else
        # Branch doesn't exist, create it
        git branch -f "$br" "$commit"
        echo "created: $br -> $(git rev-parse --short "$commit")"
        created_count=$((created_count + 1))
      fi
      
      i=$((i+1))
    done

    # Save prefix for convenience next time
    git config stack.prefix "$PREFIX" >/dev/null 2>&1 || true

    if [[ $updated_count -eq 0 && $created_count -eq 0 ]]; then
      echo "No changes to stack."
    else
      summary=""
      [[ $created_count -gt 0 ]] && summary="$created_count created"
      [[ $updated_count -gt 0 ]] && [[ -n "$summary" ]] && summary="$summary, "
      [[ $updated_count -gt 0 ]] && summary="${summary}${updated_count} updated"
      echo "Done. $summary. Use './stack.sh pr' to publish PRs."
    fi
    ;;

  rebase)
    # rebase [<BASE>]
    ensure_not_detached
    BASE=$(normalize_base "${2:-$(default_remote_base)}")
    need_clean
    if git ls-remote --exit-code --heads origin "$BASE" >/dev/null 2>&1; then
      git fetch origin "$BASE"; target="origin/$BASE"
    else
      target="$BASE"
    fi
    git config --local rebase.updateRefs true >/dev/null
    echo "Rebasing current branch onto $target with --update-refs…"
    git rebase --update-refs "$target"
    cur=$(git branch --show-current || true)
    if [[ "$cur" =~ -[0-9]+$ ]]; then
      cleanup_merged_branches "${cur%-[0-9]*}" "$target"
    fi
    ;;

  push)
    # push [<PREFIX>]
    ensure_not_detached
    PREFIX=$(get_prefix "${2:-}")
    BASE_REMOTE=$(default_remote_base)
    git fetch origin "$BASE_REMOTE" >/dev/null 2>&1 || true
    cleanup_merged_branches "$PREFIX" "origin/$BASE_REMOTE"

    # Guard rail: warn if not one commit per PR (vs stacking base chain)
    mapfile -t brs < <(branches_for_prefix "$PREFIX")
    if ((${#brs[@]})); then
      warn_if_not_one_commit "$BASE_REMOTE" "${brs[@]}"
    fi

    any=false
    for branch in "${brs[@]}"; do
      any=true
      echo "pushing: $branch"
      git push --force-with-lease origin "$branch"
    done
    $any && echo "All pushed with --force-with-lease." || echo "No branches to push for prefix '$PREFIX'."
    ;;

  pr)
    # pr [<PREFIX>] [<BASE>] [--ready]
    ensure_not_detached
    READY=false
    params=()
    for a in "${@:2}"; do
      case "$a" in
        --ready) READY=true ;;
        *) params+=("$a") ;;
      esac
    done
    PREFIX=$(get_prefix "${params[0]:-}")
    BASE=$(normalize_base "${params[1]:-$(default_remote_base)}")

    ensure_gh; ensure_origin_github_if_needed
    if git ls-remote --exit-code --heads origin "$BASE" >/dev/null 2>&1; then
      git fetch origin "$BASE"
    else
      die "base branch '$BASE' not found on origin"
    fi

    bash "$0" push "$PREFIX"   # also runs cleanup + guard rails

    prev_base="$BASE"
    for br in $(branches_for_prefix "$PREFIX"); do
      counts=$(git rev-list --left-right --count "origin/$prev_base...$br" || echo "0 0")
      read -r left right <<< "$counts"
      if [[ "${right:-0}" -eq 0 ]]; then
        echo "⏭  skipping $br: no commits vs $prev_base"
        prev_base="$br"; continue
      fi
      if [[ "${right:-0}" -ne 1 ]]; then
        warn "PR '$br' has $right commits vs base '$prev_base' (expected 1)"
      fi

      title=$(commit_subject "$br"); body=$(commit_body "$br")
      echo "PR for $br → base: $prev_base"
      if gh pr view "$br" >/dev/null 2>&1; then
        echo "  updating existing PR…"
        if $READY; then
          gh pr edit "$br" --title "$title" --body "$body" --base "$prev_base" --ready
        else
          gh pr edit "$br" --title "$title" --body "$body" --base "$prev_base" --draft
        fi
      else
        echo "  creating $( $READY && echo 'ready' || echo 'draft') PR…"
        if $READY; then
          gh pr create --head "$br" --base "$prev_base" --title "$title" --body "$body"
        else
          gh pr create --head "$br" --base "$prev_base" --title "$title" --body "$body" --draft
        fi
      fi
      prev_base="$br"
    done
    echo "PRs created/updated."
    ;;

  status)
    # status [<BASE>] [<PREFIX>]
    ensure_not_detached
    BASE=$(normalize_base "${2:-$(default_remote_base)}")
    PREFIX=$(get_prefix "${3:-}")
    ensure_origin_github_if_needed
    echo "Stack branches for prefix '$PREFIX' (base: $BASE):"
    mapfile -t brs < <(branches_for_prefix "$PREFIX")
    if ((${#brs[@]}==0)); then
      echo "  (no local branches matching $PREFIX-*)"
      exit 0
    fi

    ensure_gh || true
    for br in "${brs[@]}"; do
      # Try to get PR info; don't fail if gh unavailable
      pr_url=$(gh pr view "$br" --json url -q .url 2>/dev/null || echo "-")
      base_ref=$(gh pr view "$br" --json baseRefName -q .baseRefName 2>/dev/null || echo "-")
      ci=$(gh pr checks "$br" 2>/dev/null | awk 'NR>1 {print $1 ":" $2}' | paste -sd',' - || echo "-")
      base_show=${base_ref:-$BASE}
      behind_ahead=$(git rev-list --left-right --count "origin/$base_show...$br" 2>/dev/null || echo "0 0")
      read -r behind ahead <<< "$behind_ahead"
      delta_formatted="-${behind}/+${ahead}"
      echo "  $br  base:$base_show  PR:${pr_url}  Δ:${delta_formatted}  CI:${ci}"
    done
    ;;

  ship)
    # ship [<PREFIX>] — auto-merge (squash) bottom PR
    ensure_not_detached
    PREFIX=$(get_prefix "${2:-}")
    ensure_gh; ensure_origin_github_if_needed
    first=$(branches_for_prefix "$PREFIX" | head -n1 || true)
    [[ -n "$first" ]] || die "no branches for prefix '$PREFIX'"
    echo "Enabling auto-merge (squash) for PR of $first..."
    gh pr merge "$first" --squash --auto
    echo "Auto-merge enabled. After it lands: ./stack.sh rebase && ./stack.sh pr"
    ;;

  list)
    # list [<BASE>]
    BASE=$(normalize_base "${2:-$(default_remote_base)}")
    git rev-parse --verify "$BASE" >/dev/null 2>&1 || die "base branch '$BASE' not found"
    echo "Stack (oldest → newest) for $BASE..HEAD:"
    revlist_rev "$BASE" | while read -r c; do
      printf "%s  %s\n" "$(git rev-parse --short "$c")" "$(git show -s --format='%s' "$c")"
    done
    ;;

  prefix)
    # prefix show | set <P> | clear
    sub=${2:-}
    case "$sub" in
      show)
        val=$(get_prefix_from_config)
        [[ -n "$val" ]] && echo "$val" || echo "(no stored prefix)"
        ;;
      set)
        [[ -n "${3:-}" ]] || die "usage: ./stack.sh prefix set <prefix>"
        git config stack.prefix "$3"
        echo "prefix set to '$3'"
        ;;
      clear)
        git config --unset stack.prefix || true
        echo "prefix cleared"
        ;;
      *)
        die "usage: ./stack.sh prefix (show|set <prefix>|clear)"
        ;;
    esac
    ;;

  *)
    cat <<'USAGE'
stack.sh — commands:
  create [<BASE>] [<PREFIX>]       Create <PREFIX>-N branches for commits on BASE..HEAD (append-only); saves prefix
  rebase [<BASE>]                  Rebase current branch onto origin/BASE, update refs, and clean merged branches
  push [<PREFIX>]                  Delete merged <PREFIX>-* branches, push remaining with --force-with-lease
  pr [<PREFIX>] [<BASE>] [--ready] Push & create/update PRs stacked on BASE, then each other (draft by default)
  list [<BASE>]                    Show commits BASE..HEAD in stack order
  status [<BASE>] [<PREFIX>]       Show stack branches, PRs, bases, CI summaries
  ship [<PREFIX>]                  Enable auto-merge (squash) for the bottom PR in the stack
  prefix (show|set <P>|clear)      Manage remembered prefix

Defaults:
  - BASE defaults to origin's default branch (usually 'main'); 'origin/<BASE>' is accepted
  - PREFIX defaults to explicit arg > inferred from current branch (…-[0-9]+) > git config stack.prefix
USAGE
    exit 1
    ;;
esac
