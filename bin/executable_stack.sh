#!/usr/bin/env bash
# stack.sh — stacked PR helper with GitHub PR automation
#
# Commands:
#   create <BASE> <PREFIX>        Create <PREFIX>-N branches for commits on BASE..HEAD (append-only)
#   rebase <BASE>                 Rebase current branch onto origin/BASE and update refs; auto-clean merged branches
#   push <PREFIX>                 Delete merged <PREFIX>-* branches, push remaining with --force-with-lease
#   pr <PREFIX> <BASE> [--ready]  Push & create/update PRs stacked on <BASE> → each other (draft by default)
#   list <BASE>                   Show commits BASE..HEAD in stack order
#
# Example flow:
#   git checkout -b feature main
#   # make one-commit-per-PR commits...
#   ./stack.sh create main myfeat
#   ./stack.sh pr myfeat main            # draft PRs
#   ./stack.sh pr myfeat main --ready    # flip to ready-for-review
#   # after feedback / fixes:
#   ./stack.sh rebase main
#   ./stack.sh pr myfeat main

set -euo pipefail

die() { echo "error: $*" >&2; exit 1; }
need_clean() {
  if ! git diff --quiet || ! git diff --cached --quiet; then
    die "working tree not clean; commit/stash first"
  fi
}

revlist_rev() { git rev-list --reverse "$1"..HEAD; }

branches_for_prefix() {
  # List local branches matching PREFIX-*, sorted naturally by name
  git for-each-ref --format='%(refname:short)' --sort=refname "refs/heads/$1-*"
}

is_merged_patch() {
  local branch="$1" base="$2"
  # '-' means the patch from branch exists in base (git cherry compares by patch-id)
  git cherry "$base" "$branch" | grep -q '^-' && return 0 || return 1
}

cleanup_merged_branches() {
  local prefix="$1" base="$2"
  local deleted_any=false
  for br in $(branches_for_prefix "$prefix"); do
    if is_merged_patch "$br" "$base"; then
      echo "🗑  $br already merged into $base (patch match) — deleting..."
      git branch -D "$br"
      git push origin --delete "$br" || true
      deleted_any=true
    fi
  done
  $deleted_any && echo "Cleanup complete." || echo "No merged branches to clean."
}

commit_subject() { git log -1 --pretty=%s "$1"; }
commit_body() { git log -1 --pretty=%b "$1"; }

ensure_gh() {
  command -v gh >/dev/null 2>&1 || die "GitHub CLI (gh) not found. Install https://cli.github.com/"
  gh auth status >/dev/null 2>&1 || die "gh not authenticated. Run: gh auth login"
}

default_remote_base() {
  # origin's default branch name (main/master/etc)
  git symbolic-ref refs/remotes/origin/HEAD | sed 's@^refs/remotes/origin/@@'
}

cmd=${1:-}
case "$cmd" in
  create)
    # ./stack.sh create <BASE> <PREFIX>
    [[ $# -eq 3 ]] || die "usage: ./stack.sh create <BASE> <PREFIX>"
    BASE="$2"; PREFIX="$3"

    git rev-parse --verify "$BASE" >/dev/null 2>&1 || die "base branch '$BASE' not found"
    need_clean

    # Align local base with remote if present
    if git ls-remote --exit-code --heads origin "$BASE" >/dev/null 2>&1; then
      git fetch origin "$BASE"
      git checkout -B "$BASE" "origin/$BASE"
    fi

    commits_all=$(revlist_rev "$BASE")
    [[ -n "${commits_all}" ]] || die "no commits to stack (nothing on $BASE..HEAD)"

    # Append-only: count existing PREFIX-* branches and continue numbering
    existing_branches=$(branches_for_prefix "$PREFIX" || true)
    count_existing=$(echo "${existing_branches}" | grep -E "^$PREFIX-[0-9]+$" | wc -l | tr -d ' ')
    start=$((count_existing + 1))

    # Skip first <count_existing> commits; create branches for the rest
    i=$start
    echo "$commits_all" | tail -n +$((count_existing + 1)) | while read -r c; do
      [[ -z "$c" ]] && continue
      br="$PREFIX-$i"
      git branch -f "$br" "$c"
      echo "created: $br -> $(git rev-parse --short "$c")"
      i=$((i+1))
    done

    if [[ $i -eq $start ]]; then
      echo "No new commits to branch."
    else
      echo "Done. Use './stack.sh pr $PREFIX $BASE' to publish PRs."
    fi
    ;;

  rebase)
    # ./stack.sh rebase <BASE>
    [[ $# -eq 2 ]] || die "usage: ./stack.sh rebase <BASE>"
    BASE="$2"
    need_clean

    if git ls-remote --exit-code --heads origin "$BASE" >/dev/null 2>&1; then
      git fetch origin "$BASE"
      target="origin/$BASE"
    else
      target="$BASE"
    fi

    # Ensure update-refs for interactive or non-interactive rebase
    git config --local rebase.updateRefs true >/dev/null

    echo "Rebasing current branch onto $target with --update-refs…"
    git rebase --update-refs "$target"

    # Infer prefix from current branch (prefix-suffix)
    cur=$(git branch --show-current || true)
    prefix="${cur%-*}"
    [[ -n "${prefix}" && "${cur}" == "$prefix-"* ]] && cleanup_merged_branches "$prefix" "$target" || true
    ;;

  push)
    # ./stack.sh push <PREFIX>
    [[ $# -eq 2 ]] || die "usage: ./stack.sh push <PREFIX>"
    PREFIX="$2"

    BASE_REMOTE=$(default_remote_base || echo "main")
    if git ls-remote --exit-code --heads origin "$BASE_REMOTE" >/dev/null 2>&1; then
      git fetch origin "$BASE_REMOTE" >/dev/null 2>&1 || true
    fi

    cleanup_merged_branches "$PREFIX" "origin/$BASE_REMOTE"

    any=false
    for branch in $(branches_for_prefix "$PREFIX"); do
      any=true
      echo "pushing: $branch"
      git push --force-with-lease origin "$branch"
    done
    $any && echo "All pushed with --force-with-lease." || echo "No branches to push for prefix '$PREFIX'."
    ;;

  pr)
    # ./stack.sh pr <PREFIX> <BASE> [--ready]
    [[ $# -ge 3 && $# -le 4 ]] || die "usage: ./stack.sh pr <PREFIX> <BASE> [--ready]"
    PREFIX="$2"; BASE="$3"
    READY=false
    [[ "${4:-}" == "--ready" ]] && READY=true

    ensure_gh
    if git ls-remote --exit-code --heads origin "$BASE" >/dev/null 2>&1; then
      git fetch origin "$BASE"
    fi

    # Push all branches (and clean merged) first
    "$0" push "$PREFIX"

    prev_base="$BASE"
    for br in $(branches_for_prefix "$PREFIX"); do
      title=$(commit_subject "$br"); body=$(commit_body "$br")

      echo "PR for $br → base: $prev_base"
      if gh pr view --head "$br" >/dev/null 2>&1; then
        echo "  updating existing PR…"
        if $READY; then
          gh pr edit --head "$br" --title "$title" --body "$body" --base "$prev_base" --ready
        else
          gh pr edit --head "$br" --title "$title" --body "$body" --base "$prev_base" --draft
        fi
      else
        echo "  creating $( $READY && echo 'ready' || echo 'draft') PR…"
        if $READY; then
          gh pr create --head "$br" --base "$prev_base" --title "$title" --body "$body"
        else
          gh pr create --head "$br" --base "$prev_base" --title "$title" --body "$body" --draft
        fi
      fi

      prev_base="$br"  # next PR stacks on this branch
    done
    echo "PRs created/updated."
    ;;

  list)
    # ./stack.sh list <BASE>
    [[ $# -eq 2 ]] || die "usage: ./stack.sh list <BASE>"
    BASE="$2"
    git rev-parse --verify "$BASE" >/dev/null 2>&1 || die "base branch '$BASE' not found"
    echo "Stack (oldest → newest) for $BASE..HEAD:"
    revlist_rev "$BASE" | while read -r c; do
      printf "%s  %s\n" "$(git rev-parse --short "$c")" "$(git show -s --format='%s' "$c")"
    done
    ;;

  *)
    cat <<'USAGE'
stack.sh — commands:
  create <BASE> <PREFIX>        Create <PREFIX>-N branches for commits on BASE..HEAD (append-only)
  rebase <BASE>                 Rebase current branch onto origin/BASE, update refs, and clean merged branches
  push <PREFIX>                 Delete merged <PREFIX>-* branches, push remaining with --force-with-lease
  pr <PREFIX> <BASE> [--ready]  Push & create/update PRs stacked on <BASE>, then each other (draft by default)
  list <BASE>                   Show commits BASE..HEAD in stack order
USAGE
    exit 1
    ;;
esac
