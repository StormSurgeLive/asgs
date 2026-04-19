function _git_branch() {
  local top="$1"
  local b

  b=$(git -C "$top" symbolic-ref --quiet --short HEAD 2>/dev/null) && {
    printf '%s' "$b"
    return 0
  }

  b=$(git -C "$top" rev-parse --short HEAD 2>/dev/null) && {
    printf 'detached@%s' "$b"
    return 0
  }

  return 1
}

function _git_dirty() {
  local top="$1"

  if ! git -C "$top" diff --no-ext-diff --quiet --ignore-submodules --cached 2>/dev/null \
     || ! git -C "$top" diff --no-ext-diff --quiet --ignore-submodules 2>/dev/null \
     || [ -n "$(git -C "$top" ls-files --others --exclude-standard 2>/dev/null)" ]; then
    printf '*'
  fi
}

function _git_repo_label() {
  local top="$1"
  local remote url name

  remote=$(git -C "$top" remote 2>/dev/null | head -n1)

  if [ -n "$remote" ]; then
    url=$(git -C "$top" remote get-url "$remote" 2>/dev/null)
    name=$(basename "$url" .git)
    if [ -n "$name" ]; then
      printf '%s' "$name"
      return 0
    fi
  fi

  basename "$top"
}

function _version_from_file() {
  local dir="$PWD"

  while [ "$dir" != "/" ]; do
    if [ -f "$dir/VERSION" ]; then
      head -n 1 "$dir/VERSION"
      return 0
    fi
    dir=$(dirname "$dir")
  done

  return 1
}

function _under_dir() {
  local base="$1"
  local path="${2:-$PWD}"

  [ -n "$base" ] || return 1

  case "$path/" in
    "$base"/*|"$base"/)
      return 0
      ;;
  esac

  return 1
}

function _non_git_context() {
  local version

  version=$(_version_from_file)
  if [ -n "$version" ]; then
    printf '%s' "$version"
    return 0
  fi

  if _under_dir "$RUNDIR"; then
    printf 'rundir:%s' "$(basename "$RUNDIR")"
    return 0
  fi

  if _under_dir "$ASGS_LOCAL_DIR"; then
    printf 'localdir:%s' "$(basename "$ASGS_LOCAL_DIR")"
    return 0
  fi

  printf 'outside'
}

function pc {
  local inner_top outer_top parent
  local inner_branch inner_dirty inner_label
  local outer_branch outer_dirty

  inner_top=$(git rev-parse --show-toplevel 2>/dev/null)

  if [ -n "$inner_top" ]; then
    inner_branch=$(_git_branch "$inner_top") || inner_branch='?'
    inner_dirty=$(_git_dirty "$inner_top")

    parent=$(dirname "$inner_top")
    while [ "$parent" != "/" ]; do
      outer_top=$(git -C "$parent" rev-parse --show-toplevel 2>/dev/null)

      if [ -n "$outer_top" ] && [ "$outer_top" != "$inner_top" ]; then
        case "$inner_top/" in
          "$outer_top"/*)
            outer_branch=$(_git_branch "$outer_top") || outer_branch='?'
            outer_dirty=$(_git_dirty "$outer_top")
            inner_label=$(_git_repo_label "$inner_top")

            printf '%s%s|%s:%s%s' \
              "$outer_branch" "$outer_dirty" \
              "$inner_label" "$inner_branch" "$inner_dirty"
            return
            ;;
        esac
      fi

      parent=$(dirname "$parent")
    done

    printf '%s%s' "$inner_branch" "$inner_dirty"
    return
  fi

  _non_git_context
}

function sd {
  echo -n "($SCRIPTDIR)"
}

function profile {
  echo -n "$_ASGSH_CURRENT_PROFILE"
}

function host {
  echo -n "$HPCENVSHORT"
}

export PS1='[\[${MG}\]ASGS\[${R}\] (\[${B}${CY}\]$(pc)\[${R}\])] \[${B}${YW}\]$(host)\[${R}\]@\[${B}${GR}\]$(profile)\[${R}\]> '
