function pc {
  git branch > /dev/null 2>&1
  if [ $? != 0 ]; then
    echo -n outside repo
    return
  fi
  local branch=$(git branch | grep '*' | awk '{print $2}');
  echo -n $branch
}

function sd {
  echo -n \($SCRIPTDIR\)
}

function profile {
  echo -n "$_ASGSH_CURRENT_PROFILE"
}

function host {
  echo -n "$HPCENVSHORT"
}

export PS1='[\[${MG}\]ASGS\[${R}\] (\[${B}${CY}\]$(pc)\[${R}\])] \[${B}${YW}\]$(host)\[${R}\]\[${R}\]@\[${B}${GR}\]$(profile)\[${R}\]> '
