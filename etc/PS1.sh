function pc {
  git branch > /dev/null 2>&1
  if [ $? != 0 ]; then
    echo -n ${RD}outside repo${R}
    return
  fi
  local branch=$(git branch | grep '*' | awk '{print $2}');
  echo -n ${GR}$branch${R}
}

function sd {
  echo -n \($SCRIPTDIR\)
}

function profile {
  echo -n "${CY}${B}$_ASGSH_CURRENT_PROFILE${R}"
}

export PS1='${B}${MG}A.S.G.S.${R}|$(pc)|${B}${YW}\H${R}:./\W${R}|@$(profile) > '
