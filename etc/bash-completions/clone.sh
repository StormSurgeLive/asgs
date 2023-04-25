# START ASGS "clone" completion
# This file is in the public domain

_clone_completion() 
{
    local cur prev files 
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case $prev in
      profile)
        COMPREPLY=""
        ;;
      *)
        COMPREPLY=( $(compgen -W "profile " -- $cur) )
        ;;
    esac
    return 0
} &&
  complete -F _clone_completion clone

# END clone completion
