# START ASGS "delete" completion
# This file is in the public domain

_delete_completion() 
{
    local cur prev files 
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case $prev in
      adcirc|config|profile|statefile)
        ;;
      *)
        COMPREPLY=( $(compgen -W "adcirc config profile statefile" -- $cur) )
        ;;
    esac
    return 0
} &&
  complete -F _delete_completion delete

# END delete completion
