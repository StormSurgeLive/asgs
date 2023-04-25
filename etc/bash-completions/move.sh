# START ASGS "move" completion
# This file is in the public domain

_move_completion() 
{
    local cur prev files 
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case $prev in
      statefile)
        ;;
      *)
        COMPREPLY=( $(compgen -W "statefile" -- $cur) )
        ;;
    esac
    return 0
} &&
  complete -F _move_completion move

# END move completion
