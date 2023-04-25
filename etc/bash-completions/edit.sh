# START ASGS "edit" completion
# This file is in the public domain

_edit_completion() 
{
    local cur prev files 
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case $prev in
      adcirc|config|profile|statefile|syslog)
	;;
      *)
        COMPREPLY=( $(compgen -W "adcirc config profile statefile syslog" -- $cur) )
        ;;
    esac
    return 0
} &&
  complete -F _edit_completion edit

# END edit completion
