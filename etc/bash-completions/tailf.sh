# START ASGS "tailf" completion
# This file is in the public domain

_tailf_completion() 
{
    local cur prev files 
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case $prev in
      syslog)
	;;
      *)
        COMPREPLY=( $(compgen -W "syslog" -- $cur) )
        ;;
    esac
    return 0
} &&
  complete -F _tailf_completion tailf

# END tailf completion
