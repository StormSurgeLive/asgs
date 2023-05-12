# START ASGS "goto" completion
# This file is in the public domain

_goto_completion() 
{
    local cur prev files 
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case $prev in
      statefile)
        ;;
      *)
        COMPREPLY=( $(compgen -W "adcircworkdir adcircdir installdir lastsubdir rundir scratchdir scriptdir workdir" -- $cur) )
        ;;
    esac
    return 0
} &&
  complete -F _goto_completion goto

# END goto completion
