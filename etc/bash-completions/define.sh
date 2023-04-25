# START ASGS "define" completion
# This file is in the public domain

_define_completion() 
{
    local cur prev files 
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    
    case $prev in
      config)
        COMPREPLY=( $(compgen -d -- $cur) )
        COMPREPLY=( $(compgen -f -- $cur) )
        compopt -o filenames 
        ;;
      editor)
        COMPREPLY=( $(compgen -W "vim nano vi" -- $cur) )
	compopt -o nospace 
	;;
      adcircdir|adcircbranch|adcircremote|scriptdir|scratchdir|workdir)
	;;
      *)
        COMPREPLY=( $(compgen -W "adcircdir adcircbranch adcircremote config editor scriptdir scratchdir workdir" -- $cur) )
	;;
    esac
    return 0
} &&
  complete -F _define_completion define

# END define completion
