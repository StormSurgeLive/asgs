# START ASGS "edit" completion
# This file is in the public domain

_edit_completion()
{
    local cur prev
    COMPREPLY=()

    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case "$prev" in
        adcirc|config|profile|statefile|syslog)
            # no further completion after subcommand
            return 0
            ;;
        *)
            COMPREPLY=( $(compgen -W "adcirc config profile statefile syslog" -- "$cur") )
            ;;
    esac

    return 0
}

complete -o default -F _edit_completion edit

# END edit completion
