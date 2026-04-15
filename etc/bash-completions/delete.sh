# START ASGS "delete" completion
# This file is in the public domain

_delete_completion()
{
    local cur prev
    COMPREPLY=()

    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case "$prev" in
        adcirc|config|profile|statefile)
            # no further completion after subcommand
            return 0
            ;;
        *)
            COMPREPLY=( $(compgen -W "adcirc config profile statefile" -- "$cur") )
            ;;
    esac

    return 0
}

complete -o default -F _delete_completion delete

# END delete completion
