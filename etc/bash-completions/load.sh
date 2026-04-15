# START ASGS "load" completion
# This file is in the public domain

_load_completion()
{
    local cur prev
    COMPREPLY=()

    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case "$prev" in
        adcirc|profile)
            # no further completion after subcommand
            return 0
            ;;
        *)
            COMPREPLY=( $(compgen -W "adcirc profile" -- "$cur") )
            ;;
    esac

    return 0
}

complete -o default -F _load_completion load

# END load completion
