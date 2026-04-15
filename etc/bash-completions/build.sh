# START ASGS "build" completion
# This file is in the public domain

_build_completion()
{
    local cur prev
    COMPREPLY=()

    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case "$prev" in
        adcirc|jq|pdl|perl-dev|replaycli)
            # no further completion after subcommand
            return 0
            ;;
        *)
            COMPREPLY=( $(compgen -W "adcirc jq pdl perl-dev replaycli" -- "$cur") )
            ;;
    esac

    return 0
}

complete -o default -F _build_completion build

# END build completion
