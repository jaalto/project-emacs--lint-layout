#! /bin/sh
#
#  Copyright
#
#       Copyright (C) 2020-2025 Jari Aalto
#
#   License
#
#       This program is free software; you can redistribute it and/or
#       modify it under the terms of the GNU General Public License as
#       published by the Free Software Foundation; either version 2 of
#       the License, or (at your option) any later version.
#
#       This program is distributed in the hope that it will be useful, but
#       WITHOUT ANY WARRANTY; without even the implied warranty of
#       MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
#       General Public License for more details.
#
#       You should have received a copy of the GNU General Public License
#       along with this program. If not, see <http://www.gnu.org/licenses/>.

PROGRAM=$0

unset TEST

Help ()
{
    echo "\
Synopsis: $PROGRAM {--install|--remove} [--home|--local|--dest DIR]

OPTIONS
    -d, --dest DIR
        Select DIR as destination

    -H, --home
        Select ~/bin

    -i, --install
        Install

    -l, --local
        Select /usr/local/bin

    -r, --remove
        Remove install

    -t, --test
        Run in test mode.

    -v, --verbose
        Display verbose messages.

    -h, -help
        Display help.

DESCRIPTION

    Install or remove lint.sh as a symbolic link.
    In addition, make or remove symbolic link to 'javalint'.

EXAMPLES

    $PROGRAM --install --home  # ~/bin
    $PROGRAM --install --local # /usr/local/bin
    $PROGRAM --install --dest ~/other/path"

    exit 0
}

Warn ()
{
    echo "$*" >&2
}

Die ()
{
    Warn "$*"
    exit 1
}

Verbose ()
{
    [ "$VERBOSE" ] || return 0
    echo "$*"
}

Install ()  # Run in a subshell
{(
    ${TEST:+echo} ln ${VERBOSE:+--verbose} --force --symbolic --relative lint.sh "$1"

    Verbose "cd $1"

    ${TEST:+echo} cd "$1" &&
    ${TEST:+echo} ln ${VERBOSE:+--verbose} --symbolic lint.sh javalint
)}

Remove ()
{
    ${TEST:+echo} rm ${VERBOSE:+--verbose} --force "$1/lint.sh" "$1/javalint"
}

Main ()
{
    local dummy dest remove install

    while :
    do
        # unused
        # shellcheck disable=SC2034
        dummy="OPT: $1"

        case "$1" in
            -d | --dest | --dir)
                dest=$2
                [ "$dest" ] || Die "ERROR: Missing --dest DIR"
                [ -d "$dest" ] || Die "ERROR: no dir: $dest"
                shift ; shift
                ;;
            -H | --home)
                dest=$HOME/bin
                shift
                ;;
            -i | --install)
                install="install"
                shift
                ;;
            -l | --local)
                dest=/usr/local/bin
                shift
                ;;
            -r | --rm | --remove | --delete)
                remove="remove"
                shift
                ;;
            -t | --test)
                TEST="test"
                shift
                ;;
            -v | --verbose)
                VERBOSE="verbose"
                shift
                ;;
            -h | --help)
                Help
                ;;
            --)
                shift
                break
                ;;
            -*)
                Warn "WARN: unknown option: $1"
                shift
                ;;
            *)
                break
                ;;
        esac
    done

    if [ ! "$install" ] && [ ! "$remove" ] ; then
        echo "ERROR: Missing option --install or --remove. See --help"
        return 1
    fi

    if [ ! "$dest" ]; then
        Warn "ERROR: Missing option --home, --local or --dest DIR"
        return 1
    fi

    if [ ! -d "$dest" ]; then
        Die "ERROR: Not a directory: $dest"
    fi

    [ "$install" ] && Install "$dest"
    [ "$remove"  ] && Remove  "$dest"
}

Main "$@"

# End of file
