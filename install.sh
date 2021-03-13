#! /bin/sh
#
#  Copyright
#
#       Copyright (C) 2020-2021 Jari Aalto
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
#

Help ()
{
    echo "\
Synopsis: $0 <install option>

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
     Remove install"
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

Install ()
{
    ${test:+echo} ln --force --symbolic --relative lint.sh $1
    (${test:+echo} cd $1 && ${test:+echo} ln -s lint.sh javalint)
}

Remove ()
{
    ${test:+echo} rm --force $1/lint.sh $1/javalint
}

Main ()
{
    while :
    do
        case "$1" in
            --dest)
                [ "$2" ] || Die "ERROR: Missing dir"
                dest=$2
                shift 2
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
            -h | --help)
                Help
                return 0
                ;;
            --)
                shift
                break
                ;;
            *)
                break
                ;;
        esac
    done

    if [ ! "$install" ] && [ ! "$remove" ] ; then
        echo "Install utility. See --help"
        return 1
    fi

    if [ ! "$dest" ]; then
        Warn "ERROR: Missing option --home, --local or --dest DIR"
        return 1
    fi

    if [ ! -d "$dest" ]; then
        Die "ERROR: Not a directory: $dest"
    fi

    [ "$install" ] && Install $dest
    [ "$remove"  ] && Remove  $dest
}

Main "$@"

# End of file
