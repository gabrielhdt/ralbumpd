#!/usr/bin/perl
# vim: shiftwidth=4:tabstop=4:expandtab

=head1 NAME

ralbum.pl

=head1 DESCRIPTION

MPD interaction based on albums instead of songs. Allows the user to add a
random album to the playlist from the collection or go to next album in the
playlist.

=head1 USAGE

ralbum.pl -r=i|-n|-p|-f

=head1 REQUIRED ARGUMENTS

One of -r, -n, -p or -f

=head1 DEPENDENCIES

Audio::MPD, Getopt::Long::Descriptive

=head1 EXIT STATUS

0 if no error

=head1 AUTHOR

Gabriel Hondet

=head1 LICENSE AND COPYRIGHT

Copyright (C) 2018  Gabriel Hondet

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

=cut

use strict;
use warnings;
use Audio::MPD;
use Getopt::Long::Descriptive;

my ( $opt, $usage ) = describe_options(
    "%c %o",
    [
        "mode" => hidden => {
            one_of => [
                [ "rand|r:i",    "add n random albums to playlist" ],
                [ "nextalbum|n", "play next album in playlist" ],
                [ "prevalbum|p", "play previous album in playlist" ],
                [ "refill|f",    "refill playlist to avoid album shortage" ],
            ]
        }
    ],
    [ "help|h", "print usage message and exit", { shortcircuit => 1 } ],
);

my $mpd = Audio::MPD->new;

# Adds a random album to the playlist
sub add_ralbum {
    my $coll      = $mpd->collection;
    my @album_lst = $coll->all_albums;
    my $ind       = int( rand($#album_lst) );
    my $album     = $album_lst[$ind];
    my @songs     = $coll->songs_from_album($album);
    foreach my $song (@songs) {
        $mpd->playlist->add( $song->file );
    }
    return 0;
}

# Skip all songs in the same album than the current song in the playlist
sub skip_album {
    my $current_album = $mpd->current->album;
    my $new_album = $current_album;    # new_album will change to next album
    while ( $new_album eq $current_album && $mpd->status->state ne "stop" ) {

        # as long as the album has not changed, go to next song
        if ( $_[0] eq "next" ) {
            $mpd->next;
        }
        elsif ( $_[0] eq "prev" ) {
            $mpd->prev;
        }
        $new_album = $mpd->current->album;
    }
    if ( $mpd->status->state eq "stop" ) {    # If playlist exhausted
        add_ralbum;
    }
    return 0;
}

# Avoids going back to the beginning of the playlist if it has been played
# entirely
if ( $mpd->status->state eq "stop" ) {
    $mpd->playlist->clear;
}

if ( $opt->mode eq "rand" ) {
    for ( my $i = 0 ; $i < ( $opt->rand > 0 ? $opt->rand : 1 ) ; $i++ ) {
        add_ralbum;
    }
    $mpd->play;
}
elsif ( $opt->mode eq "nextalbum" ) {
    skip_album("next");
    $mpd->play;
}
elsif ( $opt->mode eq "prevalbum" ) {
    skip_album("prev");
    $mpd->play;
}
elsif ( $opt->mode eq "refill" ) {
    # If stopped do not play
    if ( $mpd->status->state eq "stop" ) {
        exit 0;
    }
    my $csid        = $mpd->current->id;
    my @pl_songs    = $mpd->playlist->as_items;
    my $encountered = 0;
    my $remaining   = 0;
    foreach my $song (@pl_songs) {
        if ($encountered) {
            $remaining++;
        }
        else {
            $encountered = $csid eq $song->id;
        }
    }
    if ( $remaining <= 2 ) {
        add_ralbum;
    }
}
else {
    print($usage);
}
exit 0;
