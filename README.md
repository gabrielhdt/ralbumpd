# Ralbum

Listen to albums rather than songs. This script allows one to manipulate mpd
considering the album as the standard element rather than the song. One can for
instance go to next album in the queue, add a random album, etc.

## Perl
### Requirements
* mpd
* libaudio-mpd-perl
* libgetopt-long-descriptive-perl

## Scheme
Many features are not yet available on the scheme version!
### Requirements
* chicken scheme compiler
* mpd-client egg

## Periodic refresh
Add to user crontab
``` */20 * * * * perl path/to/ralbum.pl --refill ```
