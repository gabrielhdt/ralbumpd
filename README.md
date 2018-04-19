# Ralbum

Listen to albums rather than songs. This script allows one to manipulate mpd
considering the album as the standard element rather than the song. One can for
instance go to next album in the queue, add a random album, etc.

## Scheme
### Requirements
* chicken scheme compiler (`# apt install csc` on debian)
* mpd-client egg (`# chicken-install mpd-client`)
* getopt-long egg (`# chicken-install getopt-long`)

### Building
Just type `make` at the root of the git repo.

## Perl
### Requirements
* libaudio-mpd-perl
* libgetopt-long-descriptive-perl

## Periodic refresh
Add to user crontab
``` */10 * * * * path/to/ralbum --refill ```
