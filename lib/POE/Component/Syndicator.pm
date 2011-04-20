package POE::Component::Syndicator;

use strict;
use warnings;
use Carp 'croak';
use Object::Pluggable::Constants qw(:ALL);
use POE;
use base 'Object::Pluggable';

1;

=encoding utf8

=head1 NAME

POE::Component::Syndicator - A POE component which implements the Observer pattern

=head1 SYNOPSIS

 package MyComponent;

 use strict;
 use warnings;
 use base 'POE::Component::Syndicator';

=head1 DESCRIPTION

This module originated in L<POE::Component::IRC|POE::Component::IRC>, whose
guts quickly spread to other POE components. Now they can all inherit from
this module instead, and the code is all in one place.

=head1 METHODS

=head1 AUTHOR

Hinrik E<Ouml>rn SigurE<eth>sson, hinrik.sig@gmail.com

=head1 LICENSE AND COPYRIGHT

Copyright 2011 Hinrik E<Ouml>rn SigurE<eth>sson

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
