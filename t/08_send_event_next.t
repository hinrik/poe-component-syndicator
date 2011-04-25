use strict;
use warnings FATAL => 'all';
use POE;
use Test::More tests => 6;

{
    package MyComponent;

    use strict;
    use warnings FATAL => 'all';
    use Object::Pluggable::Constants 'PLUGIN_EAT_NONE';
    use POE;
    use Test::More;
    use base 'POE::Component::Syndicator';

    sub spawn {
        my ($package, %args) = @_;
        my $self = bless \%args, $package;
        $self->_syndicator_init(
            debug         => 1,
            prefix        => 'my_',
            object_states => [
                $self => [qw(syndicator_started shutdown)],
            ],
        );
        return $self;
    }

    sub syndicator_started {
        my ($kernel, $self) = @_[KERNEL, OBJECT];
        pass('Subclass got syndicator_started event');
    }

    sub U_foo {
        my ($self) = $_[OBJECT];
        pass('Subclass got user event foo');
        $self->send_event('my_bar');
        $self->send_event_next('my_baz');
        return PLUGIN_EAT_NONE;
    }

    sub shutdown {
        my ($self) = $_[OBJECT];
        $self->_syndicator_destroy();
    }
}

my $synd = MyComponent->spawn();

POE::Session->create(
    package_states => [
        main => [qw(
            _start
            my_bar
            my_baz
            my_shutdown
            _shutdown
        )],
    ],
);

$poe_kernel->run();

sub _start {
    $poe_kernel->delay('_shutdown', 60, 'Timed out');
    $synd->yield('register', qw(bar baz shutdown));
    $synd->yield('foo');
}

sub my_bar {
    my ($kernel, $sender, $heap) = @_[KERNEL, SENDER, HEAP];
    $heap->{got_bar} = 1;
    pass('Interested session got server event my_bar');
    $kernel->post($sender, 'shutdown');
}

sub my_baz {
    my ($kernel, $sender, $heap) = @_[KERNEL, SENDER, HEAP];
    is($heap->{got_bar}, undef, 'Interested session got server event my_baz before my_baz');
    $kernel->post($sender, 'shutdown');
}

sub my_shutdown {
    pass('Interested session got server event my_shutdown');
    $poe_kernel->yield('_shutdown');
}

sub _shutdown {
    my ($kernel, $error) = @_[KERNEL, ARG0];
    fail($error) if defined $error;

    $kernel->alarm_remove_all();
    $kernel->signal($kernel, 'SYNDICATOR_SHUTDOWN');
}
