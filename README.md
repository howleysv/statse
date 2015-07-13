# statse [![TravisCI](https://travis-ci.org/howleysv/statse.svg?branch=master)](https://travis-ci.org/howleysv/statse)
Erlang [StatsD](https://github.com/etsy/statsd) client

## Usage:

Add **statse** to your .app file:

```erlang
{ applications, [ statse ] }
```

default parameters can be overridden via your sys.config:

```erlang
{   statse,
	[
		{ statsd_host, "localhost" },
		{ statsd_port,	8125 },
		{ stat_prefix, "node.local" }
	]
}
```

[Lager](https://github.com/basho/lager) backend can be added via your lager config:

```erlang
{ lager, [ { handlers, [ { statse_lager_backend, debug } ] } ] }
```

## Features:

In addition to the standard StatsD metrics, **statse** provides the following features:

#### Stat prefix

`stat_prefix` config parameter can be used to add a node-level prefix to every stat name sent to StatsD.

#### Gauge monitor

**statse** can be configured to periodically call a fun and log the result as a gauge value.

```erlang
statse_gauge_monitor:start_monitor( StatKey, StatFun, RefreshPeriod )
```

where StatFun is a zero arity fun that returns a `number()` or `{ ok, number() }`

Examples:

Start monitoring:
```erlang
statse_gauge_monitor:start_monitor( "vm.memory", fun() -> erlang:memory( total ) end, timer:minutes( 5 ) )
```

Stop monitoring:
```erlang
statse_gauge_monitor:stop_monitor( "vm.memory" )
```

Calling start_monitor with a duplicate key will cause the old monitor to be discarded.
A StatFun that throws an exception will cause the monitor for that stat to be discarded.

## Metrics:

##### Increment counter
```erlang
statse:increment( StatKey )
statse:increment( StatKey, SampleRate )
```

##### Decrement counter
```erlang
statse:decrement( StatKey )
statse:decrement( StatKey, SampleRate )
```

##### Add value to counter
```erlang
statse:count( StatKey, Value )
statse:count( StatKey, Value, SampleRate )
```

##### Timing
```erlang
statse:timing( StatKey, Milliseconds )
statse:timing( StatKey, Milliseconds, SampleRate )
```
or
```erlang
StartTime = os:timestamp(),
%% do_work...
statse:timing( StatKey, StartTime )
statse:timing( StatKey, StartTime, SampleRate )
```

##### Set gauge to value
```erlang
statse:gauge( StatKey, Value )
```

##### Modify gauge by delta
```erlang
statse:gauge_change( StatKey, Delta )
```

##### Count the number of unique values passed to a key
```erlang
statse:set( StatKey, Value )
```


## Tests:

```
make test
```

## Dialyzer

```
make dialyzer
```
