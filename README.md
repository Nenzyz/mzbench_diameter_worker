# MzBench Diameter Worker #

This is diameter worker for [MzBench](https://github.com/satori-com/mzbench)

For now it is suitable for simple request/answer scenarios. 
`SCTP` transport is not tested yet.

## Configuration

At first you need diameter dictionaries for your diameter applications
in [OTP Diameter Dictionary Format](http://erlang.org/doc/man/diameter_dict.html).

Place them in [dia](dia) subdirectory.

Worker accepts the following parameters:

  * *dictionary*, diameter dictionary module name
  * *module*, callback module, defaults to `diameter_worker_cb`, see [OTP Diameter](http://erlang.org/doc/man/diameter.html) for details. It implements `diameter_app` behavior 
  * *origin_host*, defaults to `inet:gethostname()`
  * *origin_realm*, defaults to `"mzbench-client.com"` 
  * *product_name*, defaults to `"MzBench"`
  * *auth_application_id*, comma separated list of integers, defaults to empty string
  * *acct_application_id*, comma separated list of integers, defaults to empty string
  * *service_name*, distinct one diameter service from another, must be unique per client, defaults to `diameter_worker`
  * *address*, server address, defaults to `"127.0.0.1"`
  * *port*, server port, defaults to `3868`
  * *avp_dictionaries*, comma separated list of additional diameter module dictionaries, defaults to empty string
  * *transport*, `"tcp"` or `"sctp"`, defaults to `"tcp"`

## Message Representation

Messages are represented as Erlang maps. You can use JSON format in
[resource files](https://satori-com.github.io/mzbench/scenarios/spec/#resource-files)
alternatively.

Lists of messages are represented as lists of maps:

``` erlang
[#{message_name => string(), message => map()]
```

# Usage

```
connect(address = "127.0.0.1", port=3868, auth_application_id = "4",
            dictionary="diameter_3gpp_ro", avp_dictionaries="diameter_ericsson")
call(MessageName, MessageBody),
call(MessageList)
disconnect()
```

Look in [Examples](./examples) subdicrectory for further inspiration.
