Clarke
------

*Status: WIP and Experimental*

Clarke tries to work out how much energy your machine is using, perhaps also how much carbon too and then ships that data off to somewhere so you can work out how to minimise it and then offset the residual.

The tool is named after [Edith Clarke](https://en.wikipedia.org/wiki/Edith_Clarke).

Clarke also integrates with Prometheus. Passing `--listen-prometheus=<port>` will start a prometheus server on port `<port>`.

## Energy Information Sources

Calculating energy information for an arbitrary machine is challenging. Clarke currently offers three approximations:

 - Based on information collected from the CPU
 - Based on information collected using IPMI (for bare-metals servers)
 - Based on some user-specified function over time

None are perfect and more than likely they are all going to under-approximate the amount of energy used.

The tool is also primarily focused on calculating emissions. It integrates the [carbon-intensity](https://github.com/geocaml/carbon-intensity) tool to also monitor the carbon intensity of the energy in the country where the computer is located. You can provide two arguments to control this. `--country` takes an ISO3166 alpha-2 code for the country and `--api` is the path to a file containing the [co2-signal](https://www.co2signal.com) API key. Note if you use `GB` as your code, you won't need to provide an API key.

## Typical Usage

The two main parts of `clarke` you can change are, the "meter" (the source of wattage information) and the output (where the serialised JSON is sent). The simplest example is:

```sh
clarke monitor --meter=const:100
```

This will send data to `stdout` with a constant wattage of `100`. You could equally send it over a TCP connection with:

```sh
clarke monitor --meter=const:100 --output=tcp:loopback:8080 &
nc 127.0.0.1 8080
```

### Variorum

For slightly more accurate information you can use the [variorum](https://github.com/patricoferris/ocaml-variorum) backend. You must [setup certain things that are hardware specific](https://variorum.readthedocs.io/en/latest/HWArchitectures.html).

```sh
clarke monitor --meter=variorum
```

### IPMI

The Intelligent Platform Management Interface (IPMI) is another way we can query information about the power usage of a computer, or usually in this case a server. Using `--meter=ipmi` will try to use `ipmi-tool` to query sensors for power consumption statistics.

