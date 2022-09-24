Clarke
------

Clarke tries to work out how much energy your machine is using, perhaps also how much carbon too and then ships that data off to somewhere so you can work out how to minimise it and then offset the residule.

## Energy Information Sources

Calculating energy information for an arbitrary machine is challenging. Clarke currently offers three approximations:

 - Based on information collected from the CPU
 - Based on information collected using IPMI (for bare-metals servers)
 - Based on some user-specified function over time

None are perfect and more than likely they are all going to under-approximate the amount of energy used.