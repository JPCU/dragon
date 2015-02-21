# Dragon
This fork of eradius is a radical deviation from the original
Jungerl code. 

## Project Status
The current release is v0.6-beta.1
This is a beta release. Probably works but dont use it for protecting
the castle. v0.6.1 is expected March 1, 2015 and will be production ready.


## MFA For All!
Dragon supports Yubikey usage by appending the OTP at the end of the user password.


## Backed by Snarl
Snarl is a right management server build on top of riak_core. More info here:
https://github.com/project-fifo/snarl

## 100% Uptime
v0.7 will have functionality for 100% uptime including live configuration reloads,
and zero downtime upgrades.

## Likely to move.
This project will likely be merged into the Project-Fifo organization.
A note will be posted here when/if that happens.


## Installing on SmartOS

```
wget https://github.com/JPCU/dragon/releases/download/v0.6-beta.1/dragon.tar.gz
pkg_add -u dragon.tar.gz
```

Note: Requires Erlang R16B02 or better.
