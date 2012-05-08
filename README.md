# About

Gauche-redis is a simple interface module to talk with Redis(http://redis.io/).
It provides native(low layer) access and DBM interface.  You can get this module
from GitHub repos.

	% git clone git://github.com/bizenn/Gauche-redis.git

# Getting Started

Currently we follow the tacit manner of Gauche's extension modules, so
need autoconf and make to install this small module.

	% ./DIST gen
    % ./configure
    % make -s check
    % make install

You can find how to use Gauche-redis API in tests.

_WARNING_

Gauche-redis is in alpha status, so its API might change.
