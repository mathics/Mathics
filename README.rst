Welcome to Mathics!
===================

|Travis|_ |SlackStatus|_ |PyPI|_

Mathics is a general-purpose computer algebra system (CAS). It is an open-source alternative to Mathematica. It is free both as in "free beer" and as in "freedom".

The home page of Mathics is https://mathics.org.

Installing
----------

To install so that you run from the source tree:


::

    $ make develop


More detailed information is available on the Mathics Wiki `here <https://github.com/mathics/Mathics/wiki/Installing>`_.

Running
-------

Once Mathics is installed you can run the Django-based web front-end like this:

::

    $ mathicsserver


For a crude GNU-Readline command-line interface that doesn't provide graphics run

::

  $ mathics


To get more information on both commands add `--help` at the end of the command.

For an experiemental Jupyter-based consoles and web interfaces see `iwolfram <https://github.com/mmatera/iwolfram>`_. (We this will drop the "experimental" by the next release.)

Docker
------

Another way to run ``mathics`` is via `docker <https://www.docker.com/>`_ and the `Mathics docker image <https://hub.docker.com/repository/docker/mathicsorg/mathics>`_ on dockerhub.

To run the command-line interface using docker image:
::
   $ docker run --rm -it --name mathics-cli -v /tmp:/usr/src/app/data mathicsorg/mathics --mode cli

If you want to add options add them at then end preceded with `--`: for example:
::
   $ docker run --rm -it --name mathics-cli -v /tmp:/usr/src/app/data mathicsorg/mathics --mode cli -- --help

To run the Django web interface using docker image:
::
   $ docker run --rm -it --name mathics-web -p 8000:8000 -v /tmp:/usr/src/app/data mathicsorg/mathics --mode ui


This dockerization was modified from `sealemar/mathics-dockerized <https://github.com/sealemar/mathics-dockerized>`_. See that for more details on how this works.

Contributing
------------

Please feel encouraged to contribute to Mathics! Create your own fork, make the desired changes, commit, and make a pull request.

.. |SlackStatus| image:: https://mathics-slackin.herokuapp.com/badge.svg
.. _SlackStatus: https://mathics-slackin.herokuapp.com/
.. |Travis| image:: https://secure.travis-ci.org/mathics/Mathics.svg?branch=master
.. _Travis: https://travis-ci.org/mathics/Mathics
.. |PyPI| image:: https://img.shields.io/pypi/v/Mathics
.. _PyPI: https://pypi.org/project/Mathics/

License
-------

Mathics is released under the GNU General Public License Version 3 (GPL3).
