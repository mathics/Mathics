Welcome to Mathics!
===================

|Travis|_ |SlackStatus|_

Mathics is a general-purpose computer algebra system (CAS). It is an open-source alternative to Mathematica. It is free both as in "free beer" and as in "freedom".

The home page of Mathics is http://mathics.org.

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

Alternatively to the installation step above, ``mathics`` can be installed and run via `docker <https://www.docker.com/>`_. Please refer to `sealemar/mathics-dockerized <https://github.com/sealemar/mathics-dockerized>`_ repo for instructions.

Contributing
------------

Please feel encouraged to contribute to Mathics! Create your own fork, make the desired changes, commit, and make a pull request.

.. |SlackStatus| image:: https://mathics-slackin.herokuapp.com/badge.svg
.. _SlackStatus: https://mathics-slackin.herokuapp.com/
.. |Travis| image:: https://secure.travis-ci.org/mathics/Mathics.svg?branch=master
.. _Travis: https://travis-ci.org/mathics/Mathics

License
-------

Mathics is released under the GNU General Public License Version 3 (GPL3).
