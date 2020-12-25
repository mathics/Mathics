Installing & Running
====================

Requirements
------------

Mathics runs on Python 3.6 or later. We also support PyPy 3.6 or later.
Underneath Mathics relies on
`sympy <https://www.sympy.org/en/index.html>`__ which relies on
`numpy <https://numpy.org>`__. These and the other requirements will be
installed automatically if you use the standard Python installer
```pip`` <https://pip.pypa.io/en/stable/>`__. They are also listed in
`setup.py <https://github.com/mathics/Mathics/blob/master/setup.py>`__.

Installing
----------

Although getting the base Mathics packages is pretty simple, to get the
full installation with mathics script and the various add-on packages.
The docker method right now is the most complete.

Install from PyPI
~~~~~~~~~~~~~~~~~

::

       $ pip install Mathics3

Note the name is "Mathics3" for the most recent release. "Mathics" has
pre-Python 3 code.

Install from Conda-Forge
~~~~~~~~~~~~~~~~~~~~~~~~

See https://github.com/conda-forge/mathics3-feedstock

From docker (dockerhub)
~~~~~~~~~~~~~~~~~~~~~~~

As an alternative to building from source or installing a Python
package, you can run pre-built code via
`docker <https://www.docker.com>`__. To download a copy of the docker
image run:

::

    $ docker pull mathicsorg/mathics

However you this step is also done implicitly if you just issue the
command to run the code. That invocation is given in the next section.

From an OS-specific Repository
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Click on the link below from `Repology.org <https://repology.org>`__ for
details for a specific OS and distribution:

|Packaging status|

Install from git from github
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Several additional dependencies over what is described above if you want
to:

-  build the documentation (which needs ``xetex``, etc.)
-  do full testing (which needs pytest, etc.)
-  run the command-line interface
-  run the Django 3.1 webserver (which needs SQLite, etc.) \`

Below we give command-line instructions. There is also GitHub's git
client for your operating system (`Mac <http://mac.github.com/>`__;
`Windows <http://windows.github.com/>`__). For that, clone
mathics/Mathics (there is a button at the top of
https://github.com/mathics/Mathics that says "Clone in Mac" or "Clone in
Windows" depending on your platform).

.. code:: bash

    $ git clone https://github.com/mathics/Mathics.git
    $ cd Mathics
    $ make install

Alternatively use ``make develop`` or ``pip install -e`` to run the code
installed from where the source-code is checked out. In doing this, code
changes in the source tree are reflected immediately when you rerun.

Of course, you may not want this, but instead want to run from a copy of
the last stable code, so that's what ``make install`` does.

OS-dependent packages
~~~~~~~~~~~~~~~~~~~~~

For the installation above you may need OS-specific packages.

For Debian/Ubuntu based systems:

::

    $ apt-get install python-dev libsqlite3-devp python-setuptools

as super-user, i.e. either after having issued ``su`` or by preceding
the command with ``sudo``).

On Mac OS X

::

    $ brew install sqlite3

On FreeBSD:

::

    $ sudo pkg install math/py-mathics

Running Mathics
---------------

Running natively
~~~~~~~~~~~~~~~~

Command-line interface
^^^^^^^^^^^^^^^^^^^^^^

After installing, you can run your a local Mathics command-line
interface with:

.. code:: bash

    $ mathics

If you are running code from the source tree, i.e. "develop" mode, the
the above is the same thing as running:

.. code:: bash

    $ python mathic/main.py

To get a list of options run:

::

    $ mathics --help

However note that the above CLI is pretty minimal and is likley to stay
that way.

For a more full-featured CLI see
`mathicsscript <https://github.com/Mathics3/mathicsscript>`__.

In the future, ``mathicsscript`` this may support graphics output.

Django-based-line interface
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Chances are you that instead of running a command-line interface you
will want to run the fancier Django-based web server. This does support
graphics output currently (although in the future we plan to improve
that).

To start the server after Mathics has been installed, run:

.. code:: bash

    $ mathicsserver

The first time this command is run it will create the database file for
saving your sessions. Issue

::

    $ mathicsserver --help

to see a list of options.

You can set the used port by using the option ``-p``, as in:

::

    $ mathicsserver -p 8010

The default port for Mathics is 8000. Make sure you have the necessary
privileges to start an application that listens to this port. Otherwise,
you will have to run Mathics as super-user.

By default, the Web server is only reachable from your local machine. To
be able to access it from another computer, use the option ``-e``.

However, the server is only intended for local use, as it is a security
risk to run it openly on a public Web server!

If you are running from the source tree and instead want run the
webserver in a mode where if you make changes to the code, the webserver
will get restarted, we have a GNU Makefile target for doing that.

Here, run:

.. code:: bash

    $ make runserver

This is the same thing as running
``python mathics/manage.py runserver``.

Passing options such as setting the port to listen on is a little
different here because the option has to be a ``manage.py`` option and
that works different.py. And to complicate things further you are
running GNU Make and then have to pass tell that to pass over the
``manage.py`` option. So use pass ``o=8001`` to make in order to pass
option ``8001`` to ``manage.py``. In other words, to set the port here
to 8001:

.. code:: bash

    $ make runserver o=8001 # note, no dash

Running via docker
~~~~~~~~~~~~~~~~~~

Another way to run ``mathics`` is via
`docker <https://www.docker.com>`__ using the \`Mathics docker image
https://hub.docker.com/repository/docker/mathicsorg/mathics on
dockerhub.

To run the command-line interface using docker image:

::

    $ docker run --rm -it --name mathics-cli -v /tmp:/usr/src/app/data mathicsorg/mathics --mode cli

If you want to add options add them at then end preceded with ``--``:
for example:

::

    $ docker run --rm -it --name mathics-cli -v /tmp:/usr/src/app/data mathicsorg/mathics --mode cli -- --help

In the above you are running ``mathicsscript`` (the enhanced CLI), not
``mathics``.

To run the Django 3.1-web interface using docker image run:

::

    $ docker run --rm -it --name mathics-web -p 8000:8000 -v /tmp:/usr/src/app/data mathicsorg/mathics --mode ui

Consult the `docker-run
command <https://docs.docker.com/engine/reference/run/>`__ for
information about changing external port number and other for other
``docker run`` options.

Also see the previous section on security limitations.

This dockerization was modified from
```sealemar/mathics-dockerized`` <https://github.com/sealemar/mathics-dockerized>`__.
See that for more details on how this works.

.. |Packaging status| image:: https://repology.org/badge/vertical-allrepos/mathics.svg
   :target: https://repology.org/project/mathics/versions
